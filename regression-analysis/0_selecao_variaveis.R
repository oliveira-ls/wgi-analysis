# Imports ---------------------------------------------------------------------

library(data.table); setDTthreads(threads = 0)
library(ggplot2)
library(tseries)
library(car)


# Carregar dados --------------------------------------------------------------

file_path <- "./data/wgidataset_with_sourcedata.xlsx"
db <- readxl::read_excel(file_path)
setDT(db)

# Seleção de colunas
columns_to_select <- c(
  "indicator", "code", "year", "estimate", "stddev", "nsource",
  "pctrank", "pctranklower", "pctrankupper", "eiu", "prs", "wmo",
  "scalemean", "scalesd"
)
db <- db[, ..columns_to_select]
db[ , if (.N > 0) .SD, by = .(indicator, code)]

# Conversão de categóricas
categorical_columns <- c("indicator", "code", "year")
db[, (categorical_columns) := lapply(.SD, as.factor), .SDcols = categorical_columns]

# Conversão de numéricas
numeric_columns <- setdiff(names(db), categorical_columns)
db[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols = numeric_columns] |> 
  suppressWarnings()

# Filtrar ausentes em 'estimate'
db_filtered <- na.omit(db)
setorder(db_filtered, code, indicator, year)
db_filtered

# Adicionar os eventos extremos -----------------------------------------------

# Importar a planilha de eventops extremos
db_ex <- readxl::read_excel(
  path = "data/Eventos_Extremos_CODE_Final.xlsx",
  sheet = "Folha1", range = "A1:F63"
)
setDT(db_ex)

# Selecionar year e code e converter para factor
db_ex <- db_ex[, .(year = as.factor(year), code = as.factor(code), event = 1)]

# Substituir NA por "Global" na coluna code
db_ex[is.na(code), code := "Global"]

# Adicionar a coluna extreme_event inicial com valor 0
db_filtered[, extreme_event := 0]

# Etapa 1: Comparar year e code entre db_ex e db_filtered
db_filtered[db_ex, on = .(year, code), extreme_event := i.event]

# Etapa 2: Caso o code seja "Global", atribuir 1 para o respectivo year
db_filtered[
  db_ex[code == "Global"], 
  on = .(year), 
  extreme_event := extreme_event + 1
]

db_filtered
table(db_filtered$extreme_event)

# Avaliação de estacionariedade -----------------------------------------------

# Definir as variáveis que serão avaliadas para estacionariedade
variaveis_de_interesse <- c(
  "estimate", "stddev", "pctrank", 
  "pctranklower", "pctrankupper", "eiu", "prs", 
  "wmo", "scalemean", "scalesd"
)

# Testes de Estacionariedade por 'indicator' e 'code' com KPSS
# Se não for estacionária, tenta defasagem 1
resultados_testes <- data.table()
for (indicator_value in unique(db_filtered$indicator)) {
  for (code_value in unique(db_filtered$code)) {
    # Filtrar dados para cada combinação de 'indicator' e 'code'
    db_code_filtered <- db_filtered[indicator == indicator_value & code == code_value]
    
    for (col in variaveis_de_interesse) {
      # Verificação antes de criar a série temporal
      if (all(is.na(db_code_filtered[[col]]))) next
      
      # Criar série temporal para o 'indicator' e 'code' atual
      ts_data <- ts(db_code_filtered[[col]], frequency = 1)
      
      # Realiza o teste KPSS considerando a possibilidade de drift
      kpss_test <- kpss.test(ts_data, null = "Trend")
      
      # Conclusão do KPSS
      conclusao_kpss <- ifelse(kpss_test$p.value < 0.05, "Não Estacionária", "Estacionária")
      
      if (is.nan(conclusao_kpss) | is.na(conclusao_kpss)) {
        # Armazenar os resultados
        resultados_testes <- rbind(resultados_testes, data.table(
          indicator = indicator_value,
          code = code_value,
          variavel = col,
          teste_kpss = kpss_test$p.value,
          conclusao_kpss = "Série sem variação",
          defasagem = 0
        ))
        next
      }
      
      if (conclusao_kpss != "Não Estacionária") {
        # Armazenar os resultados
        resultados_testes <- rbind(resultados_testes, data.table(
          indicator = indicator_value,
          code = code_value,
          variavel = col,
          teste_kpss = kpss_test$p.value,
          conclusao_kpss = conclusao_kpss,
          defasagem = 0
        ))
      } else {
        # Criar série temporal defasada (defasagem de 1)
        ts_data_defasada <- diff(ts_data, differences = 1)
        
        # Teste KPSS na série defasada considerando drift
        kpss_test <- kpss.test(ts_data_defasada, null = "Trend")
        
        # Conclusão do KPSS
        conclusao_kpss <- ifelse(kpss_test$p.value < 0.05, "Não Estacionária", "Estacionária")
        
        # Armazenar os resultados
        resultados_testes <- rbind(resultados_testes, data.table(
          indicator = indicator_value,
          code = code_value,
          variavel = col,
          teste_kpss = kpss_test$p.value,
          conclusao_kpss = conclusao_kpss,
          defasagem = 1 # Defasagem aplicada
        ))
      }
    }
  }
} |> suppressWarnings()

# Resultados
table(resultados_testes$conclusao_kpss)
table(resultados_testes$defasagem)
db_filtered


# Trabalhar apenas com as que podem ser estacionárias -------------------------

# Filtrar as combinações estacionárias
combinacoes_estacionarias <- resultados_testes[
  conclusao_kpss == "Estacionária", 
  .(indicator, code, variavel, defasagem)
]
combinacoes_estacionarias

# Aplicar a defasagem usando um loop for
db_filtered_new <- copy(db_filtered)
for (i in seq_len(nrow(combinacoes_estacionarias))) {
  ind <- combinacoes_estacionarias[i, indicator]
  cd <- combinacoes_estacionarias[i, code]
  var <- combinacoes_estacionarias[i, variavel]
  def <- combinacoes_estacionarias[i, defasagem]
  if(def == 1) {
    db_filtered_new[
      indicator == ind & code == cd, (var) := c(NA, diff(get(var), differences = 1)), 
      by = .(indicator, code)
    ]
  }
}

# Remover NAs devido a defasagem
db_filtered_new <- na.omit(db_filtered_new)

# Remover year, pois são estacionárias
db_filtered_new$year <- NULL

# % de aproveitamento em relação ao db original
print(paste0(round(100 * nrow(db_filtered_new) / nrow(db), 2), "%"))


# Avaliação de multicolineariedade --------------------------------------------

# Matriz de correlação
cor(db_filtered_new[, ..numeric_columns])

# Avaliação preliminar
tmp <- db_filtered_new
fit <- lm(estimate ~ ., data = tmp)
vif(fit)
anova(fit)
summary(fit)
par(mfrow = c(2, 2)); plot(fit); par(mfrow = c(1, 1))

# Salvar dados para modelagem final
saveRDS(object = db_filtered_new, file = "./data/tidy_data.RDS")
write.csv2(x = db_filtered_new, file = "./data/tidy_data.csv")
