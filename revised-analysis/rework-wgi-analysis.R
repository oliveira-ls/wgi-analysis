# =============================================================================
# Reestimação: "Nível e perfil da governança nos Worldwide Governance Indicators"
# Ressubmissão Navus (prazo 28/10/2026)
#
# Etapas: 0 configuração | 1 importação | 2 auditoria | 3 transformação
#         4 objetivo 1 (persistência e mudança) | 5 objetivo 2 (Friedman, Q)
#         6 objetivo 3 (Spearman, PCA, mudança de estrutura)
#         7 objetivo 4 (agrupamento por perfil) | 8 sensibilidade
#         9 visualização | 10 exportação
#
# Entrada: wgidataset_with_sourcedata.xlsx (edição 2024 dos WGI, 1996 a 2023),
#          o mesmo arquivo do repositório oliveira-ls/wgi-analysis.
# Regiões: CLASS.xlsx do Banco Mundial (download automático na etapa 1).
# Decisão central: todas as análises usam a estimativa em NÍVEL; não há
# diferenciação, KPSS ou VIF. O ano é preservado.
# =============================================================================

# 0 Configuração --------------------------------------------------------------
pacotes <- c("readxl", "dplyr", "tidyr", "purrr", "ggplot2", "PMCMRplus",
             "cluster", "sf", "rnaturalearth", "rnaturalearthdata", "writexl")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))

caminho_wgi <- "data/wgidataset_with_sourcedata.xlsx"
dir_saida   <- "resultados"
dir.create(dir_saida, showWarnings = FALSE)

DIMS <- c("va", "pv", "ge", "rq", "rl", "cc")
ROTULOS <- c(va = "Voz e Responsabilização", pv = "Estabilidade Política",
             ge = "Efetividade do Governo", rq = "Qualidade Regulatória",
             rl = "Estado de Direito", cc = "Controle da Corrupção")
PERIODO_REF <- 2019:2023
Z90 <- qnorm(0.95)                     # IC de 90%, convenção dos WGI
SEMENTE <- 20261028

# 1 Importação ----------------------------------------------------------------
bruto <- read_excel(caminho_wgi, na = c("", "#N/A", "..")) |>
  mutate(across(c(estimate, stddev, nsource), as.numeric))

url_class <- "https://databankfiles.worldbank.org/public/ddpext_download/site-content/CLASS.xlsx"
arq_class <- "data/CLASS.xlsx"            # guardar a versão baixada junto dos dados, para rastreabilidade
if (!file.exists(arq_class)) download.file(url_class, arq_class, mode = "wb", quiet = TRUE)
classe <- read_excel(arq_class, sheet = 1) |>
  filter(!is.na(Region)) |>
  transmute(iso = Code, region = Region)

# códigos dos WGI que diferem do ISO3 do Banco Mundial
recod <- c(ADO = "AND", ZAR = "COD", KSV = "XKX", ROM = "ROU", TMP = "TLS", WBG = "PSE")
# economias dos WGI ausentes da CLASS.xlsx (atribuição geográfica)
manual <- tibble::tribble(
  ~code, ~region_man,
  "TWN", "East Asia & Pacific",       "AIA", "Latin America & Caribbean",
  "ANT", "Latin America & Caribbean", "GUF", "Latin America & Caribbean",
  "MTQ", "Latin America & Caribbean", "REU", "Sub-Saharan Africa",
  "COK", "East Asia & Pacific",       "NIU", "East Asia & Pacific",
  "JEY", "Europe & Central Asia")

# 2 Auditoria -----------------------------------------------------------------
wgi <- bruto |>
  select(code, countryname, year, indicator, estimate, stddev, nsource) |>
  filter(!is.na(estimate))

auditoria <- list(
  linhas_brutas   = nrow(bruto),
  linhas_estimate = nrow(wgi),
  economias       = n_distinct(wgi$code),
  anos            = sort(unique(wgi$year)),
  # a edição usada normaliza cada indicador-ano para média 0 e DP 1
  normalizacao    = wgi |> group_by(indicator, year) |>
    summarise(media = mean(estimate), dp = sd(estimate), .groups = "drop") |>
    summarise(max_abs_media = max(abs(media)), dp_min = min(dp), dp_max = max(dp))
)
print(auditoria$normalizacao)

regioes <- wgi |> distinct(code, countryname) |>
  mutate(iso = dplyr::recode(code, !!!recod)) |>
  left_join(classe, by = "iso") |>
  left_join(manual, by = "code") |>
  mutate(region = coalesce(region, region_man)) |>
  select(code, countryname, region)
stopifnot(!anyNA(regioes$region))

wgi <- left_join(wgi, select(regioes, code, region), by = "code")

# 3 Transformação: amostra de referência (2019 a 2023) ------------------------
ref <- wgi |> filter(year %in% PERIODO_REF)
completos <- ref |> count(code, indicator) |>
  group_by(code) |>
  filter(n_distinct(indicator) == 6, all(n == length(PERIODO_REF))) |>
  distinct(code) |> pull(code)

X_df <- ref |> filter(code %in% completos) |>
  group_by(code, indicator) |> summarise(e = mean(estimate), s = mean(stddev), .groups = "drop")
X <- X_df |> select(-s) |> pivot_wider(names_from = indicator, values_from = e) |>
  select(code, all_of(DIMS)) |> arrange(code)
S <- X_df |> select(-e) |> pivot_wider(names_from = indicator, values_from = s) |>
  select(code, all_of(DIMS)) |> arrange(code)
Xm <- as.matrix(X[, DIMS]); rownames(Xm) <- X$code
Sm <- as.matrix(S[, DIMS]); rownames(Sm) <- S$code
cat("Economias na amostra de referência:", nrow(Xm), "\n")

# 4 Objetivo 1: persistência e mudança significativa --------------------------
mudanca <- function(a0, a1) {
  inner_join(filter(wgi, year == a0), filter(wgi, year == a1),
             by = c("code", "indicator"), suffix = c("0", "1")) |>
    mutate(d = estimate1 - estimate0,
           sig = abs(d) > Z90 * (stddev0 + stddev1),   # ICs de 90% sem sobreposição
           direcao = case_when(!sig ~ "sem", d > 0 ~ "melhora", TRUE ~ "piora"),
           region = region0)
}
resumo_mudanca <- function(m) m |> group_by(indicator) |>
  summarise(n = n(), rho = cor(estimate0, estimate1, method = "spearman"),
            melhora = sum(direcao == "melhora"), piora = sum(direcao == "piora"),
            sem = sum(direcao == "sem"), .groups = "drop") |>
  arrange(match(indicator, DIMS))
m96 <- mudanca(1996, 2023); m13 <- mudanca(2013, 2023)
tab_obj1_96 <- resumo_mudanca(m96); tab_obj1_13 <- resumo_mudanca(m13)
tab_obj1_reg <- m96 |> count(region, direcao) |>
  pivot_wider(names_from = direcao, values_from = n, values_fill = 0)

# 5 Objetivo 2: diferenças entre dimensões dentro das economias ---------------
# H0: as seis dimensões têm a mesma distribuição de posições relativas dentro das economias
fried <- friedman.test(Xm)
W_kendall <- unname(fried$statistic) / (nrow(Xm) * (ncol(Xm) - 1))
nemenyi <- frdAllPairsNemenyiTest(Xm)
postos_medios <- colMeans(t(apply(Xm, 1, rank)))
# heterogeneidade do perfil de cada economia, ponderada pelo erro-padrão dos WGI
w_ <- 1 / Sm^2
e_bar <- rowSums(Xm * w_) / rowSums(w_)
Q <- rowSums(((Xm - e_bar)^2) * w_)
p_Q <- pchisq(Q, df = ncol(Xm) - 1, lower.tail = FALSE)
tab_obj2 <- tibble(chi2 = unname(fried$statistic), gl = unname(fried$parameter),
                   p = fried$p.value, W = W_kendall,
                   Q_sig_n = sum(p_Q < .05), Q_sig_prop = mean(p_Q < .05))

# 6 Objetivo 3: associação entre dimensões ------------------------------------
rho <- cor(Xm, method = "spearman")
set.seed(SEMENTE)
boot_rho <- replicate(2000, cor(Xm[sample(nrow(Xm), replace = TRUE), ], method = "spearman"))
rho_lo <- apply(boot_rho, 1:2, quantile, 0.025); rho_hi <- apply(boot_rho, 1:2, quantile, 0.975)
autoval <- eigen(cor(Xm))$values; prop_pc1 <- autoval[1] / ncol(Xm)

inicio <- wgi |> filter(year %in% c(1996, 1998, 2000)) |>
  group_by(code, indicator) |> summarise(e = mean(estimate), .groups = "drop") |>
  pivot_wider(names_from = indicator, values_from = e) |> select(code, all_of(DIMS)) |> tidyr::drop_na()
comuns <- intersect(inicio$code, rownames(Xm))
A  <- as.matrix(inicio[match(comuns, inicio$code), DIMS]); Bm <- Xm[comuns, ]
pc1 <- function(M) eigen(cor(M))$values[1] / ncol(M)
set.seed(SEMENTE)
boot_dif <- replicate(2000, {i <- sample(nrow(A), replace = TRUE)
  c(cor(Bm[i, ], method = "spearman") - cor(A[i, ], method = "spearman"), pc1(Bm[i, ]) - pc1(A[i, ]))})
pares <- which(upper.tri(rho), arr.ind = TRUE)
tab_obj3_mud <- tibble(
  a = DIMS[pares[, 1]], b = DIMS[pares[, 2]],
  rho_1996_2000 = cor(A, method = "spearman")[pares],
  rho_2019_2023 = cor(Bm, method = "spearman")[pares]) |>
  mutate(dif = rho_2019_2023 - rho_1996_2000,
         lo = apply(boot_dif[1:36, ], 1, quantile, .025)[(pares[, 2] - 1) * 6 + pares[, 1]],
         hi = apply(boot_dif[1:36, ], 1, quantile, .975)[(pares[, 2] - 1) * 6 + pares[, 1]])
dif_pc1 <- c(estimado = pc1(Bm) - pc1(A), quantile(boot_dif[37, ], c(.025, .975)))

# 7 Objetivo 4: agrupamento por perfil ----------------------------------------
nivel  <- rowMeans(Xm)
perfil <- Xm - nivel                       # desvios de cada dimensão em relação ao nível
prop_nivel <- ncol(Xm) * sum((nivel - mean(Xm))^2) / sum((Xm - mean(Xm))^2)

ward_km <- function(M, k) {                # Ward (D2) seguido de k-means (Lloyd) com centros do Ward
  grupos <- cutree(hclust(dist(M), method = "ward.D2"), k)
  centros <- apply(M, 2, function(col) tapply(col, grupos, mean))
  kmeans(M, centers = centros, algorithm = "Lloyd", iter.max = 300)$cluster
}
validar <- function(M, ks = 2:8) {
  gap <- clusGap(M, FUNcluster = function(x, k) list(cluster = ward_km(x, k)),
                 K.max = max(ks), B = 200, d.power = 2, verbose = FALSE)
  map_dfr(ks, function(k) {
    g <- ward_km(M, k)
    tibble(k = k, silhueta = mean(silhouette(g, dist(M))[, 3]),
           gap = gap$Tab[k, "gap"], se_gap = gap$Tab[k, "SE.sim"])
  }) |> mutate(k_gap = maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax"))
}
set.seed(SEMENTE); val_nivel  <- validar(Xm)
set.seed(SEMENTE); val_perfil <- validar(perfil)

jaccard_boot <- function(M, k, B = 500) {
  base <- ward_km(M, k)
  J <- replicate(B, {
    idx <- unique(sample(nrow(M), replace = TRUE))
    g <- ward_km(M[idx, ], k)
    sapply(sort(unique(base)), function(c0) {
      a <- idx[base[idx] == c0]
      max(sapply(unique(g), function(c1) {b <- idx[g == c1]; length(intersect(a, b)) / length(union(a, b))}))
    })
  })
  rowMeans(J)
}
K_PERFIL <- 3
grupo <- ward_km(perfil, K_PERFIL)
set.seed(SEMENTE); estab <- jaccard_boot(perfil, K_PERFIL)

centroides <- aggregate(perfil, list(grupo = grupo), mean)
regiao_ref <- regioes$region[match(rownames(Xm), regioes$code)]
tab_regiao <- table(grupo, regiao_ref)
qui <- chisq.test(tab_regiao, simulate.p.value = TRUE, B = 10000)  # células com contagem baixa
V_cramer <- sqrt(unname(qui$statistic) / (sum(tab_regiao) * (min(dim(tab_regiao)) - 1)))
kw_nivel <- kruskal.test(nivel ~ factor(grupo))

membros <- tibble(code = rownames(Xm), grupo = grupo, nivel = nivel, region = regiao_ref) |>
  left_join(distinct(regioes, code, countryname), by = "code") |>
  bind_cols(as_tibble(perfil, .name_repair = ~ paste0("desvio_", .x))) |>
  arrange(grupo, desc(nivel))

# 8 Sensibilidade -------------------------------------------------------------
ari <- function(a, b) {                    # índice de Rand ajustado
  tab <- table(a, b); n <- sum(tab); c2 <- function(x) sum(choose(x, 2))
  esp <- c2(rowSums(tab)) * c2(colSums(tab)) / choose(n, 2)
  (c2(tab) - esp) / (0.5 * (c2(rowSums(tab)) + c2(colSums(tab))) - esp)
}
nsrc <- ref |> filter(code %in% rownames(Xm)) |> group_by(code) |> summarise(ns = mean(nsource))
manter <- nsrc$code[nsrc$ns >= 4]
ari_fontes <- ari(grupo[match(manter, rownames(Xm))], ward_km(perfil[manter, ], K_PERFIL))
ant <- wgi |> filter(year %in% 2014:2018) |> group_by(code, indicator) |>
  summarise(e = mean(estimate), .groups = "drop") |>
  pivot_wider(names_from = indicator, values_from = e) |> select(code, all_of(DIMS)) |> tidyr::drop_na()
com2 <- intersect(ant$code, rownames(Xm))
P2 <- as.matrix(ant[match(com2, ant$code), DIMS]); P2 <- P2 - rowMeans(P2)
ari_periodo <- ari(grupo[match(com2, rownames(Xm))], ward_km(P2, K_PERFIL))

# 9 Visualização --------------------------------------------------------------
cores <- c("1" = "#2a78d6", "2" = "#eb6834", "3" = "#1baf7a")
fig1 <- ggplot(m96, aes(estimate0, estimate1)) +
  geom_abline(colour = "grey85") +
  geom_point(data = ~ filter(.x, direcao == "sem"), colour = "grey70", size = 0.8) +
  geom_point(data = ~ filter(.x, direcao != "sem"), aes(colour = direcao, shape = direcao), size = 1.4) +
  scale_colour_manual(values = c(melhora = "#2a78d6", piora = "#eb6834"),
                      labels = c("Melhora significativa", "Piora significativa"), name = NULL) +
  scale_shape_manual(values = c(melhora = 16, piora = 15),
                     labels = c("Melhora significativa", "Piora significativa"), name = NULL) +
  facet_wrap(~ factor(indicator, DIMS, ROTULOS)) + coord_equal() +
  labs(x = "Estimativa em 1996", y = "Estimativa em 2023") +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
ggsave(file.path(dir_saida, "fig1_persistencia.png"), fig1, width = 7.5, height = 5.2, dpi = 300)

cent_long <- membros |> pivot_longer(starts_with("desvio_"), names_to = "dim", values_to = "desvio") |>
  mutate(dim = factor(sub("desvio_", "", dim), DIMS, ROTULOS)) |>
  group_by(grupo, dim) |>
  summarise(m = mean(desvio), q1 = quantile(desvio, .25), q3 = quantile(desvio, .75), .groups = "drop")
fig2 <- ggplot(cent_long, aes(dim, m, colour = factor(grupo), group = grupo)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_linerange(aes(ymin = q1, ymax = q3), position = position_dodge(.3), alpha = .6) +
  geom_line(position = position_dodge(.3), linewidth = .8) +
  geom_point(aes(shape = factor(grupo)), position = position_dodge(.3), size = 2) +
  scale_colour_manual(values = cores, name = "Perfil") + scale_shape_manual(values = c(16, 15, 17), name = "Perfil") +
  labs(x = NULL, y = "Desvio em relação ao nível do país") + theme_minimal(base_size = 9)
ggsave(file.path(dir_saida, "fig2_perfis.png"), fig2, width = 7.2, height = 3.8, dpi = 300)

mundo <- ne_countries(scale = "medium", returnclass = "sf") |> filter(adm0_a3 != "ATA")
mapa <- left_join(mundo, mutate(membros, adm0_a3 = dplyr::recode(code, ZAR = "COD", ROM = "ROU", TMP = "TLS",
                                                                     KSV = "KOS", WBG = "PSX", ADO = "AND")),
                  by = "adm0_a3")
fig3 <- ggplot(mapa) + geom_sf(aes(fill = factor(grupo)), colour = "white", linewidth = .1) +
  scale_fill_manual(values = cores, na.value = "grey90", name = "Perfil") + theme_void(base_size = 9) +
  theme(legend.position = "bottom")
ggsave(file.path(dir_saida, "fig3_mapa_perfis.png"), fig3, width = 7.5, height = 3.9, dpi = 300)

# 10 Exportação ---------------------------------------------------------------
write_xlsx(list(
  obj1_1996_2023 = tab_obj1_96, obj1_2013_2023 = tab_obj1_13, obj1_regioes = tab_obj1_reg,
  obj2_friedman = tab_obj2,
  obj2_nemenyi = as.data.frame(as.table(nemenyi$p.value)) |> tidyr::drop_na(),
  obj2_postos = tibble(dim = DIMS, posto_medio = postos_medios),
  obj3_spearman = as_tibble(rho, rownames = "dim"),
  obj3_ic_inf = as_tibble(rho_lo, rownames = "dim"), obj3_ic_sup = as_tibble(rho_hi, rownames = "dim"),
  obj3_mudanca = tab_obj3_mud,
  obj3_pca = tibble(autovalor = autoval, prop = autoval / ncol(Xm)),
  obj4_valid_nivel = val_nivel, obj4_valid_perfil = val_perfil,
  obj4_centroides = centroides, obj4_membros = membros,
  obj4_regiao = as.data.frame.matrix(tab_regiao) |> tibble::rownames_to_column("grupo"),
  sintese = tibble(
    item = c("economias_ref", "prop_var_nivel", "pc1_2019_2023", "dif_pc1", "dif_pc1_lo", "dif_pc1_hi",
             "V_cramer", "p_qui_regiao", "KW_nivel_H", "KW_nivel_p", "ari_fontes", "ari_periodo",
             paste0("jaccard_", seq_len(K_PERFIL))),
    valor = c(nrow(Xm), prop_nivel, prop_pc1, dif_pc1, V_cramer, qui$p.value,
              unname(kw_nivel$statistic), kw_nivel$p.value, ari_fontes, ari_periodo, estab))
), file.path(dir_saida, "resultados_reestimacao.xlsx"))
writeLines(capture.output(sessionInfo()), file.path(dir_saida, "sessionInfo.txt"))
