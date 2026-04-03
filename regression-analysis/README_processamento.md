# Pré-processamento de Dados com Indicadores de Governança e Eventos Extremos

## Descrição do Projeto

Este projeto realiza uma análise avançada dos indicadores de governança global (WGI) em conjunto com eventos extremos, com foco na avaliação de estacionariedade, correlações e preparação para modelagem. A análise utiliza arquivos **.R** e dados no formato **.xlsx**.

---

## Estrutura do Projeto

Abaixo está a estrutura de arquivos do projeto:

```
└── data/
    ├── wgidataset_with_sourcedata.xlsx   # Dados de governança com fontes associadas
    ├── Eventos_Extremos_CODE_Final.xlsx  # Dados de eventos extremos
    └── tidy_data.RDS                     # Dados finais processados e prontos para modelagem

└── scripts/
    └── 0_selecao_variaveis.R             # Script principal para limpeza e preparação de dados

└── README.md                            # Documentação do projeto
```

---

## Pipeline de Processamento

1. **Carregamento e Pré-processamento de Dados**  
   - **Fonte**: `wgidataset_with_sourcedata.xlsx`  
   - Seleção de colunas relevantes: indicadores, variância e percentil de governança.  
   - Conversão para fatores e numéricos.

2. **Integração de Eventos Extremos**  
   - **Fonte**: `Eventos_Extremos_CODE_Final.xlsx`  
   - Inclusão da variável `extreme_event` baseada na comparação entre códigos de país e ano.

3. **Avaliação de Estacionariedade**  
   - Utilização do teste **KPSS** para avaliar estacionariedade das variáveis.  
   - Aplicação de defasagem (diferenciação de ordem 1) em séries não estacionárias.

4. **Análise de Multicolinearidade**  
   - Matriz de correlação e cálculo do **VIF** para identificar multicolinearidade.

5. **Geração de Dados Processados**  
   - **Saída**: `tidy_data.RDS` contendo dados limpos, transformados e prontos para análise/modelagem.

---

## Tecnologias Utilizadas

- **R**  
   - Bibliotecas principais: `data.table`, `ggplot2`, `tseries`, `car`, `readxl`  
   - Script de automação: `0_selecao_variaveis.R`  
- **Excel**  
   - Dados brutos no formato `.xlsx`.  

---

## Resultados

- Arquivo **`tidy_data.RDS`**: Contém as séries temporais transformadas e estacionárias, com eventos extremos integrados.  
- Identificação de **multicolinearidade** e variáveis-chave para modelagem.

---

## Executando o Projeto

1. Clone o repositório ou organize os arquivos conforme a estrutura fornecida.
2. Certifique-se de ter as bibliotecas necessárias instaladas em R:
   ```R
   install.packages(c("data.table", "ggplot2", "tseries", "car", "readxl"))
   ```
3. Execute o script principal:
   ```R
   source("scripts/0_selecao_variaveis.R")
   ```

---

## Referências

- **Worldwide Governance Indicators**: [www.govindicators.org](https://www.govindicators.org)  
- Eventos extremos: Dados fornecidos no arquivo `Eventos_Extremos_CODE_Final.xlsx`.

---

