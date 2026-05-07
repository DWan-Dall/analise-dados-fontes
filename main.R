
library(tidyverse)
# library(dplyr)
# library(ggplot2)
library(readxl)
library(tidyr)
library(janitor)
library(lubridate)
library(skimr)

dados <- read_excel(
  "dados/Atlas_Digital/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado _AREA_INTERESSE.xlsx",
  sheet = "Área de Interesse"
) %>%
  clean_names()

names(dados)

# 1 Verificação dos dados e limpeza

head(dados)
skim(dados)

# 2 Ánalise temporal

dados <- dados %>%
  mutate(
    data_evento = as.Date(data_evento),
    ano = year(data_evento),
    mes = month(data_evento)
  )

dados %>%
  count(ano) %>%
  ggplot(aes(x = ano, y = n)) +
  geom_col()


dados %>%
  count(nome_municipio, sort = TRUE) %>%
  slice_head(n = 10)
