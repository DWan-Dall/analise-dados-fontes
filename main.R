
library(tidyverse)
# library(dplyr)
# library(ggplot2)
library(readxl)
library(tidyr)

dados <- read_excel("dados/Atlas_Digital/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado _AREA_INTERESSE.xlsx", sheet = "Área de Interesse")

head(dados)
summary(dados)

