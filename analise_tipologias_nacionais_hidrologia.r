# ==========================================================
# ANÁLISE NACIONAL DAS TIPOLOGIAS E POSIÇÃO DA HIDROLOGIA
# Base: BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado.xlsx
# Aba: Área de Interesse
# ==========================================================

library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(forcats)
library(ggplot2)
library(scales)
library(glue)

# ---------------------------
# Configurações
# ---------------------------
arquivo <- "dados/Atlas_Digital/Dados_Originais/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado (2).xlsx"
aba <- "Atlas Valores Corrigidos"
base_output <- "output/analise_nacional_hidrologia"

dir.create(base_output, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_output, "01_tabelas"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_output, "02_graficos"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Tema e funções auxiliares
# ---------------------------
theme_cientometria <- function(base_size = 16, legend = "right"){
  theme_classic(base_size = base_size) +
    theme(
      axis.title = element_text(size = base_size + 2),
      axis.text.x = element_text(angle = 45, hjust = 1, size = base_size - 3),
      axis.text.y = element_text(size = base_size - 3),
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(size = base_size - 1),
      legend.position = legend,
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text = element_text(face = "bold")
    )
}

save_plot <- function(plot, filename, width = 14, height = 8, dpi = 320){
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
}

normalize_ascii <- function(x){
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT") |>
    tolower() |>
    str_squish()
}

# ---------------------------
# Leitura e padronização
# ---------------------------
bd <- read_excel(arquivo, sheet = aba) |> clean_names()

col_grupo <- names(bd)[str_detect(names(bd), "grupo")][1]
col_desc  <- names(bd)[str_detect(names(bd), "descr.*tipologia|atlas.*descricao")][1]
col_tip   <- names(bd)[str_detect(names(bd), "tipologia")][1]
col_uf    <- names(bd)[str_detect(names(bd), "sigla.*uf|_uf$|^uf$")][1]

bd2 <- bd |>
  mutate(
    grupo_de_desastre = .data[[col_grupo]],
    descricao_tipologia = .data[[col_desc]],
    tipologia_codigo = .data[[col_tip]],
    uf = if(!is.na(col_uf)) .data[[col_uf]] else "BR"
  ) |>
  mutate(
    grupo_de_desastre = str_squish(as.character(grupo_de_desastre)),
    descricao_tipologia = str_squish(as.character(descricao_tipologia)),
    tipologia_codigo = as.character(tipologia_codigo),
    uf = str_squish(as.character(uf))
  ) |>
  filter(!is.na(grupo_de_desastre), grupo_de_desastre != "")

# ---------------------------
# 1. Ranking nacional por grupos
# ---------------------------
ranking_grupos <- bd2 |>
  count(grupo_de_desastre, name = "ocorrencias") |>
  arrange(desc(ocorrencias)) |>
  mutate(
    perc_total = ocorrencias / sum(ocorrencias),
    posicao = row_number(),
    grupo_norm = normalize_ascii(grupo_de_desastre)
  )

tipologias_por_grupo <- bd2 |>
  distinct(grupo_de_desastre, descricao_tipologia, tipologia_codigo) |>
  count(grupo_de_desastre, name = "n_tipologias")

ranking_grupos <- ranking_grupos |>
  left_join(tipologias_por_grupo, by = "grupo_de_desastre")

hidro <- ranking_grupos |>
  filter(grupo_norm == "hidrologico")

# ---------------------------
# 2. Tipologias dentro da hidrologia
# ---------------------------
tipologias_hidro <- bd2 |>
  mutate(grupo_norm = normalize_ascii(grupo_de_desastre)) |>
  filter(grupo_norm == "hidrologico") |>
  count(descricao_tipologia, name = "ocorrencias") |>
  arrange(desc(ocorrencias)) |>
  mutate(perc = ocorrencias / sum(ocorrencias))

# ---------------------------
# 3. Estados com mais ocorrências hidrológicas
# ---------------------------
ufs_hidro <- bd2 |>
  mutate(grupo_norm = normalize_ascii(grupo_de_desastre)) |>
  filter(grupo_norm == "hidrologico") |>
  count(uf, name = "ocorrencias") |>
  arrange(desc(ocorrencias)) |>
  mutate(perc = ocorrencias / sum(ocorrencias))

# ---------------------------
# Exportar tabelas
# ---------------------------
write.csv(ranking_grupos, file.path(base_output, "01_tabelas", "ranking_grupos_desastres_nacionais.csv"), row.names = FALSE)
write.csv(tipologias_hidro, file.path(base_output, "01_tabelas", "tipologias_hidrologicas_nacionais.csv"), row.names = FALSE)
write.csv(ufs_hidro, file.path(base_output, "01_tabelas", "ufs_hidrologia.csv"), row.names = FALSE)

# ---------------------------
# Gráfico 1 - Ranking dos grupos de desastres
# ---------------------------
p_grupos <- ranking_grupos |>
  mutate(destaque = grupo_norm == "hidrologico") |>
  ggplot(aes(x = fct_reorder(grupo_de_desastre, ocorrencias), y = ocorrencias, fill = destaque)) +
  geom_col() +
  geom_text(aes(label = scales::comma(ocorrencias, big.mark = ".", decimal.mark = ",")),
            hjust = -0.1, size = 4.6) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#E58B1F", "FALSE" = "steelblue")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Ranking nacional dos grupos de desastres",
    subtitle = "A barra laranja destaca a posição da hidrologia no conjunto nacional",
    x = "Grupo de desastre",
    y = "Ocorrências"
  ) +
  theme_cientometria(legend = "none")

save_plot(p_grupos, file.path(base_output, "02_graficos", "01-ranking-grupos-desastres.png"), width = 14, height = 8)

# ---------------------------
# Gráfico 2 - Tipologias hidrológicas
# ---------------------------
p_tipos_hidro <- tipologias_hidro |>
  ggplot(aes(x = fct_reorder(descricao_tipologia, ocorrencias), y = ocorrencias, fill = descricao_tipologia)) +
  geom_col() +
  geom_text(aes(label = paste0(scales::comma(ocorrencias, big.mark = "."), " (", scales::percent(perc, accuracy = 0.1, decimal.mark = ","), ")")),
            hjust = -0.1, size = 4.4) +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Tipologias dentro da hidrologia",
    subtitle = "Distribuição nacional das tipologias hidrológicas",
    x = "Tipologia",
    y = "Ocorrências"
  ) +
  theme_cientometria()

save_plot(p_tipos_hidro, file.path(base_output, "02_graficos", "02-tipologias-hidrologicas.png"), width = 14, height = 8)

# ---------------------------
# Gráfico 3 - Top UFs da hidrologia
# ---------------------------
p_ufs_hidro <- ufs_hidro |>
  slice_head(n = 15) |>
  mutate(destaque = uf == "SC") |>
  ggplot(aes(x = fct_reorder(uf, ocorrencias), y = ocorrencias, fill = destaque)) +
  geom_col() +
  geom_text(aes(label = scales::comma(ocorrencias, big.mark = ".", decimal.mark = ",")),
            vjust = -0.35, size = 4.4) +
  scale_fill_manual(values = c("TRUE" = "#E58B1F", "FALSE" = "#2E6F8E")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Estados com mais ocorrências hidrológicas",
    subtitle = "Top 15 UFs com destaque para Santa Catarina",
    x = "UF",
    y = "Ocorrências"
  ) +
  theme_cientometria(legend = "none")

save_plot(p_ufs_hidro, file.path(base_output, "02_graficos", "03-ufs-hidrologia-top15.png"), width = 14, height = 8)

# ---------------------------
# Síntese textual no console
# ---------------------------
cat(glue(
  "A hidrologia está na posição {hidro$posicao} entre {nrow(ranking_grupos)} grupos, com {scales::comma(hidro$ocorrencias, big.mark='.') } ocorrências e {scales::percent(hidro$perc_total, accuracy = 0.1, decimal.mark = ',')} do total nacional.\n"
))
