# =========================================================
# Gráficos de comparação dos estados - desastres hidrológicos
# Sem HTML, apenas tabelas e gráficos em PNG/CSV
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(scales)
library(fs)

base_output <- "output/comparacao_sc_estados_hidrologicos"
path_base <- "dados/Atlas_Digital/Dados_Originais/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado (2).xlsx"
walk(c("07_graficos_estados"), ~ dir_create(path(base_output, .x), recurse = TRUE))

theme_cientometria <- function(base_size = 16, legend = "none"){
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

save_plot <- function(p, file, width = 16, height = 8){
  ggsave(filename = file, plot = p, width = width, height = height, dpi = 300)
}

dados <- read_excel(path_base, sheet = "Atlas Valores Corrigidos") %>% clean_names()

hidro <- dados %>%
  filter(grupo_de_desastre == "Hidrológico") %>%
  mutate(sigla_uf = toupper(sigla_uf))

cat_evento <- case_when(
  "descricao_tipologia" %in% names(hidro) ~ "descricao_tipologia",
  "tipologia" %in% names(hidro) ~ "tipologia",
  TRUE ~ NA_character_
)

# ---------------------------
# Ranking de estados por ocorrências
# ---------------------------
ranking_estados <- hidro %>%
  count(sigla_uf, name = "ocorrencias") %>%
  arrange(desc(ocorrencias)) %>%
  mutate(
    posicao = row_number(),
    pct_total = 100 * ocorrencias / sum(ocorrencias),
    destaque_sc = sigla_uf == "SC"
  )

write_csv(ranking_estados, file.path(base_output, "07_graficos_estados", "01-ranking-estados-ocorrencias.csv"))

# tabela resumo SC
resumo_sc <- ranking_estados %>%
  filter(sigla_uf == "SC") %>%
  transmute(
    estado = sigla_uf,
    posicao,
    ocorrencias,
    pct_total,
    total_ocorrencias_brasil = sum(ranking_estados$ocorrencias),
    estados_analisados = nrow(ranking_estados)
  )

write_csv(resumo_sc, file.path(base_output, "07_graficos_estados", "02-resumo-sc.csv"))

# gráfico top 15 com SC destacado
ranking_top15 <- ranking_estados %>% slice_max(order_by = ocorrencias, n = 15, with_ties = FALSE)

p_top15 <- ggplot(ranking_top15, aes(x = reorder(sigla_uf, -ocorrencias), y = ocorrencias, fill = destaque_sc)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = scales::comma(ocorrencias, big.mark = ".", decimal.mark = ",")), vjust = -0.3, size = 3.8) +
  scale_fill_manual(values = c("TRUE" = "#FF0000", "FALSE" = "#5A8FBB")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, .1))) +
  labs(
    x = "Estado",
    y = "Ocorrências",
    title = "Ranking de estados por ocorrências hidrológicas",
    subtitle = "Top 15 estados | SC destacado em laranja"
  ) +
  theme_cientometria(base_size = 15)

save_plot(p_top15, file.path(base_output, "07_graficos_estados", "03-ranking-top15-estados.png"), 16, 8)

# gráfico completo horizontal
p_ranking_full <- ggplot(ranking_estados %>% arrange(ocorrencias), aes(x = ocorrencias, y = fct_reorder(sigla_uf, ocorrencias), fill = destaque_sc)) +
  geom_col() +
  geom_text(aes(label = scales::comma(ocorrencias, big.mark = ".", decimal.mark = ",")), hjust = -0.15, size = 3.6) +
  scale_fill_manual(values = c("TRUE" = "#FF0000", "FALSE" = "#5A8FBB")) +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, .12))) +
  labs(
    x = "Ocorrências",
    y = "Estado",
    title = "Ranking completo de estados por ocorrências hidrológicas",
    subtitle = "Todas as UFs ordenadas pelo número de registros"
  ) +
  theme_cientometria(base_size = 14)

save_plot(p_ranking_full, file.path(base_output, "07_graficos_estados", "04-ranking-completo-estados.png"), 14, 12)

# ---------------------------
# Tipologias
# ---------------------------
if (!is.na(cat_evento)) {
  tipologias <- hidro %>%
    mutate(tipo_evento = .data[[cat_evento]]) %>%
    count(tipo_evento, name = "ocorrencias") %>%
    arrange(desc(ocorrencias)) %>%
    mutate(pct = 100 * ocorrencias / sum(ocorrencias))

  write_csv(tipologias, file.path(base_output, "07_graficos_estados", "05-tipologias-hidrologicas.csv"))

  p_tipos <- ggplot(tipologias %>% arrange(ocorrencias), aes(x = ocorrencias, y = fct_reorder(tipo_evento, ocorrencias), fill = tipo_evento)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = paste0(scales::comma(ocorrencias, big.mark = ".", decimal.mark = ","), " (", round(pct, 1), "%)")), hjust = -0.12, size = 3.8) +
    scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, .2))) +
    labs(
      x = "Ocorrências",
      y = "Tipologia",
      title = "Tipologias dos desastres hidrológicos",
      subtitle = "Distribuição nacional por tipo de evento"
    ) +
    theme_cientometria(base_size = 14)

  save_plot(p_tipos, file.path(base_output, "07_graficos_estados", "06-tipologias-hidrologicas.png"), 15, 8)

  p_tipos_pizza <- ggplot(tipologias, aes(x = "", y = ocorrencias, fill = tipo_evento)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(tipo_evento, ": ", round(pct, 1), "%")), position = position_stack(vjust = 0.5), size = 4) +
    labs(
      title = "Composição das tipologias hidrológicas",
      subtitle = "Participação percentual de cada tipo"
    ) +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(face = "bold", size = 18), plot.subtitle = element_text(size = 13), legend.position = "none")

  save_plot(p_tipos_pizza, file.path(base_output, "07_graficos_estados", "07-tipologias-pizza.png"), 12, 8)

    p_tipos_pizza <- ggplot(tipologias, aes(x = "", y = ocorrencias, fill = tipo_evento)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(
      title = "Composição das tipologias hidrológicas",
      subtitle = "Participação percentual de cada tipo",
      fill = "Tipologia"
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 13),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 12)
    )
  
  save_plot(p_tipos_pizza, file.path(base_output, "07_graficos_estados", "07-tipologias-pizza-com-legenda.png"), 12, 8)

  # composição por estado e tipo
  estados_tipo <- hidro %>%
    mutate(tipo_evento = .data[[cat_evento]]) %>%
    count(sigla_uf, tipo_evento, name = "n_eventos") %>%
    group_by(sigla_uf) %>%
    mutate(total_estado = sum(n_eventos)) %>%
    ungroup()

  write_csv(estados_tipo, file.path(base_output, "07_graficos_estados", "08-estados-composicao-tipo.csv"))

  ordem_ufs <- estados_tipo %>% distinct(sigla_uf, total_estado) %>% arrange(total_estado) %>% pull(sigla_uf)
  estados_tipo <- estados_tipo %>% mutate(sigla_uf = factor(sigla_uf, levels = ordem_ufs))

  p_comp <- ggplot(estados_tipo, aes(x = n_eventos, y = sigla_uf, fill = tipo_evento)) +
    geom_col() +
    scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, .04))) +
    labs(
      x = "Quantidade de eventos",
      y = "Estado",
      fill = "Tipo de evento",
      title = "Composição dos eventos hidrológicos por estado",
      subtitle = "Totais estaduais com divisão por tipologia"
    ) +
    theme_cientometria(base_size = 14, legend = "right")

  save_plot(p_comp, file.path(base_output, "07_graficos_estados", "09-composicao-estados-tipologia.png"), 18, 12)
}

cat("Gráficos gerados com sucesso.\n")

