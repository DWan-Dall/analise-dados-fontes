# =========================================================
# Análise exploratória - Atlas Digital (v3)
# Inclui: cruzamento com sheet Grupo de Desastres
# Chaves: Atlas N. Tipologia <-> Atlas - Descrição Tipologia
# Também inclui visão de composição dos eventos dentro do total municipal
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(fs)

# ---------------------------
# Caminhos
# ---------------------------
base_output <- "output_v3/edexplo"
path_base <- "dados/Atlas_Digital/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado _AREA_INTERESSE.xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c(
  "01_estrutura", "02_qualidade", "03_temporal", "04_municipios",
  "05_impactos", "06_tipos", "07_relatorio", "08_auxiliares"
), pasta)

# ---------------------------
# Tema e funções auxiliares
# ---------------------------
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

abrv_num <- function(){
  label_number(scale_cut = cut_short_scale(), accuracy = 0.1)
}

parse_excel_date <- function(x){
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  out1 <- suppressWarnings(dmy(as.character(x)))
  out2 <- suppressWarnings(ymd(as.character(x)))
  coalesce(out1, out2)
}

# ---------------------------
# Leitura das abas
# ---------------------------
sheets <- excel_sheets(path_base)

dados <- read_excel(path_base, sheet = "Área de Interesse") %>%
  clean_names()

if ("Grupo de Desastres" %in% sheets) {
  grupo_desastres <- read_excel(path_base, sheet = "Grupo de Desastres") %>%
    clean_names()
} else {
  grupo_desastres <- tibble()
}

# ---------------------------
# Tratamento da base principal
# ---------------------------
if ("data_evento" %in% names(dados)) {
  dados <- dados %>% mutate(data_evento = parse_excel_date(data_evento))
}
if ("data_registro" %in% names(dados)) {
  dados <- dados %>% mutate(data_registro = parse_excel_date(data_registro))
}

num_cols <- intersect(
  c(
    "dh_mortos", "dh_feridos", "dh_enfermos", "dh_desabrigados", "dh_desalojados",
    "dh_desaparecidos", "dh_afetados_seca_estiagem", "dh_total_danos_humanos_diretos",
    "dh_outros_afetados", "dm_uni_habita_danificadas", "dm_uni_habita_destruidas",
    "dm_uni_habita_valor", "dm_inst_saude_danificadas", "dm_inst_saude_destruidas",
    "dm_inst_saude_valor", "dm_inst_ensino_danificadas", "dm_inst_ensino_destruidas",
    "dm_inst_ensino_valor", "dm_inst_servicos_danificadas", "dm_inst_servicos_destruidas",
    "dm_inst_servicos_valor", "dm_inst_comuni_danificadas", "dm_inst_comuni_destruidas",
    "dm_inst_comuni_valor", "dm_obras_de_infra_danificadas", "dm_obras_de_infra_destruidas",
    "dm_obras_de_infra_valor", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"
  ), names(dados)
)

dados <- dados %>%
  mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x))))

if ("tipologia" %in% names(dados)) dados <- dados %>% mutate(tipologia = as.character(tipologia))
if ("descricao_tipologia" %in% names(dados)) dados <- dados %>% mutate(descricao_tipologia = as.character(descricao_tipologia))

if ("data_evento" %in% names(dados)) {
  dados <- dados %>%
    mutate(
      ano = year(data_evento),
      mes = month(data_evento),
      mes_nome = factor(mes, levels = 1:12,
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
      ano_mes = format(data_evento, "%Y-%m")
    )
}

# ---------------------------
# Tratamento da aba Grupo de Desastres
# Cruzamento: Atlas N. Tipologia <-> Atlas - Descrição Tipologia
# ---------------------------
if (nrow(grupo_desastres) > 0) {
  nomes_esperados <- names(grupo_desastres)

  grupo_ref <- grupo_desastres %>%
    mutate(
      atlas_n_tipologia = as.character(atlas_n_tipologia),
      atlas_descricao_tipologia = as.character(atlas_descricao_tipologia),
      grupo_de_desastres = as.character(grupo_de_desastres)
    ) %>%
    select(atlas_n_tipologia, atlas_descricao_tipologia, grupo_de_desastres, everything())

  write_csv(grupo_ref, file.path(base_output, "08_auxiliares", "01-grupo-de-desastres.csv"))

  # cruzamento principal conforme solicitado
  if ("tipologia" %in% names(dados)) {
    dados <- dados %>%
      mutate(tipologia_chr = as.character(tipologia)) %>%
      left_join(
        grupo_ref %>% select(atlas_n_tipologia, atlas_descricao_tipologia, grupo_de_desastres),
        by = c("tipologia_chr" = "atlas_n_tipologia")
      )
  }

  # coluna final de categoria para gráficos
  dados <- dados %>%
    mutate(categoria_evento = case_when(
      !is.na(atlas_descricao_tipologia) & atlas_descricao_tipologia != "" ~ atlas_descricao_tipologia,
      !is.na(descricao_tipologia) & descricao_tipologia != "" ~ descricao_tipologia,
      !is.na(grupo_de_desastres) & grupo_de_desastres != "" ~ grupo_de_desastres,
      !is.na(tipologia) ~ as.character(tipologia),
      TRUE ~ "Sem informação"
    ))
} else {
  dados <- dados %>%
    mutate(categoria_evento = case_when(
      !is.na(descricao_tipologia) & descricao_tipologia != "" ~ descricao_tipologia,
      !is.na(tipologia) ~ as.character(tipologia),
      TRUE ~ "Sem informação"
    ))
}

# ---------------------------
# Municípios - totais e composição interna
# ---------------------------
municipios_total <- dados %>%
  count(nome_municipio, sort = TRUE, name = "n_eventos_total")
write_csv(municipios_total, file.path(base_output, "04_municipios", "01-municipios-todos.csv"))

municipios_composicao <- dados %>%
  count(nome_municipio, categoria_evento, name = "n_eventos") %>%
  group_by(nome_municipio) %>%
  mutate(
    total_municipio = sum(n_eventos),
    perc_no_municipio = 100 * n_eventos / total_municipio
  ) %>%
  ungroup() %>%
  arrange(desc(total_municipio), desc(n_eventos))
write_csv(municipios_composicao, file.path(base_output, "04_municipios", "02-municipios-composicao-eventos.csv"))

ordem_municipios <- municipios_total %>%
  arrange(n_eventos_total) %>%
  pull(nome_municipio)

municipios_composicao <- municipios_composicao %>%
  mutate(nome_municipio = factor(nome_municipio, levels = ordem_municipios))

# gráfico 1: totais por município com composição embutida
p_municipios_comp <- ggplot(municipios_composicao, aes(x = n_eventos, y = nome_municipio, fill = categoria_evento)) +
  geom_col() +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .03))) +
  labs(
    x = "Quantidade de eventos",
    y = "Município",
    fill = "Categoria do evento",
    title = "Total de eventos por município com composição interna",
    subtitle = "Cada barra representa o total municipal e sua divisão por categoria"
  ) +
  theme_cientometria(base_size = 15, legend = "right") +
  theme(axis.text.y = element_text(size = 10))
save_plot(p_municipios_comp, file.path(base_output, "04_municipios", "02-municipios-composicao-eventos.png"), 18, 12)

# gráfico 2: percentual dentro do município
p_municipios_pct <- ggplot(municipios_composicao, aes(x = perc_no_municipio, y = nome_municipio, fill = categoria_evento)) +
  geom_col() +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, .03))) +
  labs(
    x = "Percentual dentro do município",
    y = "Município",
    fill = "Categoria do evento",
    title = "Composição percentual dos eventos por município",
    subtitle = "Participação de cada categoria dentro do total de eventos do município"
  ) +
  theme_cientometria(base_size = 15, legend = "right") +
  theme(axis.text.y = element_text(size = 10))
save_plot(p_municipios_pct, file.path(base_output, "04_municipios", "03-municipios-composicao-percentual.png"), 18, 12)

# ---------------------------
# Correlação entre impactos
# ---------------------------
vars_impacto <- intersect(
  c("dh_total_danos_humanos_diretos", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"),
  names(dados)
)

if (length(vars_impacto) >= 2) {
  corr <- cor(dados[, vars_impacto], use = "pairwise.complete.obs")
  corr_df <- as.data.frame(corr) %>% rownames_to_column("variavel")
  write_csv(corr_df, file.path(base_output, "05_impactos", "02-matriz-correlacao.csv"))

  corr_long <- corr_df %>%
    pivot_longer(-variavel, names_to = "var2", values_to = "cor")

  p_corr <- ggplot(corr_long, aes(x = var2, y = variavel, fill = cor)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", cor)), size = 4) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1, 1)) +
    labs(
      x = "Variável",
      y = "Variável",
      fill = "Correlação",
      title = "Correlação entre impactos",
      subtitle = "Relação entre danos humanos, materiais e prejuízos"
    ) +
    theme_cientometria(base_size = 14, legend = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_plot(p_corr, file.path(base_output, "05_impactos", "02-matriz-correlacao.png"), 14, 8)
}

# ---------------------------
# Resumo final
# ---------------------------
resumo <- tibble(
  linhas = nrow(dados),
  colunas = ncol(dados),
  municipios = if ("nome_municipio" %in% names(dados)) n_distinct(dados$nome_municipio) else NA,
  categorias_evento = n_distinct(dados$categoria_evento)
)
write_csv(resumo, file.path(base_output, "07_relatorio", "00-resumo-geral.csv"))

cat("Script v3 concluído com sucesso.\n")
