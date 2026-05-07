# =========================================================
# Análise exploratória - Atlas Digital (versão revisada)
# Inclui: correlação entre impactos + gráfico dos 35 municípios
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(skimr)
library(scales)
library(fs)
library(glue)
library(knitr)

# ---------------------------
# Caminhos
# ---------------------------
base_output <- "output_v2/edexplo"
path_base <- "dados/Atlas_Digital/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado _AREA_INTERESSE.xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c(
  "01_estrutura", "02_qualidade", "03_temporal", "04_municipios",
  "05_impactos", "06_tipos", "07_relatorio"
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
# Leitura
# ---------------------------
dados <- read_excel(path_base, sheet = "Área de Interesse") %>%
  clean_names()

# ---------------------------
# Tratamento
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
    "dm_obras_de_infra_valor", "dm_total_danos_materiais", "pepl_assis_med_e_emergen_r",
    "pepl_abast_de_agua_pot_r", "pepl_sist_de_esgotos_sanit_r", "pepl_sis_limp_e_rec_lixo_r",
    "pepl_sis_cont_pragas_r", "pepl_distrib_energia_r", "pepl_telecomunicacoes_r",
    "pepl_tran_loc_reg_l_curso_r", "pepl_distrib_combustiveis_r", "pepl_seguranca_publica_r",
    "pepl_ensino_r", "pepl_total_publico", "pepr_agricultura_r", "pepr_pecuaria_r",
    "pepr_industria_r", "pepr_comercio_r", "pepr_servicos_r", "pepr_total_privado", "tipologia"
  ),
  names(dados)
)

num_cols <- setdiff(num_cols, "tipologia")

dados <- dados %>%
  mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x), locale = locale(decimal_mark = ".", grouping_mark = ","))))

if ("tipologia" %in% names(dados)) {
  dados <- dados %>% mutate(tipologia = as.character(tipologia))
}
if ("grupo_de_desastre" %in% names(dados)) {
  dados <- dados %>% mutate(grupo_de_desastre = as.character(grupo_de_desastre))
}

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
# Estrutura e qualidade
# ---------------------------
estrutura <- tibble(
  coluna = names(dados),
  tipo = map_chr(dados, ~ class(.x)[1]),
  faltantes = map_int(dados, ~ sum(is.na(.x))),
  pct_faltantes = round(map_dbl(dados, ~ mean(is.na(.x)) * 100), 2)
)
write_csv(estrutura, file.path(base_output, "01_estrutura", "01-estrutura.csv"))

qualidade <- tibble(
  indicador = c("linhas", "colunas", "duplicadas_linha", "datas_evento_na", "datas_registro_na"),
  valor = c(
    nrow(dados), ncol(dados), sum(duplicated(dados)),
    if ("data_evento" %in% names(dados)) sum(is.na(dados$data_evento)) else NA_integer_,
    if ("data_registro" %in% names(dados)) sum(is.na(dados$data_registro)) else NA_integer_
  )
)
write_csv(qualidade, file.path(base_output, "02_qualidade", "01-qualidade.csv"))

# ---------------------------
# Temporal
# ---------------------------
serie_ano <- dados %>% count(ano, name = "n_eventos") %>% filter(!is.na(ano))
write_csv(serie_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.csv"))

p_ano <- ggplot(serie_ano, aes(x = factor(ano), y = n_eventos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Ano", y = "Quantidade de eventos",
       title = "Eventos por ano (1991-2024)",
       subtitle = "Base Atlas Digital | frequência anual") +
  theme_cientometria()
save_plot(p_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.png"), 16, 8)

serie_mes <- dados %>% count(mes_nome, name = "n_eventos")
write_csv(serie_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.csv"))

p_mes <- ggplot(serie_mes, aes(x = mes_nome, y = n_eventos)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Mês", y = "Quantidade de eventos",
       title = "Sazonalidade dos eventos por mês",
       subtitle = "Base Atlas Digital | distribuição mensal") +
  theme_cientometria()
save_plot(p_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.png"), 16, 8)

# ---------------------------
# Municípios - todos os 35
# ---------------------------
serie_municipios_total <- dados %>%
  count(nome_municipio, sort = TRUE, name = "n_eventos")
write_csv(serie_municipios_total, file.path(base_output, "04_municipios", "01-municipios-todos.csv"))

municipios_tipo <- dados %>%
  mutate(tipo_plot = case_when(
    !is.na(grupo_de_desastre) & grupo_de_desastre != "" ~ grupo_de_desastre,
    !is.na(tipologia) & tipologia != "" ~ tipologia,
    TRUE ~ "Sem informação"
  )) %>%
  count(nome_municipio, tipo_plot, name = "n_eventos") %>%
  group_by(nome_municipio) %>%
  mutate(total_municipio = sum(n_eventos)) %>%
  ungroup() %>%
  arrange(total_municipio, nome_municipio)
write_csv(municipios_tipo, file.path(base_output, "04_municipios", "02-municipios-por-tipo.csv"))

ordem_municipios <- municipios_tipo %>%
  distinct(nome_municipio, total_municipio) %>%
  arrange(total_municipio) %>%
  pull(nome_municipio)

municipios_tipo <- municipios_tipo %>%
  mutate(nome_municipio = factor(nome_municipio, levels = ordem_municipios))

p_municipios_tipo <- ggplot(municipios_tipo, aes(x = n_eventos, y = nome_municipio, fill = tipo_plot)) +
  geom_col() +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .03))) +
  labs(x = "Quantidade de eventos", y = "Município", fill = "Tipo / grupo",
       title = "Municípios e composição dos eventos",
       subtitle = "35 municípios | barras horizontais com composição por tipologia ou grupo") +
  theme_cientometria(base_size = 15, legend = "right") +
  theme(axis.text.y = element_text(size = 10))
save_plot(p_municipios_tipo, file.path(base_output, "04_municipios", "02-municipios-por-tipo.png"), 18, 12)

# ---------------------------
# Impactos e correlação
# ---------------------------
vars_impacto <- intersect(
  c("dh_total_danos_humanos_diretos", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"),
  names(dados)
)

if (length(vars_impacto) > 0) {
  resumo_impactos <- dados %>%
    summarise(across(all_of(vars_impacto), list(
      total = ~ sum(.x, na.rm = TRUE),
      media = ~ mean(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE)
    ))) %>%
    pivot_longer(
      everything(),
      names_to = c("variavel", "medida"),
      names_pattern = "(.*)_(total|media|max)",
      values_to = "valor"
    )

  write_csv(resumo_impactos, file.path(base_output, "05_impactos", "01-resumo-impactos.csv"))

  p_imp <- ggplot(resumo_impactos %>% filter(medida %in% c("total", "media")),
                  aes(x = variavel, y = valor, fill = medida)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = abrv_num()) +
    labs(x = "Indicador", y = "Valor", fill = "Medida",
         title = "Resumo de impactos por indicador",
         subtitle = "Base Atlas Digital | total e média") +
    theme_cientometria(base_size = 15, legend = "top")
  save_plot(p_imp, file.path(base_output, "05_impactos", "01-resumo-impactos.png"), 16, 8)
}

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
    labs(x = "Variável", y = "Variável", fill = "Correlação",
         title = "Correlação entre impactos",
         subtitle = "Relação entre danos humanos, materiais e prejuízos") +
    theme_cientometria(base_size = 14, legend = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_plot(p_corr, file.path(base_output, "05_impactos", "02-matriz-correlacao.png"), 14, 8)
}

# ---------------------------
# Tipologias
# ---------------------------
if ("tipologia" %in% names(dados)) {
  serie_tipo <- dados %>%
    count(tipologia, sort = TRUE, name = "n_eventos") %>%
    mutate(tipologia = replace_na(tipologia, "Sem informação")) %>%
    arrange(n_eventos)

  write_csv(serie_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.csv"))

  p_tipo <- ggplot(serie_tipo, aes(x = n_eventos, y = fct_reorder(tipologia, n_eventos))) +
    geom_col(fill = "purple4") +
    geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.8) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
    labs(x = "Quantidade de eventos", y = "Tipologia",
         title = "Tipologias mais frequentes",
         subtitle = "Base Atlas Digital | distribuição das tipologias") +
    theme_cientometria(base_size = 16, legend = "none")

  save_plot(p_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.png"), 16, 8)
}

# ---------------------------
# Resumo final
# ---------------------------
resumo <- tibble(
  linhas = nrow(dados),
  colunas = ncol(dados),
  anos_min = if ("ano" %in% names(dados)) min(dados$ano, na.rm = TRUE) else NA,
  anos_max = if ("ano" %in% names(dados)) max(dados$ano, na.rm = TRUE) else NA,
  municipios = if ("nome_municipio" %in% names(dados)) n_distinct(dados$nome_municipio) else NA,
  tipologias = if ("tipologia" %in% names(dados)) n_distinct(dados$tipologia) else NA
)
write_csv(resumo, file.path(base_output, "07_relatorio", "00-resumo-geral.csv"))

cat("Script concluído com sucesso.\n")
