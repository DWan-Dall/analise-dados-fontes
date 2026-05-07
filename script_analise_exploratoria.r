# =========================================================
# Análise exploratória - Atlas Digital
# Base: BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado-_AREA_INTERESSE.xlsx
# =========================================================

library(tidyverse)
library(readxl)
library(tidyr)
library(janitor)
library(lubridate)
library(skimr)
library(glue)
library(scales)
library(fs)
library(rmarkdown)

# ---------------------------
# Caminhos
# ---------------------------
base_output <- "output_v1/edexplo"
path_base <- "dados/Atlas_Digital/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado _AREA_INTERESSE.xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c("01_estrutura","02_qualidade","03_temporal","04_municipios","05_impactos","06_tipos","07_relatorio"), pasta)

# ---------------------------
# Tema e funções auxiliares
# ---------------------------
theme_analise <- function(base_size = 16, legend = "none"){
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

# abrv_num <- function(x) scales::label_number_si(accuracy = 0.1)(x)
abrv_num <- function(x) label_number(scale_cut = cut_short_scale())(x)

# ---------------------------
# Leitura e limpeza
# ---------------------------
dados <- read_excel(path_base, sheet = "Área de Interesse") %>%
  clean_names()

# Datas
parse_excel_date <- function(x){
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1991-04-12"))
  out <- suppressWarnings(dmy(as.character(x)))
  out2 <- suppressWarnings(ymd(as.character(x)))
  coalesce(out, out2)
}

if ("data_evento" %in% names(dados)) dados <- dados %>% mutate(data_evento = parse_excel_date(data_evento))
if ("data_registro" %in% names(dados)) dados <- dados %>% mutate(data_registro = parse_excel_date(data_registro))

# Numéricos
# num_cols <- names(dados)[str_detect(names(dados), "^(dh_|dm_|da_|pepl_|pepr_)")]

# Converte só colunas realmente numéricas
num_cols <- intersect(
  c("dh_mortos","dh_feridos","dh_enfermos","dh_desabrigados","dh_desalojados","dh_desaparecidos",
    "dh_afetados_seca_estiagem","dh_total_danos_humanos_diretos","dh_outros_afetados",
    "dm_uni_habita_danificadas","dm_uni_habita_destruidas","dm_uni_habita_valor",
    "dm_inst_saude_danificadas","dm_inst_saude_destruidas","dm_inst_saude_valor",
    "dm_inst_ensino_danificadas","dm_inst_ensino_destruidas","dm_inst_ensino_valor",
    "dm_inst_servicos_danificadas","dm_inst_servicos_destruidas","dm_inst_servicos_valor",
    "dm_inst_comuni_danificadas","dm_inst_comuni_destruidas","dm_inst_comuni_valor",
    "dm_obras_de_infra_danificadas","dm_obras_de_infra_destruidas","dm_obras_de_infra_valor",
    "dm_total_danos_materiais",
    "pepl_assis_med_e_emergn_r_","pepl_abast_de_agua_pot_r_","pepl_sist_de_esgotos_sanit_r_",
    "pepl_sis_limp_e_rec_lixo_r_","pepl_sis_cont_pragas_r_","pepl_distrib_energia_r_",
    "pepl_telecomunicacoes_r_","pepl_tran_loc_reg_l_curso_r_","pepl_distrib_combustiveis_r_",
    "pepl_seguranca_publica_r_","pepl_ensino_r_","pepl_total_publico",
    "pepr_agricultura_r_","pepr_pecuaria_r_","pepr_industria_r_","pepr_comercio_r_",
    "pepr_servicos_r_","pepr_total_privado"),
  names(dados)
)

dados <- dados %>% mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x), locale = locale(decimal_mark = ",", grouping_mark = "."))))

# Variáveis derivadas
if ("data_evento" %in% names(dados)) {
  dados <- dados %>%
    mutate(
      ano = year(data_evento),
      mes = month(data_evento),
      mes_nome = factor(mes, levels = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")),
      ano_mes = format(data_evento, "%Y-%m")
    )
}

# ---------------------------
# 1 Estrutura
# ---------------------------
estrutura <- dados %>%
  summarise(across(everything(), list(tipo = ~ class(.x)[1], falt = ~ sum(is.na(.x)), pct = ~ mean(is.na(.x))*100), .names = "{.col}__{.fn}"))
write_csv(tibble::enframe(as.list(estrutura), name = "campo", value = "valor"), file.path(base_output, "01_estrutura", "01-estrutura.csv"))

# ---------------------------
# 2 Qualidade
# ---------------------------
qualidade <- tibble(
  indicador = c("linhas", "colunas", "duplicadas_linha", "datas_evento_na", "datas_registro_na"),
  valor = c(nrow(dados), ncol(dados), sum(duplicated(dados)), if ("data_evento" %in% names(dados)) sum(is.na(dados$data_evento)) else NA_integer_, if ("data_registro" %in% names(dados)) sum(is.na(dados$data_registro)) else NA_integer_)
)
write_csv(qualidade, file.path(base_output, "02_qualidade", "01-qualidade.csv"))

# ---------------------------
# 3 Temporal
# ---------------------------
serie_ano <- dados %>% count(ano, name = "n_eventos") %>% filter(!is.na(ano))

p_ano <- ggplot(serie_ano, aes(x = factor(ano), y = n_eventos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Ano", y = "Quantidade de eventos", title = "Eventos por ano (1991-2024)", subtitle = "Base Atlas Digital | frequência anual") +
  theme_analise() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.png"), 16, 8)
write_csv(serie_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.csv"))

serie_mes <- dados %>% count(mes_nome, name = "n_eventos")

p_mes <- ggplot(serie_mes, aes(x = mes_nome, y = n_eventos)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.8) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Mês", y = "Quantidade de eventos", title = "Sazonalidade dos eventos por mês", subtitle = "Base Atlas Digital | distribuição mensal") +
  theme_analise()
save_plot(p_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.png"), 16, 8)
write_csv(serie_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.csv"))

# ---------------------------
# 4 Municípios
# ---------------------------
serie_mun <- dados %>%
  count(nome_municipio, sort = TRUE, name = "n_eventos") %>%
  slice_head(n = 15) %>%
  arrange(n_eventos)

p_mun <- ggplot(serie_mun, aes(x = n_eventos, y = fct_reorder(nome_municipio, n_eventos))) +
  geom_col(fill = "seagreen4") +
  geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.8) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
  labs(x = "Quantidade de eventos", y = "Município", title = "Municípios com mais registros", subtitle = "Base Atlas Digital | top 15 municípios") +
  theme_analise(base_size = 16, legend = "none")
save_plot(p_mun, file.path(base_output, "04_municipios", "01-top-municipios.png"), 16, 8)
write_csv(serie_mun, file.path(base_output, "04_municipios", "01-top-municipios.csv"))

# ---------------------------
# 5 Impactos
# ---------------------------
# vars_impacto <- intersect(c("dh_total_danos_humanos_diretos","dm_total_danos_materiais","pepl_total_publico","pepr_total_privado"), names(dados))
# resumo_impactos <- dados %>%
#   summarise(across(all_of(vars_impacto), list(total = ~ sum(.x, na.rm = TRUE), media = ~ mean(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE)))) %>%
#   pivot_longer(everything(), names_to = c("variavel", "medida"), names_sep = "_(?=[^_]+$)", values_to = "valor")
# write_csv(resumo_impactos, file.path(base_output, "05_impactos", "01-resumo-impactos.csv"))

# p_imp <- ggplot(resumo_impactos, aes(x = variavel, y = valor, fill = medida)) +
#   geom_col(position = position_dodge(width = 0.8)) +
#   scale_y_continuous(labels = abrv_num) +
#   labs(x = "Indicador", y = "Valor", fill = "Medida", title = "Resumo de impactos por indicador", subtitle = "Base Atlas Digital | total e média") +
#   theme_analise(base_size = 15, legend = "top")
# save_plot(p_imp, file.path(base_output, "05_impactos", "01-resumo-impactos.png"), 16, 8)

# if (length(vars_impacto) >= 2) {
#   corr <- cor(dados[, vars_impacto], use = "pairwise.complete.obs")
#   write_csv(as.data.frame(corr) %>% rownames_to_column("variavel"), file.path(base_output, "05_impactos", "02-matriz-correlacao.csv"))
#   p_corr <- corr %>% as.data.frame() %>% rownames_to_column("variavel") %>% pivot_longer(-variavel, names_to = "var2", values_to = "cor") %>%
#     ggplot(aes(var2, variavel, fill = cor)) +
#     geom_tile() +
#     geom_text(aes(label = sprintf("%.2f", cor)), size = 4) +
#     scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1,1)) +
#     labs(x = "Variável", y = "Variável", fill = "Correlação", title = "Correlação entre impactos", subtitle = "Base Atlas Digital | matriz de correlação") +
#     theme_analise(base_size = 14, legend = "right") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   save_plot(p_corr, file.path(base_output, "05_impactos", "02-matriz-correlacao.png"), 14, 8)
# }

vars_impacto <- intersect(
  c("dh_total_danos_humanos_diretos","dm_total_danos_materiais","pepl_total_publico","pepr_total_privado"),
  names(dados)
)

resumo_impactos <- dados %>%
  summarise(across(all_of(vars_impacto),
                   list(total = ~ sum(.x, na.rm = TRUE),
                        media = ~ mean(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("variavel", "medida"),
               names_pattern = "(.*)_(total|media)",
               values_to = "valor") %>%
  mutate(
    variavel = recode(variavel,
                      dh_total_danos_humanos_diretos = "Danos humanos",
                      dm_total_danos_materiais = "Danos materiais",
                      pepl_total_publico = "Prejuízo público",
                      pepr_total_privado = "Prejuízo privado")
  )

p_imp <- ggplot(resumo_impactos, aes(x = variavel, y = valor, fill = medida)) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_y_continuous(labels = abrv_num) +
  labs(x = "Indicador", y = "Valor", fill = "Medida",
       title = "Resumo de impactos por indicador",
       subtitle = "Base Atlas Digital | total e média") +
  theme_cientometria(base_size = 15, legend = "top")

save_plot(p_imp, file.path(base_output, "05_impactos", "01-resumo-impactos.png"), 16, 8)

# ---------------------------
# 6 Tipologias
# ---------------------------
# if ("tipologia" %in% names(dados)) {
#   serie_tipo <- dados %>% count(tipologia, sort = TRUE, name = "n_eventos") %>% slice_head(n = 10) %>% arrange(n_eventos)
#   write_csv(serie_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.csv"))
#   p_tipo <- ggplot(serie_tipo, aes(x = n_eventos, y = fct_reorder(tipologia, n_eventos))) +
#     geom_col(fill = "purple4") +
#     geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.8) +
#     scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
#     labs(x = "Quantidade de eventos", y = "Tipologia", title = "Tipologias mais frequentes", subtitle = "Base Atlas Digital | top 10 tipologias") +
#     theme_analise(base_size = 16, legend = "none")
#   save_plot(p_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.png"), 16, 8)
# }

if ("tipologia" %in% names(dados)) {
  dados <- dados %>% mutate(tipologia = as.character(tipologia))

  serie_tipo <- dados %>%
    count(tipologia, sort = TRUE, name = "n_eventos") %>%
    slice_head(n = 10) %>%
    arrange(n_eventos)

  p_tipo <- ggplot(serie_tipo, aes(x = n_eventos, y = fct_reorder(tipologia, n_eventos))) +
    geom_col(fill = "purple4") +
    geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.8) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
    labs(x = "Quantidade de eventos", y = "Tipologia",
         title = "Tipologias mais frequentes",
         subtitle = "Base Atlas Digital | top 10 tipologias") +
    theme_cientometria(base_size = 16, legend = "none")

  save_plot(p_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.png"), 16, 8)
  write_csv(serie_tipo, file.path(base_output, "06_tipos", "01-top-tipologias.csv"))
}

# ---------------------------
# 7 Resumo final
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

cat("Script concluído com sucesso.
")
