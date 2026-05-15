# =========================================================
# Análise exploratória - 35 cidades costeiras de SC
# Filtro: grupo_de_desastre == "Hidrológico"
# Inclui recorte por regiões costeiras definidas pelo estudo
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(fs)

base_output <- "output/analise_costeira_hidrologico_sc_regioes"
path_base <- "dados/Atlas_Digital/Dados_Originais/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado (2).xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c("08_regioes"), pasta)

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

parse_excel_date <- function(x){
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  out1 <- suppressWarnings(dmy(as.character(x)))
  out2 <- suppressWarnings(ymd(as.character(x)))
  coalesce(out1, out2)
}

abrv_num <- function(){
  label_number(scale_cut = cut_short_scale(), accuracy = 0.1)
}

municipios_regioes <- tribble(
  ~nome_municipio, ~regiao_costeira,
  "Itapoá", "Região Norte",
  "Garuva", "Região Norte",
  "São Francisco do Sul", "Região Norte",
  "Joinville", "Região Norte",
  "Araquari", "Região Norte",
  "Balneário Barra do Sul", "Região Norte",
  "Barra Velha", "Região Norte",
  "Balneário Piçarras", "Região Centro-Norte",
  "Penha", "Região Centro-Norte",
  "Navegantes", "Região Centro-Norte",
  "Itajaí", "Região Centro-Norte",
  "Balneário Camboriú", "Região Centro-Norte",
  "Itapema", "Região Centro-Norte",
  "Porto Belo", "Região Centro-Norte",
  "Bombinhas", "Região Centro-Norte",
  "Governador Celso Ramos", "Região Central",
  "Biguaçu", "Região Central",
  "São José", "Região Central",
  "Florianópolis", "Região Central",
  "Palhoça", "Região Central",
  "Tijucas", "Região Central",
  "Garopaba", "Região Centro-Sul",
  "Imbituba", "Região Centro-Sul",
  "Laguna", "Região Centro-Sul",
  "Jaguaruna", "Região Centro-Sul",
  "Pescaria Brava", "Região Centro-Sul",
  "Imaruí", "Região Centro-Sul",
  "Balneário Rincão", "Região Sul",
  "Araranguá", "Região Sul",
  "Balneário Gaivota", "Região Sul",
  "Passo de Torres", "Região Sul",
  "Balneário Arroio do Silva", "Região Sul",
  "Sombrio", "Região Sul",
  "Santa Rosa do Sul", "Região Sul",
  "São João do Sul", "Região Sul"
)

write_csv(municipios_regioes, file.path(base_output, "08_regioes", "00-municipios-regioes.csv"))

dados <- read_excel(path_base, sheet = "Atlas Valores Corrigidos") %>% clean_names()

if ("data_evento" %in% names(dados)) dados <- dados %>% mutate(data_evento = parse_excel_date(data_evento))
if ("data_registro" %in% names(dados)) dados <- dados %>% mutate(data_registro = parse_excel_date(data_registro))
if ("nome_municipio" %in% names(dados)) dados <- dados %>% mutate(nome_municipio = str_squish(nome_municipio))

num_cols <- intersect(
  c(
    "dh_mortos", "dh_feridos", "dh_enfermos", "dh_desabrigados", "dh_desalojados",
    "dh_desaparecidos", "dh_afetados_seca_estiagem", "dh_total_danos_humanos_diretos",
    "dh_outros_afetados", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"
  ), names(dados)
)

dados <- dados %>% mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x))))

dados_costeiros <- dados %>%
  inner_join(municipios_regioes, by = "nome_municipio") %>%
  filter(grupo_de_desastre == "Hidrológico") %>%
  mutate(
    ano = year(data_evento),
    mes = month(data_evento),
    mes_nome = factor(mes, levels = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")),
    regiao_costeira = factor(regiao_costeira, levels = c("Região Norte", "Região Centro-Norte", "Região Central", "Região Centro-Sul", "Região Sul"))
  )

# Região: totais
regiao_total <- dados_costeiros %>%
  count(regiao_costeira, sort = TRUE, name = "n_eventos")
write_csv(regiao_total, file.path(base_output, "08_regioes", "01-eventos-por-regiao.csv"))

p_regiao_total <- ggplot(regiao_total %>% arrange(n_eventos), aes(x = n_eventos, y = fct_reorder(regiao_costeira, n_eventos))) +
  geom_col(fill = "steelblue4") +
  geom_text(aes(label = n_eventos), hjust = -0.15, size = 4) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .12))) +
  labs(x = "Quantidade de eventos", y = "Região costeira",
       title = "Eventos hidrológicos por região costeira",
       subtitle = "35 municípios costeiros agrupados em cinco regiões") +
  theme_cientometria(base_size = 15)
save_plot(p_regiao_total, file.path(base_output, "08_regioes", "01-eventos-por-regiao.png"), 14, 8)

# Região por ano
regiao_ano <- dados_costeiros %>%
  count(regiao_costeira, ano, name = "n_eventos") %>%
  filter(!is.na(ano))
write_csv(regiao_ano, file.path(base_output, "08_regioes", "02-eventos-por-regiao-ano.csv"))

p_regiao_ano <- ggplot(regiao_ano, aes(x = ano, y = n_eventos, color = regiao_costeira)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(regiao_ano$ano))) +
  scale_y_continuous(labels = comma) +
  labs(x = "Ano", y = "Quantidade de eventos", color = "Região costeira",
       title = "Série temporal por região costeira",
       subtitle = "Eventos hidrológicos por ano e região") +
  theme_cientometria(base_size = 14, legend = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p_regiao_ano, file.path(base_output, "08_regioes", "02-eventos-por-regiao-ano.png"), 18, 8)

# Região por mês
regiao_mes <- dados_costeiros %>%
  count(regiao_costeira, mes_nome, name = "n_eventos")
write_csv(regiao_mes, file.path(base_output, "08_regioes", "03-eventos-por-regiao-mes.csv"))

p_regiao_mes <- ggplot(regiao_mes, aes(x = mes_nome, y = n_eventos, fill = regiao_costeira)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Mês", y = "Quantidade de eventos", fill = "Região costeira",
       title = "Sazonalidade por região costeira",
       subtitle = "Distribuição mensal dos eventos hidrológicos") +
  theme_cientometria(base_size = 15, legend = "right")
save_plot(p_regiao_mes, file.path(base_output, "08_regioes", "03-eventos-por-regiao-mes.png"), 16, 8)

# Região e tipo de evento
cat_evento <- case_when(
  "atlas_descricao_tipologia" %in% names(dados_costeiros) ~ "atlas_descricao_tipologia",
  "descricao_tipologia" %in% names(dados_costeiros) ~ "descricao_tipologia",
  "tipologia" %in% names(dados_costeiros) ~ "tipologia",
  TRUE ~ NA_character_
)

if (!is.na(cat_evento)) {
  regiao_tipo <- dados_costeiros %>%
    mutate(categoria_evento = .data[[cat_evento]]) %>%
    count(regiao_costeira, categoria_evento, name = "n_eventos")
  write_csv(regiao_tipo, file.path(base_output, "08_regioes", "04-eventos-por-regiao-tipo.csv"))

  p_regiao_tipo <- ggplot(regiao_tipo, aes(x = n_eventos, y = regiao_costeira, fill = categoria_evento)) +
    geom_col() +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .03))) +
    labs(x = "Quantidade de eventos", y = "Região costeira", fill = "Tipo de evento",
         title = "Composição dos eventos por região costeira",
         subtitle = "Totais regionais com divisão por tipo de evento") +
    theme_cientometria(base_size = 15, legend = "right")
  save_plot(p_regiao_tipo, file.path(base_output, "08_regioes", "04-eventos-por-regiao-tipo.png"), 16, 8)
}

# Região e município
municipio_regiao <- dados_costeiros %>%
  count(regiao_costeira, nome_municipio, name = "n_eventos")
write_csv(municipio_regiao, file.path(base_output, "08_regioes", "05-municipios-por-regiao.csv"))

p_municipio_regiao <- ggplot(municipio_regiao, aes(x = n_eventos, y = fct_reorder(nome_municipio, n_eventos), fill = regiao_costeira)) +
  geom_col() +
  facet_wrap(~ regiao_costeira, scales = "free_y") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Quantidade de eventos", y = "Município", fill = "Região costeira",
       title = "Municípios dentro de cada região costeira",
       subtitle = "Distribuição municipal dos eventos hidrológicos por região") +
  theme_cientometria(base_size = 14, legend = "none")
save_plot(p_municipio_regiao, file.path(base_output, "08_regioes", "05-municipios-por-regiao.png"), 18, 12)

cat("Script com análise regional concluído com sucesso.\n")

