# =========================================================
# Análise exploratória - 35 cidades costeiras de SC
# Filtro: grupo_de_desastre == "Hidrológico"
# Base: BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado-_AREA_INTERESSE.xlsx
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(skimr)
library(scales)
library(fs)

# ---------------------------
# Caminhos
# ---------------------------
base_output <- "output/analise_costeira_hidrologico_sc"
path_base <- "dados/Atlas_Digital/Dados_Originais/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado.xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c(
  "01_estrutura", "02_qualidade", "03_temporal", "04_municipios",
  "05_tipos", "06_impactos", "07_relatorio"
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

# ---------------------------
# Municípios costeiros alvo
# ---------------------------
municipios_costeiros <- c(
  "Itapoá", "São Francisco do Sul", "Joinville", "Araquari", "Balneário Barra do Sul",
  "Barra Velha", "Balneário Piçarras", "Penha", "Navegantes", "Itajaí", "Balneário Camboriú",
  "Itapema", "Porto Belo", "Bombinhas", "Governador Celso Ramos", "Biguaçu",
  "São José", "Florianópolis", "Palhoça", "Garopaba", "Imbituba", "Laguna",
  "Jaguaruna", "Balneário Rincão", "Araranguá", "Balneário Gaivota", "Passo de Torres",
  "Garuva", "Tijucas", "Pescaria Brava", "Imaruí", "Balneário Arroio do Silva",
  "Sombrio", "Santa Rosa do Sul", "São João do Sul"
)

# ---------------------------
# Leitura da base
# ---------------------------
dados <- read_excel(path_base, sheet = "Atlas Valores Corrigidos") %>%
  clean_names()

# ---------------------------
# Padronização
# ---------------------------
if ("data_evento" %in% names(dados)) dados <- dados %>% mutate(data_evento = parse_excel_date(data_evento))
if ("data_registro" %in% names(dados)) dados <- dados %>% mutate(data_registro = parse_excel_date(data_registro))

num_cols <- intersect(
  c(
    "dh_mortos", "dh_feridos", "dh_enfermos", "dh_desabrigados", "dh_desalojados",
    "dh_desaparecidos", "dh_afetados_seca_estiagem", "dh_total_danos_humanos_diretos",
    "dh_outros_afetados", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"
  ), names(dados)
)

dados <- dados %>% mutate(across(all_of(num_cols), ~ readr::parse_number(as.character(.x))))

# ---------------------------
# Filtro temático
# ---------------------------
dados_costeiros <- dados %>%
  filter(
    nome_municipio %in% municipios_costeiros,
    grupo_de_desastre == "Hidrológico"
  ) %>%
  mutate(
    ano = year(data_evento),
    mes = month(data_evento),
    mes_nome = factor(mes, levels = 1:12, labels = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"))
  )

# ---------------------------
# Estrutura e qualidade
# ---------------------------
estrutura <- tibble(
  coluna = names(dados_costeiros),
  tipo = map_chr(dados_costeiros, ~ class(.x)[1]),
  faltantes = map_int(dados_costeiros, ~ sum(is.na(.x))),
  pct_faltantes = round(map_dbl(dados_costeiros, ~ mean(is.na(.x)) * 100), 2)
)
write_csv(estrutura, file.path(base_output, "01_estrutura", "01-estrutura.csv"))

qualidade <- tibble(
  indicador = c("linhas", "colunas", "municipios", "tipologias", "anos_min", "anos_max"),
  valor = c(
    nrow(dados_costeiros),
    ncol(dados_costeiros),
    n_distinct(dados_costeiros$nome_municipio),
    if ("descricao_tipologia" %in% names(dados_costeiros)) n_distinct(dados_costeiros$descricao_tipologia) else NA,
    min(dados_costeiros$ano, na.rm = TRUE),
    max(dados_costeiros$ano, na.rm = TRUE)
  )
)
write_csv(qualidade, file.path(base_output, "02_qualidade", "01-qualidade.csv"))

# ---------------------------
# Temporal
# ---------------------------
serie_ano <- dados_costeiros %>%
  count(ano, name = "n_eventos") %>%
  filter(!is.na(ano))
write_csv(serie_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.csv"))

p_ano <- ggplot(serie_ano, aes(x = factor(ano), y = n_eventos)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Ano", y = "Quantidade de eventos",
       title = "Eventos hidrológicos por ano nas cidades costeiras",
       subtitle = "35 municípios costeiros de Santa Catarina") +
  theme_cientometria()
save_plot(p_ano, file.path(base_output, "03_temporal", "01-eventos-por-ano.png"), 16, 8)

serie_mes <- dados_costeiros %>% count(mes_nome, name = "n_eventos")
write_csv(serie_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.csv"))

p_mes <- ggplot(serie_mes, aes(x = mes_nome, y = n_eventos)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = n_eventos), vjust = -0.25, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(x = "Mês", y = "Quantidade de eventos",
       title = "Sazonalidade dos eventos hidrológicos",
       subtitle = "35 municípios costeiros de Santa Catarina") +
  theme_cientometria()
save_plot(p_mes, file.path(base_output, "03_temporal", "02-eventos-por-mes.png"), 16, 8)

cat_evento <- dplyr::case_when(
  "atlas_descricao_tipologia" %in% names(dados_costeiros) ~ "atlas_descricao_tipologia",
  "descricao_tipologia" %in% names(dados_costeiros) ~ "descricao_tipologia",
  "tipologia" %in% names(dados_costeiros) ~ "tipologia",
  TRUE ~ NA_character_
)

if (!is.na(cat_evento)) {
  serie_mes_tipo <- dados_costeiros %>%
    mutate(categoria_evento = .data[[cat_evento]]) %>%
    count(mes_nome, categoria_evento, name = "n_eventos")

  p_mes <- ggplot(serie_mes_tipo, aes(x = mes_nome, y = n_eventos, fill = categoria_evento)) +
    geom_col() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
    labs(
      x = "Mês", y = "Quantidade de eventos", fill = "Tipo de evento",
      title = "Sazonalidade dos eventos hidrológicos",
      subtitle = "35 cidades costeiras de SC | barras empilhadas por tipo"
    ) +
    theme_cientometria(base_size = 15, legend = "right")

  save_plot(p_mes, file.path(base_output, "03_temporal", "03-eventos-por-mes-com-eventos.png"), 16, 8)

  p_mes_pct <- ggplot(serie_mes_tipo, aes(x = mes_nome, y = n_eventos, fill = categoria_evento)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = label_percent()) +
    labs(
      x = "Mês", y = "Participação relativa", fill = "Tipo de evento",
      title = "Composição sazonal dos eventos hidrológicos",
      subtitle = "35 cidades costeiras de SC | proporção mensal por tipo"
    ) +
    theme_cientometria(base_size = 15, legend = "right")

  save_plot(p_mes_pct, file.path(base_output, "03_temporal", "04-eventos-por-mes-percentual.png"), 16, 8)
}

# Escolha da categoria do evento
cat_evento <- dplyr::case_when(
  "atlas_descricao_tipologia" %in% names(dados_costeiros) ~ "atlas_descricao_tipologia",
  "descricao_tipologia" %in% names(dados_costeiros) ~ "descricao_tipologia",
  "tipologia" %in% names(dados_costeiros) ~ "tipologia",
  TRUE ~ NA_character_
)

# Sazonalidade baseada em anos com ocorrência
serie_mes_anos <- dados_costeiros %>%
  filter(!is.na(ano), !is.na(mes_nome)) %>%
  distinct(ano, mes_nome) %>%
  count(mes_nome, name = "n_anos_com_evento")

write_csv(serie_mes_anos, file.path(base_output, "03_temporal", "05-anos-com-ocorrencia-por-mes.csv"))

p_mes_anos <- ggplot(serie_mes_anos, aes(x = mes_nome, y = n_anos_com_evento)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = n_anos_com_evento), vjust = -0.25, size = 3.5) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
  labs(
    x = "Mês",
    y = "Anos com ocorrência",
    title = "Sazonalidade por anos com ocorrência",
    subtitle = "Número de anos em que houve ao menos um evento hidrológico no mês"
  ) +
  theme_cientometria()

save_plot(p_mes_anos, file.path(base_output, "03_temporal", "05-anos-com-ocorrencia-por-mes.png"), 16, 8)

if (!is.na(cat_evento)) {
  serie_mes_tipo_anos <- dados_costeiros %>%
    filter(!is.na(ano), !is.na(mes_nome)) %>%
    mutate(categoria_evento = .data[[cat_evento]]) %>%
    distinct(ano, mes_nome, categoria_evento) %>%
    count(mes_nome, categoria_evento, name = "n_anos_com_evento")

  write_csv(serie_mes_tipo_anos, file.path(base_output, "03_temporal", "06-anos-com-ocorrencia-por-mes-e-tipo.csv"))

  p_mes_tipo_anos <- ggplot(serie_mes_tipo_anos, aes(x = mes_nome, y = n_anos_com_evento, fill = categoria_evento)) +
    geom_col() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .08))) +
    labs(
      x = "Mês",
      y = "Anos com ocorrência",
      fill = "Tipo de evento",
      title = "Sazonalidade por anos com ocorrência e tipo",
      subtitle = "Cada barra mostra em quantos anos cada tipo apareceu no mês"
    ) +
    theme_cientometria(base_size = 15, legend = "right")

  save_plot(p_mes_tipo_anos, file.path(base_output, "03_temporal", "06-anos-com-ocorrencia-por-mes-e-tipo.png"), 16, 8)

  p_mes_tipo_anos_pct <- ggplot(serie_mes_tipo_anos, aes(x = mes_nome, y = n_anos_com_evento, fill = categoria_evento)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = label_percent()) +
    labs(
      x = "Mês",
      y = "Participação relativa",
      fill = "Tipo de evento",
      title = "Composição sazonal por anos com ocorrência",
      subtitle = "Proporção dos tipos entre os anos com ocorrência em cada mês"
    ) +
    theme_cientometria(base_size = 15, legend = "right")

  save_plot(p_mes_tipo_anos_pct, file.path(base_output, "03_temporal", "07-anos-com-ocorrencia-por-mes-e-tipo-percentual.png"), 16, 8)
}

# Lista os anos em que ocorreram eventos em cada mês
# Uso: executar após criar dados_costeiros com as colunas ano e mes_nome

anos_por_mes <- dados_costeiros %>%
  filter(!is.na(ano), !is.na(mes_nome)) %>%
  distinct(mes_nome, ano) %>%
  arrange(mes_nome, ano) %>%
  group_by(mes_nome) %>%
  summarise(
    n_anos = n(),
    anos_ocorrencia = paste(ano, collapse = ", "),
    .groups = "drop"
  )

write_csv(anos_por_mes, file.path(base_output, "03_temporal", "08-anos-de-ocorrencia-por-mes.csv"))

# versão expandida: mês x ano com quantidade de eventos
mes_ano_eventos <- dados_costeiros %>%
  filter(!is.na(ano), !is.na(mes_nome)) %>%
  count(mes_nome, ano, name = "n_eventos") %>%
  arrange(mes_nome, ano)

write_csv(mes_ano_eventos, file.path(base_output, "03_temporal", "09-eventos-por-mes-e-ano.csv"))

# heatmap para localizar rapidamente os anos com ocorrência em cada mês
p_mes_ano <- ggplot(mes_ano_eventos, aes(x = ano, y = mes_nome, fill = n_eventos)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#fff5eb", high = "#d94801") +
  labs(
    x = "Ano",
    y = "Mês",
    fill = "Eventos",
    title = "Ocorrência mensal de eventos por ano",
    subtitle = "Cada célula mostra quantos eventos ocorreram no mês em cada ano"
  ) +
  theme_cientometria(base_size = 14, legend = "right") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_mes_ano, file.path(base_output, "03_temporal", "08-heatmap-mes-ano-eventos.png"), 18, 8)

# ---------------------------
# Municípios
# ---------------------------
municipios_total <- dados_costeiros %>%
  count(nome_municipio, sort = TRUE, name = "n_eventos")
write_csv(municipios_total, file.path(base_output, "04_municipios", "01-municipios-total.csv"))

p_municipios <- ggplot(municipios_total %>% arrange(n_eventos), aes(x = n_eventos, y = fct_reorder(nome_municipio, n_eventos))) +
  geom_col(fill = "seagreen4") +
  geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .12))) +
  labs(x = "Quantidade de eventos", y = "Município",
       title = "Eventos hidrológicos por município",
       subtitle = "35 cidades costeiras de Santa Catarina") +
  theme_cientometria(base_size = 15)
save_plot(p_municipios, file.path(base_output, "04_municipios", "01-municipios-total.png"), 18, 12)

municipios_tipo <- dados_costeiros %>%
  mutate(categoria_evento = case_when(
    !is.na(descricao_tipologia) & descricao_tipologia != "" ~ descricao_tipologia,
    !is.na(tipologia) ~ as.character(tipologia),
    TRUE ~ "Sem informação"
  )) %>%
  count(nome_municipio, categoria_evento, name = "n_eventos") %>%
  group_by(nome_municipio) %>%
  mutate(
    total_municipio = sum(n_eventos),
    perc_no_municipio = 100 * n_eventos / total_municipio
  ) %>%
  ungroup()
write_csv(municipios_tipo, file.path(base_output, "04_municipios", "02-municipios-composicao.csv"))

ordem_municipios <- municipios_total %>% arrange(n_eventos) %>% pull(nome_municipio)
municipios_tipo <- municipios_tipo %>% mutate(nome_municipio = factor(nome_municipio, levels = ordem_municipios))

p_municipios_comp <- ggplot(municipios_tipo, aes(x = n_eventos, y = nome_municipio, fill = categoria_evento)) +
  geom_col() +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .03))) +
  labs(x = "Quantidade de eventos", y = "Município", fill = "Categoria do evento",
       title = "Composição dos eventos hidrológicos por município",
       subtitle = "Totais municipais com divisão por categoria de evento") +
  theme_cientometria(base_size = 15, legend = "right") +
  theme(axis.text.y = element_text(size = 10))
save_plot(p_municipios_comp, file.path(base_output, "04_municipios", "02-municipios-composicao.png"), 18, 12)

p_municipios_pct <- ggplot(municipios_tipo, aes(x = perc_no_municipio, y = nome_municipio, fill = categoria_evento)) +
  geom_col() +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(mult = c(0, .03))) +
  labs(x = "Percentual dentro do município", y = "Município", fill = "Categoria do evento",
       title = "Composição percentual dos eventos hidrológicos por município",
       subtitle = "Participação de cada categoria no total municipal") +
  theme_cientometria(base_size = 15, legend = "right") +
  theme(axis.text.y = element_text(size = 10))
save_plot(p_municipios_pct, file.path(base_output, "04_municipios", "03-municipios-composicao-percentual.png"), 18, 12)

# ---------------------------
# Tipologias / categorias
# ---------------------------
serie_tipos <- dados_costeiros %>%
  mutate(categoria_evento = case_when(
    !is.na(descricao_tipologia) & descricao_tipologia != "" ~ descricao_tipologia,
    !is.na(tipologia) ~ as.character(tipologia),
    TRUE ~ "Sem informação"
  )) %>%
  count(categoria_evento, sort = TRUE, name = "n_eventos")
write_csv(serie_tipos, file.path(base_output, "05_tipos", "01-categorias-evento.csv"))

p_tipos <- ggplot(serie_tipos %>% arrange(n_eventos), aes(x = n_eventos, y = fct_reorder(categoria_evento, n_eventos))) +
  geom_col(fill = "purple4") +
  geom_text(aes(label = n_eventos), hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, .12))) +
  labs(x = "Quantidade de eventos", y = "Categoria do evento",
       title = "Categorias hidrológicas mais frequentes",
       subtitle = "35 cidades costeiras de Santa Catarina") +
  theme_cientometria(base_size = 15)
save_plot(p_tipos, file.path(base_output, "05_tipos", "01-categorias-evento.png"), 16, 8)

# ---------------------------
# Impactos
# ---------------------------
resumo_impactos <- dados_costeiros %>%
  summarise(
    total_danos_humanos = sum(dh_total_danos_humanos_diretos, na.rm = TRUE),
    total_danos_materiais = sum(dm_total_danos_materiais, na.rm = TRUE),
    total_prejuizo_publico = sum(pepl_total_publico, na.rm = TRUE),
    total_prejuizo_privado = sum(pepr_total_privado, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "indicador", values_to = "valor")
write_csv(resumo_impactos, file.path(base_output, "06_impactos", "01-resumo-impactos.csv"))

p_impactos <- ggplot(resumo_impactos, aes(x = indicador, y = valor)) +
  geom_col(fill = "firebrick") +
  geom_text(aes(label = abrv_num()(valor)), vjust = -0.25, size = 3.5) +
  scale_y_continuous(labels = abrv_num(), expand = expansion(mult = c(0, .08))) +
  labs(x = "Indicador", y = "Valor",
       title = "Resumo dos impactos associados aos eventos hidrológicos",
       subtitle = "35 cidades costeiras de Santa Catarina") +
  theme_cientometria(base_size = 15)
save_plot(p_impactos, file.path(base_output, "06_impactos", "01-resumo-impactos.png"), 16, 8)

vars_corr <- intersect(c("dh_total_danos_humanos_diretos", "dm_total_danos_materiais", "pepl_total_publico", "pepr_total_privado"), names(dados_costeiros))
if (length(vars_corr) >= 2) {
  corr <- cor(dados_costeiros[, vars_corr], use = "pairwise.complete.obs")
  corr_df <- as.data.frame(corr) %>% rownames_to_column("variavel")
  write_csv(corr_df, file.path(base_output, "06_impactos", "02-matriz-correlacao.csv"))

  corr_long <- corr_df %>% pivot_longer(-variavel, names_to = "var2", values_to = "cor")
  p_corr <- ggplot(corr_long, aes(x = var2, y = variavel, fill = cor)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", cor)), size = 4) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", limits = c(-1, 1)) +
    labs(x = "Variável", y = "Variável", fill = "Correlação",
         title = "Correlação entre impactos dos eventos hidrológicos",
         subtitle = "35 cidades costeiras de Santa Catarina") +
    theme_cientometria(base_size = 14, legend = "right") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_plot(p_corr, file.path(base_output, "06_impactos", "02-matriz-correlacao.png"), 14, 8)
}

# ---------------------------
# Resumo final
# ---------------------------
resumo <- tibble(
  linhas_filtradas = nrow(dados_costeiros),
  municipios = n_distinct(dados_costeiros$nome_municipio),
  grupo_de_desastre = "Hidrológico"
)
write_csv(resumo, file.path(base_output, "07_relatorio", "00-resumo-geral.csv"))

cat("Script concluído com sucesso.\n")

