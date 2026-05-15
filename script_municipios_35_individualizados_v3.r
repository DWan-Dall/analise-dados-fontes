# =========================================================
# Análise exploratória - 35 cidades costeiras de SC
# Filtro: grupo_de_desastre == "Hidrológico"
# Geração individual por município com séries completas:
# - 02_series_individuais: série anual por município com composição por cor da tipologia
#   e total anual acima de cada barra
# - 03_heatmaps_individuais: heatmap mês x ano por município
# - 04_heatmaps_tipologia_individuais: heatmap mês x ano por município para cada tipologia
# Obs.: todos os anos e todos os meses aparecem, mesmo com zero evento
# =========================================================

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)
library(fs)

base_output <- "output/analise_costeira_hidrologico_sc_individual"
path_base <- "dados/Atlas_Digital/Dados_Originais/BD_Atlas_1991_2024_v1.0_2025.04.14_Consolidado (2).xlsx"

pasta <- function(x) dir_create(path(base_output, x), recurse = TRUE)
walk(c(
  "01_bases_processadas",
  "02_series_individuais",
  "03_heatmaps_individuais",
  "04_heatmaps_tipologia_individuais",
  "05_relatorio"
), pasta)

theme_cientometria <- function(base_size = 16, legend = "right") {
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

save_plot <- function(p, file, width = 14, height = 8) {
  ggsave(filename = file, plot = p, width = width, height = height, dpi = 300)
}

parse_excel_date <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  out1 <- suppressWarnings(dmy(as.character(x)))
  out2 <- suppressWarnings(ymd(as.character(x)))
  coalesce(out1, out2)
}

cat_evento_col <- function(df) {
  case_when(
    "atlas_descricao_tipologia" %in% names(df) ~ "atlas_descricao_tipologia",
    "descricao_tipologia" %in% names(df) ~ "descricao_tipologia",
    "tipologia" %in% names(df) ~ "tipologia",
    TRUE ~ NA_character_
  )
}

nome_arquivo_seguro <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")
}

municipios_costeiros <- c(
  "Itapoá", "São Francisco do Sul", "Joinville", "Araquari", "Balneário Barra do Sul",
  "Barra Velha", "Balneário Piçarras", "Penha", "Navegantes", "Itajaí", "Balneário Camboriú",
  "Itapema", "Porto Belo", "Bombinhas", "Governador Celso Ramos", "Biguaçu",
  "São José", "Florianópolis", "Palhoça", "Garopaba", "Imbituba", "Laguna",
  "Jaguaruna", "Balneário Rincão", "Araranguá", "Balneário Gaivota", "Passo de Torres",
  "Garuva", "Tijucas", "Pescaria Brava", "Imaruí", "Balneário Arroio do Silva",
  "Sombrio", "Santa Rosa do Sul", "São João do Sul"
)

dados <- read_excel(path_base, sheet = "Atlas Valores Corrigidos") %>%
  clean_names()

if ("data_evento" %in% names(dados)) dados <- dados %>% mutate(data_evento = parse_excel_date(data_evento))
if ("data_registro" %in% names(dados)) dados <- dados %>% mutate(data_registro = parse_excel_date(data_registro))
if ("nome_municipio" %in% names(dados)) dados <- dados %>% mutate(nome_municipio = str_squish(nome_municipio))

dados_costeiros <- dados %>%
  filter(
    nome_municipio %in% municipios_costeiros,
    grupo_de_desastre == "Hidrológico"
  ) %>%
  mutate(
    ano = year(data_evento),
    mes = month(data_evento),
    mes_nome = factor(
      mes,
      levels = 1:12,
      labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    )
  )

col_tipologia <- cat_evento_col(dados_costeiros)

if (!is.na(col_tipologia)) {
  dados_costeiros <- dados_costeiros %>%
    mutate(
      categoria_evento = case_when(
        !is.na(.data[[col_tipologia]]) & as.character(.data[[col_tipologia]]) != "" ~ as.character(.data[[col_tipologia]]),
        TRUE ~ "Sem informação"
      )
    )
} else {
  dados_costeiros <- dados_costeiros %>%
    mutate(categoria_evento = "Sem informação")
}

write_csv(dados_costeiros, file.path(base_output, "01_bases_processadas", "00-dados-costeiros-processados.csv"))

anos_completos <- seq(min(dados_costeiros$ano, na.rm = TRUE), max(dados_costeiros$ano, na.rm = TRUE), by = 1)
meses_niveis <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
tipologias_ordenadas <- dados_costeiros %>% count(categoria_evento, sort = TRUE) %>% pull(categoria_evento)

serie_anual_municipio_tipologia <- dados_costeiros %>%
  filter(!is.na(ano)) %>%
  count(nome_municipio, ano, categoria_evento, name = "n_eventos")

heatmap_municipio_mes_ano <- dados_costeiros %>%
  filter(!is.na(ano), !is.na(mes_nome)) %>%
  count(nome_municipio, mes_nome, ano, name = "n_eventos")

heatmap_municipio_mes_ano_tipologia <- dados_costeiros %>%
  filter(!is.na(ano), !is.na(mes_nome)) %>%
  count(nome_municipio, categoria_evento, mes_nome, ano, name = "n_eventos")

write_csv(serie_anual_municipio_tipologia, file.path(base_output, "01_bases_processadas", "01-serie-anual-municipio-tipologia.csv"))
write_csv(heatmap_municipio_mes_ano, file.path(base_output, "01_bases_processadas", "02-heatmap-municipio-mes-ano.csv"))
write_csv(heatmap_municipio_mes_ano_tipologia, file.path(base_output, "01_bases_processadas", "03-heatmap-municipio-mes-ano-tipologia.csv"))

for (mun in municipios_costeiros) {
  dados_mun <- serie_anual_municipio_tipologia %>%
    filter(nome_municipio == mun) %>%
    complete(ano = anos_completos, categoria_evento = tipologias_ordenadas, fill = list(n_eventos = 0)) %>%
    mutate(nome_municipio = mun)

  totais_ano <- dados_mun %>%
    group_by(ano) %>%
    summarise(total_ano = sum(n_eventos), .groups = "drop")

  arquivo_base <- nome_arquivo_seguro(mun)

  write_csv(
    dados_mun,
    file.path(base_output, "02_series_individuais", paste0(arquivo_base, "-serie-anual-tipologia.csv"))
  )

  write_csv(
    totais_ano,
    file.path(base_output, "02_series_individuais", paste0(arquivo_base, "-serie-anual-totais.csv"))
  )

  limite_superior <- max(totais_ano$total_ano, na.rm = TRUE)
  margem_rotulo <- ifelse(limite_superior == 0, 0.3, max(0.3, limite_superior * 0.04))

  p <- ggplot(dados_mun, aes(x = factor(ano, levels = anos_completos), y = n_eventos, fill = categoria_evento)) +
    geom_col() +
    geom_text(
      data = totais_ano,
      aes(x = factor(ano, levels = anos_completos), y = total_ano, label = total_ano),
      inherit.aes = FALSE,
      vjust = -0.35,
      size = 3.8
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, .12))
    ) +
    coord_cartesian(ylim = c(0, limite_superior + margem_rotulo)) +
    labs(
      x = "Ano",
      y = "Quantidade de eventos",
      fill = "Tipologia",
      title = paste("Série anual de eventos hidrológicos -", mun),
      subtitle = "Composição da quantidade de eventos por tipologia em cada ano"
    ) +
    theme_cientometria(base_size = 15, legend = "right") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))

  save_plot(
    p,
    file.path(base_output, "02_series_individuais", paste0(arquivo_base, "-serie-anual-tipologia.png")),
    width = 16,
    height = 8
  )
}

for (mun in municipios_costeiros) {
  dados_mun <- heatmap_municipio_mes_ano %>%
    filter(nome_municipio == mun) %>%
    mutate(mes_nome = as.character(mes_nome)) %>%
    complete(ano = anos_completos, mes_nome = meses_niveis, fill = list(n_eventos = 0)) %>%
    mutate(
      nome_municipio = mun,
      mes_nome = factor(mes_nome, levels = meses_niveis)
    )

  arquivo_base <- nome_arquivo_seguro(mun)

  write_csv(
    dados_mun,
    file.path(base_output, "03_heatmaps_individuais", paste0(arquivo_base, "-heatmap-mes-ano.csv"))
  )

  p <- ggplot(
    dados_mun,
    aes(x = factor(ano, levels = anos_completos), y = mes_nome, fill = n_eventos)
  ) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "#d94801", limits = c(0, max(dados_mun$n_eventos, na.rm = TRUE))) +
    labs(
      x = "Ano",
      y = "Mês",
      fill = "Eventos",
      title = paste("Heatmap de eventos hidrológicos -", mun),
      subtitle = "Distribuição temporal mensal (mês x ano)"
    ) +
    theme_cientometria(base_size = 15, legend = "right") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 8),
      axis.text.y = element_text(size = 10)
    )

  save_plot(
    p,
    file.path(base_output, "03_heatmaps_individuais", paste0(arquivo_base, "-heatmap-mes-ano.png")),
    width = 16,
    height = 8
  )
}

for (mun in municipios_costeiros) {
  dados_mun <- heatmap_municipio_mes_ano_tipologia %>%
    filter(nome_municipio == mun)

  arquivo_base_mun <- nome_arquivo_seguro(mun)
  dir_create(file.path(base_output, "04_heatmaps_tipologia_individuais", arquivo_base_mun), recurse = TRUE)

  if (nrow(dados_mun) > 0) {
    write_csv(
      dados_mun,
      file.path(base_output, "04_heatmaps_tipologia_individuais", arquivo_base_mun, paste0(arquivo_base_mun, "-heatmap-mes-ano-tipologias-observadas.csv"))
    )
  }

  for (tipo in tipologias_ordenadas) {
    dados_mun_tipo <- heatmap_municipio_mes_ano_tipologia %>%
      filter(nome_municipio == mun, categoria_evento == tipo) %>%
      mutate(mes_nome = as.character(mes_nome)) %>%
      complete(ano = anos_completos, mes_nome = meses_niveis, fill = list(n_eventos = 0)) %>%
      mutate(
        nome_municipio = mun,
        categoria_evento = tipo,
        mes_nome = factor(mes_nome, levels = meses_niveis)
      )

    arquivo_tipo <- nome_arquivo_seguro(tipo)

    write_csv(
      dados_mun_tipo,
      file.path(base_output, "04_heatmaps_tipologia_individuais", arquivo_base_mun, paste0(arquivo_base_mun, "-", arquivo_tipo, "-heatmap.csv"))
    )

    p <- ggplot(
      dados_mun_tipo,
      aes(x = factor(ano, levels = anos_completos), y = mes_nome, fill = n_eventos)
    ) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "#08519c", limits = c(0, max(dados_mun_tipo$n_eventos, na.rm = TRUE))) +
      labs(
        x = "Ano",
        y = "Mês",
        fill = "Eventos",
        title = paste("Heatmap por tipologia -", mun),
        subtitle = paste("Tipologia:", tipo)
      ) +
      theme_cientometria(base_size = 15, legend = "right") +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 10)
      )

    save_plot(
      p,
      file.path(base_output, "04_heatmaps_tipologia_individuais", arquivo_base_mun, paste0(arquivo_base_mun, "-", arquivo_tipo, "-heatmap.png")),
      width = 16,
      height = 8
    )
  }
}

resumo <- tibble(
  linhas_filtradas = nrow(dados_costeiros),
  municipios = n_distinct(dados_costeiros$nome_municipio),
  tipologias = n_distinct(dados_costeiros$categoria_evento),
  ano_min = min(dados_costeiros$ano, na.rm = TRUE),
  ano_max = max(dados_costeiros$ano, na.rm = TRUE),
  grupo_de_desastre = "Hidrológico"
)

write_csv(resumo, file.path(base_output, "05_relatorio", "00-resumo-geral.csv"))

cat("Script individualizado v3 concluído com sucesso.\n")
