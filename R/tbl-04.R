library(gt)
library(tidyverse)
library(systemfonts)
library(here)

datos <- read_rds(here("data/datos_limpios.rds"))

df <- datos |>
  count(rango_alquiler) |>
  drop_na()|>
  mutate(
    prop = n/sum(n)
  )
df
tabla <- df |>
  mutate(prop = prop) |>                      # convertir a porcentaje
  gt(rowname_col = "rango_alquiler") |>
  cols_label(
    n    = "Frecuencia (personas)",
    prop = "Porcentaje"
  ) |>
  tab_stubhead(label = md("**Rango de alquiler**")) |>
  tab_header(
    title = md("**Costa Rica: Estudiantes del curso Análisis Exploratorio de Datos de la Universidad de Costa Rica según rango de alquiler, junio 2025**"),
    subtitle = md("(absolutos y porcentuales)")
  ) |>
  # alineación
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(columns = where(is.numeric))) |>
  tab_style(
    style      = cell_text(align = "left"),
    locations  = cells_stubhead()
  ) |>
  tab_style(
    style      = cell_text(align = "left"),
    locations  = cells_stub()
  ) |>

  # fuente
  tab_source_note(
    md("Fuente: Encuesta a estudiantes de Análisis Exploratorio de Datos, UCR, 2025")
  ) |>
  # formato numérico
  fmt_number(columns = n,    decimals = 0, sep_mark = " ") |>
  fmt_percent(columns = prop, decimals = 1, sep_mark = " ") |>
  # estilos generales (idénticos al cuadro 1)
  # Opciones generales de formato y estilo (se conservan del original)
  opt_table_font(font = "ChollaSans") |>
  tab_options(
    heading.title.font.size        = px(18),
    heading.subtitle.font.size     = px(16),
    column_labels.font.size        = px(18),
    stub.font.size                 = px(18),
    table.font.size                = px(18),
    table.border.left.width        = px(0),
    table.border.right.width       = px(0),
    table.border.top.width         = px(0),
    table.border.bottom.width      = px(0),
    column_labels.border.top.width = px(0),
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    heading.border.bottom.width    = px(0),
    column_labels.background.color = "#27548A",
    column_labels.padding          = px(10),
    data_row.padding.horizontal    = px(10),
    stub.border.width              = px(0),
    table_body.hlines.width        = px(4),
    table_body.hlines.color        = "white",
    table_body.border.top.color    = "black",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(1),
    footnotes.spec_ref             = "^",
    footnotes.spec_ftr             = " ",
    footnotes.font.size     = px(14),   # tamaño de las notas al pie
    source_notes.font.size  = px(14)    # tamaño de la fuente
  ) |>
  sub_missing(missing_text = "ND") |>
  cols_width(
    rango_alquiler ~ px(150),
    n              ~ px(200),
    prop           ~ px(150)
  )|>
  # Para alinear source notes a la izquierda
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_source_notes()
  ) |>
  # Para alinear footnotes a la izquierda
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_footnotes()
  )

tabla |>
  gtsave("qmd/images/tbl-04.png",
         zoom   = 3,
         vwidth = 2200,
         vheight = 0,   # alto automático
         expand = 10)
