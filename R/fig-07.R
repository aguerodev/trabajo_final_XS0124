library(tidyverse)
library(here)

datos <- read_rds(here("data/datos_limpios.rds"))

# Crear tabla simple de transporte por beca
df <- datos |>
  mutate(
    medio = case_when(
      transporte_autobus == "Sí" |
        transporte_autobus_inst == "Sí" |
        transporte_tren == "Sí" ~ "Transporte público",
      transporte_taxi == "Sí" |
        transporte_plataforma == "Sí" ~ "Transporte privado",
      !is.na(medio_transporte) ~ medio_transporte,
      TRUE ~ "Camina"
    )
  ) |>
  count(tiene_beca, medio) |>
  mutate(prop = n/sum(n), .by = medio)

df
g <- ggplot(
  data = df,
  mapping = aes(
    y = medio,
    x = prop,
    fill = tiene_beca,
    label = scales::percent(prop, accuracy = 1)
  )
) +
  geom_col(
    position = position_fill(vjust = 0.5)
  ) +
  geom_text(
    position = position_fill(vjust = 0.5),
    size = 12/.pt,
    color = "white",
    fontface = "bold"
  ) +
   labs(
     title = "Costa Rica: Estudiantes del curso Análisis Exploratorio de\nDatos de la Universidad de Costa Rica según medio de\ntransporte utilizado por condición de beca socioeconómica,\njunio 2025",
     subtitle = "(Distribución porcentual por condición de beca)",
     x = "Porcentaje de estudiantes según condición de beca",
     y = "Medio de transporte",
     fill = "¿Cuenta con beca?",
     caption = "Fuente: Encuesta a estudiantes de Análisis Exploratorio de Datos, UCR, 2025"
   ) +
   scale_fill_manual(
     values = c(
       "No" = "#00809D",
       "Sí" = "#9FC87E"
     )
   ) +
  scale_x_continuous(
    labels = percent_format()
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(
      hjust = 0,
      size = 10,
      color = "#3F4F44"),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.title.x = element_text(
      margin = margin(t = 10, b = 10)
    ),
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 12
    ),
    plot.margin = margin(10,10,10,10),
    legend.title = element_text(
      size = 10
    )
  )
g
ggsave("qmd/images/fig-07.jpg", dpi = 300, height = 5.82, width = 7.65)
