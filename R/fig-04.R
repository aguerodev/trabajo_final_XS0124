library(tidyverse)
library(here)
library(scales)


datos <- read_rds(here("data/datos_limpios.rds"))

df <- datos |>
  select(transporte_autobus:transporte_otro) |>
  pivot_longer(cols = everything()) |>
  drop_na() |>
  mutate(
    name = case_match(
      name,
      "transporte_autobus"      ~ "Autobús público",
      "transporte_autobus_inst" ~ "Autobús institucional",
      "transporte_otro"         ~ "Otro",
      "transporte_plataforma"   ~ "Plataformas digitales",
      "transporte_tren"         ~ "Tren",
      .default = name
    )
  ) |>
  count(name) |>
  mutate(
    prop = n/sum(n),
    text = paste0(n," (",scales::percent(prop, accuracy = 0.1),")"),
    name = fct_reorder(name,n)
  ) |>
  arrange(desc(name))

df

g <- ggplot(
  data = df,
  mapping = aes(
    y = name,
    x = n,
    label = text
  )
) +
  geom_col() +
  geom_text(
    size = 12/.pt,
    color = "black",
    hjust = -0.1
  ) +
  labs(
    x = "Frecuencia (personas)",
    y = "Medio de transporte",
    title = "Costa Rica. Estudiantes del curso Análisis Exploratorio\nde Datos de la Universidad de Costa Rica según medio\nde transporte al campus, junio 2025",
    subtitle = "(Distribución absoluta y porcentual)",
    caption = "Fuente: Encuesta a estudiantes de Análisis Exploratorio de Datos, UCR, 2025" ,
    fill = "¿Cuenta con beca?"
  ) +
  scale_x_continuous(
    breaks = breaks_pretty(n = 5),
    expand = expansion(mult = c(0,0.3))
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
ggsave("qmd/images/fig-04.jpg", dpi = 300, height = 5.82, width = 7.65)
