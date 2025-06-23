library(tidyverse)
library(here)
library(scales)


datos <- read_rds(here("data/datos_limpios.rds"))

df <- datos |>
  count(tiene_beca) |>
  mutate(
    prop = n/sum(n),
    text = paste0(n," (",percent(prop),")")
  )


g <- ggplot(
  data = df,
  mapping = aes(
    y = n,
    x = tiene_beca,
    fill = tiene_beca,
    label = text
  )
) +
  geom_col() +
  geom_text(
    size = 12/.pt,
    color = "white",
    vjust = 1.5,
    fontface = "bold"
  ) +
  labs(
    y = "Frecuencia (personas)",
    x = "Tenencia de beca socioeconómica",
    title = "Costa Rica: Estudiantes del curso Análisis Exploratorio de\nDatos de la Escuela de Estadística de la Universidad de\nCosta Rica según tenencia de beca socioeconómica,\njunio 2025",
    subtitle = "(Distribución absoluta y porcentual)",
    caption = "Fuente: Encuesta a estudiantes de Análisis Exploratorio de Datos, UCR, 2025" ,
    fill = "¿Cuenta con beca?"
  ) +
  scale_y_continuous(
    breaks = breaks_pretty(n = 5),
    expand = expansion(mult = c(0,0.1))
  ) +
  scale_fill_manual(
    values = c(
      "No" = "#00809D",
      "Sí" = "#9FC87E"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "center",
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
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
  ) +
  coord_cartesian(clip = "off")
g
ggsave("qmd/images/fig-01.jpg", dpi = 300, height = 5.82, width =6.42)
