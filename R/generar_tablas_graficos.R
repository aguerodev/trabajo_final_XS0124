library(here)
library(purrr)

source <- quietly(source)
files <- list.files(path = here("R"), pattern = "(tbl|fig)_*", full.names = TRUE)

walk(files, \(x){
  cli::cli_inform("Generando...{ basename(x)}",)
  source(x)
})
