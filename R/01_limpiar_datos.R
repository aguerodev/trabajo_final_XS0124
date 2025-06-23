library(tidyverse)
library(haven)
library(labelled)
library(naniar) # njtierney/naniar

datos <- read_sav("data/survey_838955_spss.sav") |>
  as_factor() |>
  filter(!is.na(submitdate)) |>
  select(matches("IG\\d+$"), contains("BS"), contains("TR")) |>
  rename(
    # Información General
    carne = IG01,
    sexo = IG02,
    edad = IG03,
    provincia_actual = IG04,

    # Beca Socioeconómica
    tiene_beca = BS01,
    categoria_beca = BS02,
    provincia_origen = BS03,
    provincia_reubicacion = BS04,
    condicion_vivienda = BS05,
    condicion_vivienda_otro = BS05_other,
    rango_alquiler = BS06,
    monto_cubre_necesidades = BS07,
    dinero_adicional = BS09,
    satisfaccion_periodos = BS10,
    acuerdo_periodos = BS11,

    # Transporte
    medio_transporte = TR02,
    transporte_autobus = TR03_1,
    transporte_autobus_inst = TR03_2,
    transporte_tren = TR03_3,
    transporte_taxi = TR03_4,
    transporte_plataforma = TR03_5,
    transporte_otro = TR03_other,
    transporte_privado = TR04,
    gasto_transporte_uni = TR05,
    gasto_transporte_general = TR06
  ) |>
  mutate(
    across(contains("transporte"), \(x) na_if(as.character(x), c("2"))),
    across(contains("transporte"), \(x) na_if(as.character(x), c("")))
  ) |>
  select(-gasto_transporte_general) |>
  write_rds("data/datos_limpios.rds")

# se elimina las variable gasto_transporte_general
# eliminamos transporte general porque encontramos inconsistencias no coincide
# con la variable categórica en no se pregunta lo mismo y tiene valores atípicos

glimpse(datos)

labelled::get_variable_labels(datos)



