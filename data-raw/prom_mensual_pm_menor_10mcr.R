## code to prepare `prom_mensual_pm_menor_10mcr` dataset goes here
# Titulo
#
# 29. Promedio mensual de partículas inferiores a 10 micras en el aire
# de la provincia de Lima por estación de medición, 2021 y 2022
# Links
url <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/29_4.xlsx"
url
library(readxl)
library(tidyverse)

# Función para descargar el archivo temporalmente
descargar_archivo_temporal <- function(url) {
  # Crear un archivo temporal
  archivo_temporal <- tempfile(fileext = ".xlsx")

  # Descargar el archivo desde la URL
  try({
    download.file(url, archivo_temporal, mode = "wb")
    message("Archivo descargado correctamente.")
  }, silent = TRUE)

  # Comprobar si el archivo existe
  if (file.exists(archivo_temporal)) {
    return(archivo_temporal)
  }
  else {
    stop("Error: no se pudo descargar el archivo.")
  }
}

# Descargar el archivo
ruta_archivo_temporal <- descargar_archivo_temporal(url)

# Leer el archivo Excel si la descarga fue exitosa
if (file.exists(ruta_archivo_temporal)) {
  file.exists(ruta_archivo_temporal)
  datos <- read_excel(ruta_archivo_temporal)
  print("Datos cargados correctamente.")
} else {
  print("No se pudo leer el archivo.")
}

datos

datos <-
  datos |>
  dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

datos

colnames(datos) <- as.character(datos[4,])
datos

prom_mensual_pm_menor_10mcr <- datos |>
  dplyr::slice(-c(1:4)) |>
  dplyr::mutate(año = dplyr::if_else(
    stringr::str_detect(`Año/Mes`, "[0-9]{4}"),
    `Año/Mes`,
    NA_character_
  )) |>
  dplyr::relocate(año) |>
  tidyr::fill(año, .direction = "down") |>
  tidyr::drop_na() |>
  tidyr::pivot_longer(-c(1:2),
                      names_to = "lugar",
                      values_to = "pm25_mgm") |>
  dplyr::mutate(lugar = str_remove( lugar, "\r\n"),
                pm25_mgm = as.numeric(pm25_mgm)) |>
  dplyr::rename(mes = `Año/Mes`) |>
  dplyr::arrange(año, mes) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

prom_mensual_pm_menor_10mcr |>
  dplyr::filter(!is.na(pm25_mgm))

readr::write_csv(prom_mensual_pm_menor_10mcr,
                 "data-raw/prom_mensual_pm_menor_10mcr.csv")
usethis::use_data(prom_mensual_pm_menor_10mcr,
                  compress = "xz",
                  overwrite = T)
