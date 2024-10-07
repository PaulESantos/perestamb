## code to prepare `rios_frontera` dataset goes here
# Titulo
# 11. Longitud aproximada de ríos de las fronteras internacionales
# Links
# "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/11_11.xlsx"
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/11_11.xlsx"
ruta
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

# URL de ejemplo
url <- ruta
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

colnames(datos) <- janitor::make_clean_names(as.character(datos[1,]))

fuentes <- datos |>
  dplyr::slice(39:44) |>
  dplyr::select(fuente = pais_fronterizo_nombre_del_rio) |>
  dplyr::mutate(idfuente = stringr::str_extract(fuente,
                                                "[0-9]{1}/") |>
                  stringr::str_remove("/")) |>
  dplyr::mutate(fuente = stringr::str_remove(fuente,
                                             "^[0-9]{1}/ "))


df_1 <- datos |>
  dplyr::slice(-c(1, 38:44)) |>
  dplyr::mutate(pais_fronterizo = dplyr::if_else(
    is.na(longitud_km),
    pais_fronterizo_nombre_del_rio, NA_character_
  )) |>
  tidyr::fill(pais_fronterizo, .direction = "down") |>
  dplyr::filter(!is.na(longitud_km)) |>
  dplyr::rename(nombre_del_rio = pais_fronterizo_nombre_del_rio) |>
  dplyr::relocate(pais_fronterizo) |>
  dplyr::mutate(idfuente = stringr::str_extract(pais_fronterizo,
                                                "[0-9]{1}/") |>
                  stringr::str_remove("/")) |>
  dplyr::mutate(pais_fronterizo = stringr::str_remove(pais_fronterizo,
                                                      " [0-9]{1}/"))


rios_frontera <- df_1 |>
  dplyr::left_join(fuentes) |>
  dplyr::mutate(longitud_km = as.numeric(longitud_km)) |>
  dplyr::select(-idfuente) |>
  dplyr::mutate(dplyr::across(c(pais_fronterizo,
                                nombre_del_rio),
                              ~iconv(., to = "ASCII//TRANSLIT")))
rios_frontera
readr::write_csv(rios_frontera,
                 "data-raw/rios_frontera.csv")
usethis::use_data(rios_frontera,
                  compress = "xz",
                  overwrite = T)
