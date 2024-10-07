## code to prepare `temperatura_max` dataset goes here
# Titulo
xlsx_links[2,1]
# Links
ruta <- xlsx_links[2,2] |>  as.character()

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

datos <- datos |>
  tidyr::drop_na()

colnames(datos) <- janitor::make_clean_names(as.character(datos[1,]))

temperatura_maxima <- datos |>
  dplyr::slice(-1) |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),  # Selecciona columnas que empiecen con 'x' y luego 4 dígitos
                      names_to = "año",             # Los nombres se colocarán en la columna 'year'
                      names_prefix = "x",            # Elimina el prefijo 'x' de los años
                      values_to = "temperatura_max") |>            # Los valores se guardan en la columna 'temp'
  dplyr::mutate(temperatura_max = as.numeric(temperatura_max)) |>
  dplyr::arrange(departamento, año)

readr::write_csv(temperatura_maxima,
                 "data-raw/temperatura_maxima.csv")
usethis::use_data(temperatura_maxima,
                  compress = "xz",
                  overwrite = T)

