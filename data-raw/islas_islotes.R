## code to prepare `islas_islotes` dataset goes here
# Titulo
# 20. Reserva Nacional Sistema de islas, islotes y puntas guaneras, 2010
# xlsx_links[20,1]
#
# Links
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/20_9.xlsx"
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

colnames(datos) <- janitor::make_clean_names(as.character(datos[2,]))

islas_islotes <- datos |>
  dplyr::slice(-c(1:2)) |>
  rename(nombre = sistema_de_islas_islotes_y_puntas_guaneras) |>
  filter(nombre != "Total") |>
  mutate(sistema = dplyr::if_else(
    is.na(ubicacion),
    nombre,
    NA_character_
  )) |>
  tidyr::fill(sistema, .direction = "down") |>
  dplyr::filter(!is.na(ubicacion)) |>
  dplyr::select(-na) |>
  dplyr::relocate(sistema) |>
  dplyr::mutate(superficie = as.numeric(superficie)) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

islas_islotes
readr::write_csv(islas_islotes,
                 "data-raw/islas_islotes.csv")
usethis::use_data(islas_islotes,
                  compress = "xz",
                  overwrite = T)
