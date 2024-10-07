## code to prepare `acr` dataset goes here
# Titulo
# 21. Áreas de conservación regional, 2021
# xlsx_links[21,1]
# Links
ruta <-  "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/21_8.xlsx"
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

colnames(datos) <- janitor::make_clean_names(as.character(datos[2,]))

acr_sernanp <- datos |>
  dplyr::slice(-c(1:3)) |>
  dplyr::mutate(fecha_de_promulgacion = lubridate::dmy(fecha_de_promulgacion),
                superficie_hectareas = as.numeric(superficie_hectareas)) |>
  dplyr::filter(!is.na(departamento)) |>
  dplyr::arrange(departamento) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

readr::write_csv(acr_sernanp,
                 "data-raw/acr_sernanp.csv")
usethis::use_data(acr_sernanp,
                  compress = "xz",
                  overwrite = T)

