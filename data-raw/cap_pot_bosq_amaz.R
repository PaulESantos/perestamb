## code to prepare `cap_pot_bosq_amaz` dataset goes here
# Titulo
# 27. Capacidad potencial de los bosques amazónicos, 2015-2021
# xlsx_links[27,1]
# Links
url <- xlsx_links[27,2] |>
  as.ch"https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/27_5.xlsx"
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
colnames(datos) <- janitor::make_clean_names(as.character(datos[2,]))
datos

cap_pot_bosq_amaz <- datos |>
  dplyr::slice(-c(1:3)) |>
  dplyr::filter(!is.na(x2013)) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),
                      names_to = "año",
                      names_prefix = "x",
                      values_to = "sup_ha") |>
  dplyr::mutate(sup_ha = as.numeric(sup_ha)) |>
  dplyr::arrange(año) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))
cap_pot_bosq_amaz


readr::write_csv(cap_pot_bosq_amaz,
                 "data-raw/cap_pot_bosq_amaz.csv")
usethis::use_data(cap_pot_bosq_amaz,
                  compress = "xz",
                  overwrite = T)
