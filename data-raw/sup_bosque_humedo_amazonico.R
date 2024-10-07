## code to prepare `sup_bosque_humedo_amazonico` dataset goes here
# Titulo
# 26. Superficie de bosque húmedo amazónico, según departamento, 2013-2021
# xlsx_links[26,1]
# Links
url <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/26_6.xlsx"
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

superficie_bha <- datos |>
  dplyr::slice(-c(1:4)) |>
  dplyr::filter(!is.na(x2013)) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),
                      names_to = "año",
                      names_prefix = "x",
                      values_to = "sup_ha") |>
  dplyr::mutate(sup_ha = as.numeric(sup_ha)) |>
  dplyr::arrange(departamento, año) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))


readr::write_csv(superficie_bha,
                 "data-raw/superficie_bha.csv")
usethis::use_data(superficie_bha,
                  compress = "xz",
                  overwrite = T)
