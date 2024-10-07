## code to prepare `sup_dep_sup_bha` dataset goes here
# Titulo
# 28. Superficie departamental y superficie de bosque húmedo amazónico,
# según departamento, 2020-2021
#
# as.character(xlsx_links[28,1])
#
# Links
url <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/28_5.xlsx"
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

oldnames <- janitor::make_clean_names(as.character(datos[4,]))
oldnames
colnames(datos) <- c("departamento",
                     "sup_departamento_2020",
                     "sup_bha_2020",
                     "porc_sup_bha_2020",
                     "na",
                     "sup_departamento_2021",
                     "sup_bha_2021",
                     "porc_sup_bha_2021"
)

sup_dep_sup_bha <-
datos |>
  dplyr::slice(-c(1:5)) |>
  dplyr::filter(!is.na(sup_bha_2020)) |>
  dplyr::select(-c(na, sup_departamento_2021)) |>
  dplyr::rename(sup_departamento = sup_departamento_2020) |>
  dplyr::select(c(departamento, sup_departamento,
                  sup_bha_2020, sup_bha_2021)) |>
  tidyr::pivot_longer(-c(departamento, sup_departamento),
                      names_to = "año",
                      values_to = "sup_ha") |>
  dplyr::mutate(año = stringr::str_extract(año, "[0-9]{4}$")) |>
  dplyr::mutate(dplyr::across(c(sup_departamento, año, sup_ha),
                              ~as.numeric(.))) |>
  dplyr::relocate(año) |>
  dplyr::mutate(porc_sup_bha = (sup_ha/sup_departamento)*100) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

sup_dep_sup_bha
readr::write_csv(sup_dep_sup_bha,
                 "data-raw/sup_dep_sup_bha.csv")
usethis::use_data(sup_dep_sup_bha,
                  compress = "xz",
                  overwrite = T)
