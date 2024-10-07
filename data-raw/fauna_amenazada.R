## code to prepare `fauna_amenazada` dataset goes here
# Titulo
# 25. Especies de fauna silvestre amenazada, según categoría, 2004 y 2014
# xlsx_links[25,1]
# Links
url <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/25_11.xlsx"
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
colnames(datos) <- janitor::make_clean_names(as.character(datos[3,]))
datos

df <- datos |>
  dplyr::rename(categoria = na) |>
  dplyr::slice(-c(1)) |>
  dplyr::filter(categoria != "Total") |>
  dplyr::filter(!is.na(total)) |>
  dplyr::select(-c(na_2))

oldnames <- names(df)
oldnames
new_names <- c("categoria", "total", "anfibios_2004",
               "reptiles_2004", "aves_2004", "mamiferos_2004",
               "total_2", "invertebrados_2014", "anfibios_2014",
               "reptiles_2014", "aves_2014", "mamiferos_2014")

colnames(df) <- new_names

fauna_amenazada <- df |>
  dplyr::slice(-1) |>
  dplyr::select(-c(total, total_2)) |>
  tidyr::pivot_longer(-categoria,
                      names_to = "grupo",
                      values_to = "num_especies") |>
  tidyr::separate_wider_delim(grupo,
                              delim = "_",
                              names = c("grupo", "año")) |>
  dplyr::relocate(categoria, .before = num_especies) |>
  dplyr::mutate(num_especies = as.numeric(num_especies),
                grupo = stringr::str_to_sentence(grupo)) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

fauna_amenazada

readr::write_csv(fauna_amenazada,
                 "data-raw/fauna_amenazada.csv")
usethis::use_data(fauna_amenazada,
                  compress = "xz",
                  overwrite = T)

