## code to prepare `flora_fauna_2013_2020` dataset goes here
# Titulo
#  23. Especies de fauna y flora existentes en Perú, 2013-2020
# xlsx_links[23,1]
# Links
url <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/23_5.xlsx"
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

flora_fauna_2013_2020 <-
  datos |>
  dplyr::slice(-c(1:2)) |>
  tidyr::drop_na() |>
  dplyr::filter(categoria != "Total") |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),  # Selecciona columnas que empiecen con 'x' y luego 4 dígitos
                      names_to = "año",             # Los nombres se colocarán en la columna 'year'
                      names_prefix = "x",            # Elimina el prefijo 'x' de los años
                      values_to = "num_especies") |>            # Los valores se guardan en la columna 'temp'
  dplyr::mutate(num_especies = as.numeric(num_especies)) |>
  dplyr::arrange(categoria, año) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))
flora_fauna_2013_2020

readr::write_csv(flora_fauna_2013_2020 ,
                 "data-raw/flora_fauna_2013_2020 .csv")
usethis::use_data(flora_fauna_2013_2020 ,
                  compress = "xz",
                  overwrite = T)

