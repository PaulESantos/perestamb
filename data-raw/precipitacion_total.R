## code to prepare `precipitacion_total` dataset goes here
## 4. Precipitación total anual, según departamento, 2013-2022
# Links
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/4_18.xlsx"
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

precipitacion <-
  datos |>
  dplyr::slice(-c(1:2)) |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),  # Selecciona columnas que empiecen con 'x' y luego 4 dígitos
                      names_to = "año",             # Los nombres se colocarán en la columna 'year'
                      names_prefix = "x",            # Elimina el prefijo 'x' de los años
                      values_to = "precipitacion") |>            # Los valores se guardan en la columna 'temp'
  dplyr::mutate(precipitacion = as.numeric(precipitacion)) |>
  dplyr::arrange(departamento, año) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))
precipitacion

readr::write_csv(precipitacion,
                 "data-raw/precipitacion.csv")
usethis::use_data(precipitacion,
                  compress = "xz",
                  overwrite = T)
