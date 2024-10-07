## code to prepare `humedad_relativa` dataset goes here
# Titulo
# 6. Humedad relativa promedio anual, según departamento, 2013-2022
# Links
#
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/6_18.xlsx"
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


datos <-
  datos |>
  dplyr::filter(!dplyr::if_all(dplyr::everything(), is.na))

colnames(datos) <- janitor::make_clean_names(as.character(datos[2,]))
datos

dep_names <- c("Amazonas",
               "Apurímac",
               "Arequipa",
               "Ayacucho",
               "Cajamarca",
               "Cusco",
               "Huancavelica",
               "Huánuco",
               "Ica",
               "Junín",
               "La Libertad",
               "Lambayeque",
               "Lima",
               "Loreto",
               "Madre de Dios",
               "Moquegua",
               "Pasco",
               "Piura",
               "Puno",
               "San Martín",
               "Tacna",
               "Tumbes",
               "Ucayali",
               "Áncash" )


humedad_rel <-  datos |>
  dplyr::slice(-c(1:2)) |>
  dplyr::filter(departamento %in% dep_names) |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),
                      names_to = "año",
                      names_prefix = "x",
                      values_to = "hum_rel") |>
  dplyr::mutate(hum_rel = as.numeric(hum_rel)) |>
  dplyr::arrange(departamento, año) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))
humedad_rel

readr::write_csv(humedad_rel,
                 "data-raw/humedad_rel.csv")
usethis::use_data(humedad_rel,
                  compress = "xz",
                  overwrite = T)

