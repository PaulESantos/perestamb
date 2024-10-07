## code to prepare `acp_sernanp` dataset goes here
# Titulo
#  22. Áreas de conservación privada, 2022
# xlsx_links[22,1]

# Links
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/22_4.xlsx"
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
datos

acp_sernanp <- datos |>
  dplyr::slice(-c(1:2)) |>
  dplyr::select(!dplyr::starts_with("na")) |>
  dplyr::filter(area_de_conservacion != "Total") |>
  dplyr::filter(!is.na(departamento)) |>
  dplyr::filter(area_de_conservacion != "Área de conservación" ) |>
  dplyr::mutate(base_legal = stringr::str_squish(base_legal) |>
                  stringr::str_trim()) |>
  dplyr::mutate(year = dplyr::if_else(!stringr::str_detect(ano_de_promulgacion, "\r"),
                                      ano_de_promulgacion,
                                      "99999")) |>
  dplyr::mutate(fecha = janitor::excel_numeric_to_date(as.numeric(year))) |>
  tidyr::separate_rows(base_legal, sep = ";") |>
  dplyr::mutate(base_legal = stringr::str_squish(base_legal) |>
                  stringr::str_trim()) |>
  tidyr::separate_rows(ano_de_promulgacion, sep = ";\r\n") |>
  dplyr::mutate(fecha = dplyr::case_when(
    year == "99999" ~ lubridate::dmy(ano_de_promulgacion),
    TRUE ~ fecha
  )) |>
  dplyr::rename(fecha_promulgacion = fecha) |>
  dplyr::rename(sup_ha = superficie_hectareas) |>
  dplyr::mutate(sup_ha = as.numeric(sup_ha)) |>
  dplyr::mutate(sup_ha =  dplyr::case_when(
    area_de_conservacion == "Campo Verde" ~ 8049.87,
    area_de_conservacion == "Nihii Eupa Francisco" ~ 2103.75,
    area_de_conservacion == "Suttoc y Pacchac" ~  1808.75,
    area_de_conservacion == "Misquiyaco" ~ 1797.91,
    area_de_conservacion == "Comunidad Nativa Once de Agosto Río Ucayali" ~  1122.47,
    area_de_conservacion == "Lomas de Quebrada Río Seco" ~ 787.82,
    area_de_conservacion == "Bosque Urum" ~ 705.95,
    area_de_conservacion == "Paraje Capiro Llaylla" ~ 350.18,
    area_de_conservacion == "San Lorenzo" ~  191.14 ,
    area_de_conservacion == "Tambopata Eco Lodge I" ~ 184.81,
    area_de_conservacion == "Los Amigos" ~ 140.35,
    area_de_conservacion == "Juningue" ~ 65.56,
    area_de_conservacion == "Predio Collpapampa (Huadquiña-Mesada Chico)" ~ 43.00,
    area_de_conservacion == "El Bosque Encantado de Sho'llet" ~ 20.88,
    area_de_conservacion == "Pablito II" ~ 12.52,
    area_de_conservacion == "Fundo Miguel I" ~  6.66,
    TRUE ~ sup_ha
  )) |>
  dplyr::select(-c(ano_de_promulgacion, year)) |>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

acp_sernanp

readr::write_csv(acp_sernanp,
                 "data-raw/acp_sernanpl.csv")
usethis::use_data(acp_sernanp,
                  compress = "xz",
                  overwrite = T)
