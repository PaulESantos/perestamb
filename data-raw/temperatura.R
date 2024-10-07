## code to prepare `temperatura` dataset goes here
ruta <- "https://www.inei.gob.pe/media/MenuRecursivo/indices_tematicos/1_27.xlsx"

# Crear archivo temporal
ruta_archivo_temporal <- tempfile(fileext = ".xlsx")

# Descargar el archivo temporalmente
try({
  download.file(ruta, ruta_archivo_temporal, mode = "wb")
  message("Descargado temporalmente en: ", ruta_archivo_temporal)
}, silent = TRUE)

# Cargar el archivo descargado para trabajar en R
library(readxl)
datos <- read_excel(ruta_archivo_temporal) |>
  tidyr::drop_na()

colnames(datos) <- janitor::make_clean_names(as.character(datos[1,]))

temperatura <- datos |>
  dplyr::slice(-1) |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(cols = matches("^x[0-9]{4}$"),  # Selecciona columnas que empiecen con 'x' y luego 4 dígitos
                      names_to = "año",             # Los nombres se colocarán en la columna 'year'
                      names_prefix = "x",            # Elimina el prefijo 'x' de los años
                      values_to = "temperatura") |>            # Los valores se guardan en la columna 'temp'
  dplyr::mutate(temperatura = as.numeric(temperatura))|>
  dplyr::mutate_if(is.character,
                   ~iconv(., to = "ASCII//TRANSLIT"))

temperatura
readr::write_csv(temperatura,
                 "data-raw/temperatura.csv")
usethis::use_data(temperatura,
                  compress = "xz",
                  overwrite = T)
