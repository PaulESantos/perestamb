## code to prepare `get_xlsx_link` dataset goes here
library(rvest)
library(tidyverse)

# Función para extraer los links de descarga de archivos .xlsx desde una URL
get_xlsx_links <- function(url) {

  # Leer el HTML de la página
  site <- rvest::read_html(url)

  # Extraer todos los enlaces (etiquetas <a>)
  nodes <- site |>
    rvest::html_nodes("a")

  # Extraer el texto visible y los enlaces
  links_info <- tibble::tibble(
    titulo = nodes |>  rvest::html_text(),    # Extraer el texto visible en el enlace (nombre de la tabla)
    link = nodes |>  rvest::html_attr("href") # Extraer el atributo href (el enlace de descarga)
  )

  # Filtrar solo las filas que contienen enlaces a archivos .xlsx
  xlsx_info <- links_info |>
    dplyr::filter(stringr::str_detect(link, "\\.xlsx$")) |>
    # Añadir la URL base a los enlaces relativos (si es necesario)
    dplyr::mutate(link = rvest::url_absolute(link, url)) |>
    dplyr::mutate(titulo = stringr::str_squish(titulo)) # Limpiar los espacios en blanco

  return(xlsx_info)
}

# Llamar a la función con la URL del INEI
path <- "https://www.inei.gob.pe/estadisticas/indice-tematico/medio-ambiente/"
xlsx_links <- get_xlsx_links(path)
# readr::write_csv(xlsx_links,
#                 "data-raw/inei_est_ambi.csv")
# usethis::use_data(xlsx_links,
#                  compress = "xz",
#                  overwrite = TRUE)
