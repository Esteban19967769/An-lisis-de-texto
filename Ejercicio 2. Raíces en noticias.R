#Cargar paquetes

#Traer codigo html y buscar etiquetas que nos interesan

install.packages("rvest")
library(rvest)
install.packages("robotstxt")
library(robotstxt)
library(dplyr) #para filtro,pipe,etc
install.packages("stringr")
library(stringr)
library(tidyr)
library(lubridate)
library(purrr)
install.packages("beepr")
library(beepr) #me sirve para reproducir sonido y nos avise
#que el proceso termine de ejecutarse

# vamos a extraer el texto de una noticia
# titular: h1
# bajada: .excerpt
# cuerpo: .paragraph


html_noticia_ejemplo <- read_html("https://www.elmostrador.cl/claves/cambio-climatico")
#traer el codigo html

#vamos a convertir esto en una funcion

extraer_titulares_cc <- function(numero_pagina){
  Sys.sleep(2)
  # crear el enlace
  enlace <- paste0("https://www.elmostrador.cl/claves/cambio-climatico", numero_pagina)
  # extraemos el html
  
  html <- read_html(enlace)
  titulares <- html %>% 
    html_elements("h3 a") %>% 
    html_text(trim = TRUE)
  
  enlaces <- html %>% 
    html_elements("h3 a") %>% 
    html_attr("href") %>% 
    paste0("https://www.elmostrador.cl", .)
  
  tibble(titular = titulares, enlace_noticia = enlaces) %>% 
    separate(titular, into = c("seccion", "titular"), sep = "  ", extra = "merge")
}

map(1:3, extraer_titulares_cc)

map_df(1:3, extraer_titulares_cc)
beep(sound = 8) #suena la cancion de MARIO JAJAAJAJAJAAJ


#opcion1 : manual

titulares_cambio_climatico <- map_df(1:59, extraer_titulares_cc)


# opcion 2: extraer el numero total de paginas desde el sitio

total_paginas <- read_html("https://www.elmostrador.cl/claves/cambio-climatico") %>% 
  html_element(".pagination-info") %>% 
  html_text() %>% 
  str_extract("[:digit:]+$") %>%  # \\d
  as.numeric()

titulares_cambio_climatico <- map_df(1:total_paginas, extraer_titulares_cc)



