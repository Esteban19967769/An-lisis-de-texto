
# voy a crear una carpeta nueva
#dir.create("figuras")

# cargar nuestro paquetes

library(readtext) 
library(syuzhet)  
library(tidytext)
library(stringr)  #manejar caracteres
library(dplyr)
library(tidyr) #ordenar datos
library(ggplot2)
library(readr)

# leer el archivo

boric <- scan("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/datos/programas_2021/2021_boric_pacto-apruebo-dignidad_primera-vuelta.txt", what = "char") %>%  
  paste(collapse = " ",                                                                                                                                                                    
                      encoding = "UTF-8") # enconding = "UTF-8"
#scan es para importar un objeto de texto, what es para leer caracteres, colapse es para unir espacios

sentimientos_boric <- get_nrc_sentiment(boric, language = "spanish")
#nrc es un diccionario de sentimientos
sentimientos_boric
#me calcula las palabras segun furia, miedo, tristeza, etc

sentimientos_boric_larga <- sentimientos_boric %>% 
  pivot_longer(anger:positive, names_to = "sentimiento", values_to = "frecuencia")
#pivot_longer es para transponer como tabla los sentimientos desde furia hasta positivo
sentimientos_boric_larga
#me genera tablita


"grafico de barras de la cantidad de palabras por sentimiento"
sentimientos_boric_larga %>% 
  filter(!sentimiento %in% c("")) %>%  #quiero lo que no este en la columna de sentimiento relacionado a los sentimientos negativo y positivo
  ggplot(aes(reorder(sentimiento, -frecuencia), frecuencia)) +  #-frecuencia es para mayor a menor
  geom_col(fill = "turquoise4") +
  theme_minimal()+
  labs(title = "Grafico de barras de la cantidad de palabras por sentimiento",
       y = "Frecuencias",
       X = "Sentimientos") 

#Guardo el objeto en las figuras
ggsave("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/figuras/columnas_sentimientos_boric.png", height = 7, width = 10)


#Grafico de linea de polaridad a lo largo del texto, que tan negativo a positivo se vuelve el texto
polaridad_boric <- boric %>% 
  get_sentences() %>% #divido el texto en oraciones
  get_sentiment(method = "nrc", language = "spanish")

#promedio de polaridad
head(polaridad_boric, n  = 20)


tibble(indice = 1:length(polaridad_boric), polaridad = polaridad_boric) %>% 
  ggplot(aes(indice, polaridad)) +
  geom_line()
#transformaremos el grafico para tener algo mas representativo, debemos transformar los datos:

polaridad_boric <- get_dct_transform(polaridad_boric)

tibble(indice = 1:length(polaridad_boric), polaridad = polaridad_boric) %>% 
  ggplot(aes(indice, polaridad)) +
  geom_line() +
  labs(title = "Grafico de línea acerca de la polaridad")

#El programa de boric comienza con "negativismo" para luego terminar con esperanza y "alegría"

#Guardo el objeto en las figuras
ggsave("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/figuras/polaridad_sentimientos_boric.png", height = 7, width = 10)


