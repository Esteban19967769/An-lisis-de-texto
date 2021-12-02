# cargar nuestro paquetes

library(readtext) 
library(syuzhet)
library(tidytext)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# EJERCICIO 2: TF-IDF (term frequency * inverse document frequency) ----
"consiste en tratar de determinar el peso de cada palabra en cada documento
simplemente destacamos las palabras mas relevantes cuando la comparo con un grupo determinado de textos"


mensajes <- readtext("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/datos/programas_2021/",
                     docvarsfrom = "filenames",
                     docvarnames = c("fecha", "candidato", "coalicion","primera-vuelta"), #me crea variables a partir de la info del documentos
                     dvsep = "_",
                     encoding = "UTF-8")
head(mensajes)

# importar una lista de stopwords

unas_stopwords <- read_csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt")
#Agregar unas palabras a las stopwords que no tienen mucha relevancia
otras_stopwords <- tibble(palabra = c("ción","nes","com","https"))

# tokenizar

mensajes %>% 
  unnest_tokens(input = text, output = palabra, strip_numeric = TRUE,  collapse = "candidato") %>% #mantenemos al candidato con cada palabra asociada
  count(candidato, palabra, sort = TRUE) %>% #si ejecuto este codico hasta aca, me aparecen string con numeros raros y los quiero eliminar, aplico un filter
  filter(!str_detect(palabra, "^\\d"))  #eliminar str que elimine palabras que comiencen con numero
  
#SI ejecuto este código, me aparecen las stopwords que quiero eliminar y que se llevan la mayor cantidad de palabras

frecuencias_mensajes <- mensajes %>% 
  unnest_tokens(input = text, output = palabra, strip_numeric = TRUE,  collapse = "candidato") %>% #mantenemos al candidato con cada palabra asociada
  count(candidato, palabra, sort = TRUE) %>% #si ejecuto este codico hasta aca, me aparecen string con numeros raros y los quiero eliminar, aplico un filter
  filter(!str_detect(palabra, "^\\d"))  %>% #eliminar str que elimine palabras que comiencen con numero
  anti_join(unas_stopwords) %>% 
  anti_join(otras_stopwords) 


frecuencias_mensajes
#Ejecutando este código anterior, me entregan las palabras sin stopwords, donde estado, chile y desarrollo son las top3


tf_idf_mensajes <- bind_tf_idf(frecuencias_mensajes,
                               term = palabra,
                               document = candidato,
                               n = n)
tf_idf_mensajes

#ordenamos idf
tf_idf_mensajes %>% 
  arrange(desc(tf_idf)) 

#con un n de 10
tf_idf_mensajes %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(candidato) %>% 
  slice_max(tf_idf, n = 10)

#
tf_idf_mensajes %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(candidato) %>% 
  slice_max(tf_idf, n = 10) %>% 
  ggplot(aes(y = reorder(palabra, tf_idf), x = tf_idf, fill = candidato)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~candidato, scales = "free") #scales free para que cada uno tenga su barra
#El gráfico me muestra las palabras según el contexto del programa de cada candidato
#el itdf nos permite identificar las características de todos los textos de uno con otro

#Guardo figura

ggsave("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/figuras/idf_mensajes.png", height = 7, width = 10)



