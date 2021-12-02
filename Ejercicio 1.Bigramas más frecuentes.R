#Carga de códigos
install.packages('Rcpp')
library(Rcpp)
install.packages("pdftools")
library(Rcpp)
library(pdftools) #para traer un pdf a R
library(tidytext) #para dividir el texto en palabras, son tokenizadores enfocados en dataframes
library(stringr) #trabaja tabla de caracteres, transformaciones
library(readr) #leer y escribir archovs de texto plano(.csv, .txt)
library(dplyr) #filter, mutate
library(tidyr) # ordenar datos
library(ggplot2) #visualizacion
library(tokenizers)
library(SnowballC) #stemming (stem =raiz de la palabra), nos sirve para agrupar palabras
library(udpipe) #etiquetado gramatical y lematizaciÃ³n
library(wordcloud)

# Ejercicio 1: Cálculo de bigramas -----

#Ahora vamos a descargar este documento
#creamos carpeta
##dir.create("datos")

download.file("https://obtienearchivo.bcn.cl/obtienearchivo?id=recursoslegales/10221.3/22933/6/20120521.pdf",
              destfile = "/Users/Luis/Downloads/eval-texto_Lopez_Esteban/datos/bcn.pdf", mode = "wb")

#ejecuto este codigo para descargar el pdf, se llamara segun destfile y en version wb
#ver si se colan encabezxados, titulos, indices, etc que alteren el analisis

#vamos a extraer el texto del pdf

bcn <- pdf_text("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/datos/bcn.pdf")

#texto por paginas
bcn[1]
bcn[2]
bcn[21]

#Eliminamos los datos innecesarios como marcas y numero de paginas

bcn2 <- bcn %>% 
  str_remove_all("MENSAJE PRESIDENCIAL") %>% 
  str_remove_all("21 DE MAYO DE 2012") %>% 
  str_remove_all("[:space:]{2,}[:digit:]+\n") %>% 
  str_replace_all("[:space:]{2,}", " ") %>%  #cada vez que han 2 o mas espacios reemplazalo por solo uno
  .[3:34]  %>%
  str_remove_all("^\\d") %>% 
  paste(collapse = " ")   #juntar las cadenas de caracteres de las paginas 3 a 34

#me selecciona de la pagina 3 a 34, me elimina las demas

#vamos a guardar nuestra version en txt

write_lines(bcn2, "/Users/Luis/Downloads/eval-texto_Lopez_Esteban/datos/DISCURSO_2012.txt")
#---------------------------------------------------

#OBJETIVO: SACAR LA LISTA NEGRA DE PALABRAS, PARA ESO IMPORTO UN ARCHI TXT QUE
#ME CONVIENE TODAS ESAS PALABRAS PROHIBIDAS: UNOS,Y,ELLA,A,ETC

unas_stopwords <- read.csv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/vacias.txt",
                           encoding = "UTF-8") #encoding para que me lea las tildes

#tambien existan otras palabras de la lista negra que quizas no queramos para nuestro analisis

otras_stopwords <- tibble(palabra = c("mil","año","ciento","mm",
                                      "05","12","20","21","04","presidencial_2012"))

#ahora las excluyo de mi version final

frecuencias_2012 <- tibble(discurso = bcn2) %>% #variable llamada discurso
  unnest_tokens(input = discurso, 
                output = palabra, 
                strip_numeric = TRUE) %>% 
  count(palabra, sort = TRUE) %>%    
  anti_join(unas_stopwords) %>% 
  anti_join(otras_stopwords)

frecuencias_2012
#ESTAMOS OK!

#Finalmente vamos con los bigramas
#contar la frecuencia de los bigramas, palabras sueltas como: de la, de los

tibble(discurso = bcn2) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = T) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") #separo en dos clomunas para eliminar las stopwrods de una columana o de otra

#ahora excluyo entre columnas segun las stopwords

tibble(discurso = bcn2) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = T) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra,
         !palabra1 %in% unas_stopwords$palabra,
         !palabra2 %in% otras_stopwords$palabra,
         !palabra1 %in% otras_stopwords$palabra)

#ahora estos resultados los uno en una sola columna:

tibble(discurso = bcn2) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = T) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra,
         !palabra1 %in% unas_stopwords$palabra,
         !palabra2 %in% otras_stopwords$palabra,
         !palabra1 %in% otras_stopwords$palabra) %>% 
  unite(col ="bigrama", c(palabra1, palabra2), sep = " ") 

#Finalmente creamos el gráfico
tibble(discurso = bcn2) %>% 
  unnest_ngrams(input = discurso,
                output = bigrama,
                n = 2) %>% 
  count(bigrama, sort = T) %>% 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>% 
  filter(!palabra2 %in% unas_stopwords$palabra,
         !palabra1 %in% unas_stopwords$palabra,
         !palabra2 %in% otras_stopwords$palabra,
         !palabra1 %in% otras_stopwords$palabra) %>% 
  unite(col ="bigrama", c(palabra1, palabra2), sep = " ") %>% 
  slice_max(n, n=10) %>% #columna n,recortar 10 valores maximos no mas. si se repiten las frecuencias se agregan mas valores maximos
  ggplot(aes(y =reorder(bigrama, n),n)) +
  geom_col(fill = "red") +
  labs(title = "Los 10 bigramas mas frecuentes del mensaje presidencial 2012",
       y = NULL,
       X = "frecuencias") + 
  theme_minimal()

"OJO: me recorto en más de 10 porque hay 5 bigramas que se repiten 5 veces"

ggsave("/Users/Luis/Downloads/eval-texto_Lopez_Esteban/figuras/bigramas_ejercicio1.png", height = 7, width = 10)

