# Librerias ----
library(tidyverse)
library(datos)
library(stringi)

# 14.2.5 Ejercicios ----

# En ejemplos de código en los que no se utiliza stringr, 
# verás usualmente paste() y paste0() (paste = pegar). 
# ¿Cuál es la diferencia entre estas dos funciones? 
# ¿A qué función de stringr son equivalentes? 
# ¿Cómo difieren estas dos funciones respecto de su manejo de los NA?

## Diferencias entre paste y paste0
x <- c("a", NA, "c", "d", NULL, FALSE, NaN)
?paste
?paste0

## Primer ejemplo 
paste(x)
paste0(x)
paste(x, collapse = ",")
paste0(x, collapse = ",")

## Segundo ejemplo
paste("a", "b", "c")
### paste0 es equivalente a 
#### paste("a", "b", "c", sep = "")
paste0("a", "b", "c")

## paste0 y paste son equivalentes a str_c
## Ver la sección See Also
?str_c

## Manejo de NA
str_c(x)
### genera valores NA cuando se combinan
str_c(x, collapse = ",")

# Describe con tus propias palabras la diferencia entre 
# los argumentos sep y collapse de la función str_c().

## sep son los caracteres que se utilizan para 
## separar cada vector que se utilizan como 
## insumos
x <- c("a", "b", "c")
y <- c("d", "e", "f")

str_c(x, y, sep = ",", collapse = NULL)
## collapse son los caracteres que se utilizan
## para combinar vectores 
str_c(x, y, sep = ",", collapse = "_")

# Utiliza str_length() y str_sub() para extraer el caracter 
# del medio de una cadena. ¿Qué harías si el número de caracteres es par?

## posible función
str_sub_middle <- function(string, lower = TRUE) {
  if ((str_length(string = string) %% 2) == 0) {
    if (lower) {
      middle <- (str_length(string = string) / 2) %>%
        str_sub(string = string, start = ., end = .)
    } else {
      middle <- ((str_length(string = string) / 2) + 1) %>%
        str_sub(string = string, start = ., end = .)  
    }
    
  } else {
    middle <- (str_length(string = string) / 2) %>%
      ceiling() %>% 
      str_sub(string = string, start = ., end = .)
  }
  
  return(middle)
}

## probando la función
str_sub_middle(string = "am", lower = TRUE)
str_sub_middle(string = "a", lower = TRUE)
str_sub_middle(string = "ame", lower = TRUE)

# ¿Qué hace str_wrap()? (wrap = envolver) 
# ¿Cuándo podrías querer utilizarla?
?str_wrap

## divide los párrafos de un texto en líneas
### https://stringr.tidyverse.org/reference/str_wrap.html
thank_path <- file.path(R.home(component = "doc"), "THANKS")
thank <- readLines(con = thank_path) %>% 
  str_c(collapse = "\n")
thank
thank %>% str_wrap(width = 100, indent = 2, exdent = 1) %>% cat()

# ¿Qué hace str_trim()? (trim = recortar) 
# ¿Cuál es el opuesto de str_trim()?
?str_trim

## remueve los espacios blancos al principio y al final
## de una cadena de caracteres
str_trim(string = " animales ", side = "both")

# Escribe una función que convierta, por ejemplo, 
# el vector c("a", "b", "c") en la cadena a, b y c. 
# Piensa con detención qué debería hacer dado un vector 
# de largo 0, 1 o 2.

## Función propuesta
str_list <- function(vector) {
  length <- length(vector)
  if (length == 0) {
    str <- vector
  } else if (length >= 1) {
    str <- vector[-length] %>% 
              str_c(collapse = ", ") %>% 
              c(., vector[length]) %>% 
              str_c(collapse = " y ")
  }
  return(str)
}

## Revisando la solución se puede
## mejorar de la siguiente manera
str_list <- function(vector, delim = ",") {
  length <- length(vector)
  if (length == 0) {
    str <- vector
  } else if (length >= 1) {
    str <- vector[-length] %>% 
      str_c(collapse = str_glue({delim}, " ")) %>% 
      c(., vector[length]) %>% 
      str_c(collapse = " y ")
  }
  return(str)
}

## Probando la función
1:10 %>% as.character() %>% str_list(delim = "")
letters %>% str_list()

# 14.2.7.1 Ejercicios ----

# Explica por qué cada una de estas cadenas no 
# coincide con \: "\", "\\", "\\\".
writeLines("\\")

## En el caso de "\" no coindice dado que estaría tratando
## de generar un escape al caracter " a la derecha es decir 
## algo como \" donde además el símbolo " a la izquierda
## quedaría solo

## En el caso de "\\" el problema está que aunque la expresión
## regular "\\" es la correcta al incluirla como un caracter se
## requiere interpretar "\" como un signo de escape

## En el caso de "\\\" se tiene que "\" en la primera posición
## escapa "\" en la seguna posición pero se requiere escapar "\"
## en la primera posición. Por lo tanto se requiere "\\\\" para 
## que exista una coincidencia con "\" que en realidad debe ser
## "\"

# ¿Cómo harías coincidir la secuencia "'\?
writeLines("\"\'\\?")
str_view(string = "\"\'\\?",
         # aquí la diferencia es que se debe
         # incluir \? por que ? tiene un significado
         # en una expresión regular
         pattern = r"(\"\'\\\?)")

# ¿Con qué patrones coincidiría la expresión regular\..\..\..? 
# ¿Cómo la representarías en una cadena?

writeLines(".a.b.c")
str_view(string = ".a.b.c", pattern = r"(\..\..\..)")
str_view(string = ".a.b.cmasletras", pattern = r"(\..\..\..)")
## solo incluye la primera coincidencia 
str_view_all(string = ".a.b.cmasletras.o.p.q", pattern = r"(\..\..\..)")
## si se requiere todas las coincidencias
str_view_all(string = ".a.b.cmasletras.o.p.q", pattern = r"(\..\..\..)")


# 14.2.8.1 Ejercicios ----

# ¿Cómo harías coincidir la cadena "$^$" de manera literal?
writeLines("$^$")
str_view(string = "$^$ es un símbolo extraño", 
         pattern = r"(\$\^\$)")

# Dado el corpus de palabras comunes en datos::palabras, 
# crea una expresión regular que busque palabras que:

# Empiecen con “y”.
# Terminen con “z”
# Tengan una extensión de exactamente tres letras. 
# (¡No hagas trampa usando str_length()!)
# Tengan ocho letras o más.
datos::palabras %>% 
  str_view(pattern = "^y", match = TRUE)

datos::palabras %>% 
  str_view(pattern = "z$", match = TRUE)

## Esta es la opción que no se quiere
datos::palabras[str_length(datos::palabras) == 3]
## Esta es la opción que se quiere
datos::palabras %>% 
  str_view(pattern = r"(\b...\b)", match = TRUE)

## Esta es la opción que no se quiere
datos::palabras[str_length(datos::palabras) >= 8]
## Esta es la opción que se quiere
datos::palabras %>% 
  str_view(pattern = "........", match = TRUE)

# 14.2.9.1 Ejercicios ----

# Crea una expresión regular que encuentre todas las palabras que:

# Empiecen con una vocal.
datos::palabras %>% str_view(pattern = "^[aeiou]", match = TRUE)
# Solo contengan consonantes. (Pista: piensa en cómo buscar 
# coincidencias para “no”-vocales.)
datos::palabras %>% str_view(pattern = "[aeiouáéíóú]",
                              # con este argumento no encuentra
                              # ninguna coincidencia con una
                              # vocal
                              match = FALSE)
# Terminen en ón, pero no en ión.
datos::palabras %>% str_view(pattern = "[^i]ón$", match = TRUE)
# Terminen con ndo o ado.
datos::palabras %>% str_view(pattern = "(ndo|ado)$", match = TRUE)

# Escribe una expresión regular que permita buscar un verbo que 
# haya sido escrito usando voseo en segunda persona plural
# (por ejemplo, queréis en vez de quieren).
c("conocéis", "conocen", "caminan", "caminaráis") %>% 
  str_view(pattern = "(éis|áis)$")

# Crea una expresión regular que coincida con la forma en que habitualmente 
# se escriben los números de teléfono en tu país.
c("7710897", 
  "3124541550", 
  "23453", 
  "0317710897", 
  "018007710897", "028007710897") %>% 
  str_view(pattern = r"(\b(\d\d\d\d\d\d\d|\d\d\d\d\d\d\d\d\d\d|01800\d\d\d\d\d\d\d)\b)")

# En inglés existe una regla que dice que la letra i va siempre antes 
# de la e, excepto cuando está después de una c". Verifica empíricamente 
# esta regla utilizando las palabras contenidas en stringr::words.

## aquí aplica la regla
stringr::words %>% 
  str_view(pattern = "(cei|[^c]ie)", match = TRUE)
## aquí no aplica la regla
stringr::words %>% 
  str_view(pattern = "(cie|[^c]ei)", match = TRUE)

#  14.2.10.1 Ejercicios ----

# Describe los equivalentes de ?, +, * en el formato {m,n}.

## equivalente ?
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos?")
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos{0,1}")

## equivalente +
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos+")
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos{1,}")

## equivalente *
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos*")
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss"), 
         pattern = "gatos{0,}")

# Describe en palabras con qué coincidiría cada una de 
# estas expresiones regulares: (lee con atención para 
# ver si estamos utilizando una expresión regular o 
# una cadena que define una expresión regular.)

# ^.*$
## Significa cualquier cadena de caracteres incluyendo
## una cadena de caracteres vacía
str_view(c("gatos", "gato" , "gats", "gatoss", "gatosss", "", " "), 
         # . es cualquir caracter
         # .* es cualquier caracter 0 a más veces
         pattern = "^.*$")

# "\\{.+\\}"
## una cadena de carateres que contenga { luego 1 
## o más caracteres seguido por }
str_view(c("{}", "{g}", "{ga}", "{gat}", "{gato", "gatos}"), 
         pattern = r"(\{.+\})")

# \d{4}-\d{2}-\d{2}
## Una cadena de caracteres que empiece con exactamente
## 4 dígitos seguida de - luego de exactamente 2 dígitos
## luego - y después exactamente 2 dígitos
str_view(c("1234-56-78", "234-56-78"), 
         pattern = r"(\d{4}-\d{2}-\d{2})")

# "\\\\{4}"
## Una cadena de caracteres que contenga "\" 
## exactamente cuatro veces
writeLines(r"(\)", r"(\\\)", r"(\\\\)")

# Crea expresiones regulares para buscar todas las palabras que:
  
# Empiecen con dos consonantes.
str_view(string = stringr::words,
         # en español tendriamos que 
         # no incluir también áéíóú
         pattern = "^[^aeiou]{2}", match = TRUE)

# Tengan tres o más vocales seguidas.
str_view(string = c(stringr::words),
         # en español tendriamos que 
         # incluir también áéíóú
         pattern = "[aeiou]{3,}", match = TRUE)

# Tengan tres o más pares de vocal-consonante seguidos.
str_view(string = c(stringr::words),
         # en español tendriamos que 
         # incluir y no incluir también áéíóú
         pattern = "([aeiou][^aeiou]){3,}", match = TRUE)

# 14.2.11.1 Ejercicios ----

# Describe en palabras con qué coinciden estas expresiones:
  
# (.)\1\1
## En este caso estoy capturando cualquier caracter 
## y luego queremos que se repita tres veces seguidas
str_view(string = c("aaa", "laaal", "lool"), pattern = r"((.)\1\1)")

# "(.)(.)\\2\\1"
## En este caso capturamos un caracter y luego capturamos un 
## segundo caracter luego nos referimos a cada captura como
## \1 y \2
str_view(string = c("soos", "caac", "casa"), pattern = r"((.)(.)\2\1)")

# (..)\1
# Se capturan un par de caracteres y luego
# se busca que se repitan
str_view(stringr::words, pattern = r"((..)\1)", match = TRUE)

# "(.).\\1.\\1"
# se captura un caracter luego se quiere 
# otro caracter despues se tiene el caracter
# capturado despues se tiene cualquier otro 
# caracter y finalmente se tiene de nuevo el
# caracter capturado
str_view(string = c("abaca", "dedfd", "ghgij"), 
         pattern = r"((.).\1.\1)")

#"(.)(.)(.).*\\3\\2\\1"
## Se realizan tres capturas. Luego aparece 0 o
## más caracteres y despues debe aparecer la tercera 
## segunda y primera captura en ese orden
str_view(string = c("abcdcba", "abcdcba", "abcabd"), 
         pattern = r"((.)(.)(.).*\3\2\1)")

# Construye una expresión regular que coincida con palabras que:

# Empiecen y terminen con el mismo caracter.
str_view(string = stringr::words, 
         pattern = r"(^(.).*\1$)", match = TRUE)

str_view(string = stringr::words, 
         pattern = r"(^(.).*\1$)", match = TRUE)


# Contengan un par de letras repetido 
# (p. ej. “nacional” tiene “na” repetido dos veces.)
str_view(string = stringr::words, 
         pattern = r"((..).*\1)", match = TRUE)

str_view(string = "nacional", 
         pattern = r"((..).*\1)", match = TRUE)

# Contengan una letra repetida en al menos tres lugares 
# (p. ej. “característica” tiene tres “a”.)
str_view(string = stringr::words, 
         pattern = r"((.).*\1.*\1)", match = TRUE)

str_view(string = "característica",
         # en este caso la primera coincidencia es 
         # la "c"
         pattern = r"((.).*\1.*\1)", match = TRUE)

# 14.3.2 Ejercicios ----

# Para cada uno de los siguientes desafíos, intenta buscar 
# una solución utilizando tanto una expresión regular simple 
# como una combinación de múltiples llamadas a str_detect().

# Encuentra todas las palabras que empiezan o terminan con y.
stringr::words %>% 
  str_view(pattern = "^y|y$", match = TRUE)

# Encuentra todas las palabras que empiezan con una vocal y 
# terminan con una consonante.
stringr::words %>% 
  str_view(pattern = "^[aeiou].*[^aeiou]$", match = TRUE)

# ¿Existen palabras que tengan todas las vocales?
## Me pareció difícil esta pregunta
str_string <- "Inmunizándole"

(str_detect(string = str_string, pattern = "[Aaá]") & 
  str_detect(string = str_string, pattern = "[Eeé]") &
  str_detect(string = str_string, pattern = "[Iií]") &
  str_detect(string = str_string, pattern = "[Ooó]") &
  str_detect(string = str_string, pattern = "[Uuúü]")) %>% 
  str_string[.]

## Cuál es el operador "and" en expresiones regulares
### https://stackoverflow.com/questions/54267095/what-is-the-regex-to-match-the-words-containing-all-the-vowels
str_detect(string  = "murcielago", 
           pattern = r"(\b(?=\w*?a)(?=\w*?e)(?=\w*?i)(?=\w*?o)(?=\w*?u)[a-zA-Z]+\b)")

str_view(string = "abc123xyz", pattern = "123")

str_view_all(string = "The car parked in the garage", pattern = ".ar")


library(stringr)
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

# ¿Qué palabra tiene el mayor número de vocales? 
# ¿Qué palabra tiene la mayor proporción de vocales? 
# (Pista: ¿cuál es el denominador?)
vocales_tbl <- tibble(words = stringr::words) %>% 
  mutate(n_vocales   = str_count(string = words, pattern = "[aeiou]"),
         pct_vocales = n_vocales / str_length(words))

vocales_tbl %>% 
  slice_max(order_by = n_vocales, n = 3, with_ties = TRUE)

vocales_tbl %>% 
  slice_max(order_by = pct_vocales, n = 3, with_ties = TRUE)

# 14.3.3.1 Ejercicios ----

# Te habrás dado cuenta que en el ejemplo anterior la expresión 
# regular que utilizamos también devolvió como resultado “arrojo” 
# y “azulejos”, que no son nombres de colores. Modifica la expresión 
# regular para resolver ese problema.
datos::oraciones %>% 
  # Creo que es mas ordenado trabajar de esta
  # forma
  tibble::enframe(name  = "id_oracion", 
                  value = "oracion") %>% 
  mutate(colores = str_extract_all(string = oracion,
                      # Consultar para concer \b    
                      # https://www.rexegg.com/regex-boundaries.html#wordboundary
                      # http://www.regular-expressions.info/wordboundaries.html
                                   pattern = r"(\b(rojo|amarillo|verde|azul|marrón)\b)")) %>% 
  mutate(len_colores = map_int(.x = colores, .f = length)) %>% 
  filter(len_colores >= 2)


# De datos::oraciones extrae:

# La primera palabra de cada oración.
datos::oraciones %>% 
  tibble::enframe(name  = "id_oracion", 
                  value = "oracion") %>% 
  mutate(primera_palab = str_extract(string  = oracion, 
                                     # aquí lo aplico para 
                                     # no tener problemas
                                     # con tildes, números
                                     # u otros signos
                                     pattern = "^[^ ]+"))

# Todas las palabras que terminen en ción.
datos::oraciones %>% 
  tibble::enframe(name  = "id_oracion", 
                  value = "oracion") %>% 
  mutate(term_cion = str_extract_all(string  = oracion,
                                     # parece que no hay
                                     pattern = r"([^\s]*ción\b)")) %>% 
  mutate(len_term_cion = map_int(term_cion, length)) %>% 
  filter(len_term_cion > 0)

# Todos los plurales.
## Este es mi primer intento fallido
## dado que include por ejemplo Las, 
## gruesas, través
str_view_all(oraciones, r"([^ ]+s\b)")

# 14.3.4.1 Ejercicios ----

# Busca en datos::oraciones todas las palabras que vengan
# después de un “número”, como “un(o|a)”, “dos”, “tres”, etc. 
# Extrae tanto el número como la palabra.
datos::oraciones %>% 
  str_view(pattern = r"(\b(un|uno|una|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\b ([^ .]+))")

datos::oraciones %>% 
  str_extract(pattern = r"(\b(un|uno|una|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\b ([^ .]+))")

datos::oraciones %>% 
  str_match(pattern = r"(\b(un|uno|una|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez)\b ([^ .]+))") %>% 
  as_tibble() %>% 
  set_names(nm = c("cadena", "numero", "sujeto")) %>% 
  drop_na()

# En español a veces se utiliza el guión para unir adjetivos, establecer 
# relaciones entre conceptos o para unir gentilicios 
# (p. ej., teórico-práctico, precio-calidad, franco-porteña). 
# ¿Cómo podrías encontrar esas palabras y separar lo que viene antes y 
# después del guión?
c("teórico-práctico", "precio-calidad", "franco-porteña") %>% 
  str_match(pattern = r"(([^ ]+)-([^ ]+))") %>% 
  as_tibble() %>% 
  set_names(nm = c("cadena", "primera_cadena", "segunda_cadena"))

# 14.4.4.1 Exercises ----

# Remplaza en una cadena todas las barras (forward slashes) por 
# barras invertidas (backslashes).
"/home/usuario/Documentos/" %>% 
  str_replace_all(pattern = "/", replacement = r"(\\)") %>% 
  writeLines()

# Implementa una versón simple de str_to_lower() (a minúsculas) 
# usando replace_all().
stringr::words %>% 
  str_to_title() %>% 
  str_replace_all(pattern = c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
                              "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
                              "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
                              "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
                              "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
                              "Z" = "z"))

# Cambia la primera y la última letra en palabras. 
# ¿Cuáles de esas cadenas siguen siendo palabras?
## Las palabras que no cambiarían sería 
datos::palabras %>% 
  str_replace_all(pattern     = "(^.)([^ ]*)(.$)", 
                  replacement = r"(\3\2\1)")

## Es una pregunta difícil
### Sin embargo existen algunos candidatos
#### Que la primera y última palabra sean las mismas
datos::palabras %>% 
  str_view(pattern = r"((^.)([^ ]*)(\1$))", match = TRUE)
## otra que solo exista una letra
(datos::palabras %>% 
  str_length() == 1) %>% 
  datos::palabras[.]

# 14.3.6.1 Ejercicios ----

# Divide una cadena como "manzanas, peras y bananas" 
# en elementos individuales.
"manzanas, peras y bananas" %>% 
  str_split(pattern = "(, )|( y )") %>% 
  pluck(1)

# ¿Por qué es mejor dividir utilizando boundary("word") 
# en vez de " "?
?boundary

"El (gato) es negro#" %>% 
  str_split(pattern = " ")

## esta es la ventaja de utilizar
## pattern = boundary(type = "word")
"El (gato) es negro#" %>% 
  str_split(pattern = boundary(type = "word"))

## claro que no funciona en casos como
## ne(gro#
"El (gato) es ne(gro#" %>% 
  str_split(pattern = boundary(type = "word"))

# ¿Qué pasa si dividimos con una cadena vacía ("")? 
# Experimenta y luego lee la documentación

?str_split
## en la documentación se señala 
## An empty pattern, "", is equivalent to boundary("character")
"El gato es negro" %>% 
  # en este caso la cadena de caracteres
  # es dividida en cada uno de sus caracteres
  str_split(pattern = "")

"El gato es negro" %>% 
  str_split(pattern = boundary(type = "character"))

# 14.4.1 Ejercicios ----

# ¿Cómo buscarías todas las cadenas que contienen \ con regex() 
# vs. con fixed()?
c(r"(hola\)", r"(co\mó)", r"(estás)") %>% writeLines()

## Caso regex
c(r"(hola\)", r"(co\mó)", r"(estás)") %>% 
  str_view(pattern = regex(pattern = r"(\\)"))

## Caso fixed
c(r"(hola\)", r"(co\mó)", r"(estás)") %>% 
                     # En este caso no se requiere el
                     # escape
  str_view(pattern = fixed(pattern = r"(\)"))

# ¿Cuáles son las cinco palabras más comunes en oraciones?
datos::oraciones %>% 
  str_extract_all(pattern = boundary(type = "word")) %>% 
  enframe(name = "id_oracion", value = "palabra") %>% 
  unnest(cols = palabra) %>% 
  count(palabra) %>% 
  slice_max(order_by = n, n = 5)

# 14.6.1 Ejercicios ----

# Busca la función de stringi que:
help(package = "stringi")
# Cuenta el número de palabras.
?stri_count_boundaries
test <- 'The\u00a0above-mentioned features are very useful. Spam, spam, eggs, bacon, and spam.'
stringi::stri_count_words(test)

# Busca cadenas duplicadas
?stri_duplicated
stri_duplicated(c('a', 'b', 'a', NA, 'a', NA))

# Genera texto aleatorio.
?stri_rand_strings
set.seed(123)
stri_rand_strings(n = 3, length = 5)

# ¿Cómo puedes controlar qué lengua usa stri_sort() 
# para ordenar?
?stri_sort
x <- c("arándano", "espinaca", "banana")
stri_sort(str = x, locale = "haw")
stri_sort(str = x, locale = "es")