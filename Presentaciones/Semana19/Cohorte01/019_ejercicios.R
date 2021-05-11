# Librerias ----
library(tidyverse)
library(moments)

# 19.2.1 Ejercicios ----

# ¿Por qué TRUE no es un parámetro para rescale01()? 
# ¿Qué pasaría si x está contenido en un valor único perdido 
# y na.rm fuese FALSE?
rescale01 <- function(x, na.rm = TRUE, finite = TRUE) {
  rng <- range(x, na.rm = na.rm, finite = finite)
  (x - rng[1]) /
    (rng[2] - rng[1])
}

## El resultado no cambia
### NA - x = NA
rescale01(c(1:10, NA, -Inf), na.rm = TRUE)
### Esta parte ocurre dado que is.finite(NA) es false
### De todas maneras es importante señalar que
### is.infinite(NA) es también false
rescale01(c(1:10, NA, -Inf), na.rm = FALSE)
rescale01(c(1:10, NA, -Inf), na.rm = FALSE, finite = FALSE)

# En la segunda variante de rescale01(), los valores 
# infinitos se dejan sin cambio. Reescribe rescale01() 
# para que -Inf sea convertido a 0, e Inf a 1.
rescale01 <- function(x, na.rm = TRUE, finite = TRUE) {
  rng <- range(x, na.rm = na.rm, finite = finite)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == Inf]  <- 1
  y[y == -Inf] <- 0
  y
}

rescale01(c(1:10, NA, Inf, -Inf))

# Practica convertir los siguientes fragmentos de código en 
# funciones. Piensa en lo que hace cada función. ¿Cómo la llamarías? 
# ¿Cuántos argumentos necesita? ¿Puedes reescribirla para ser más expresiva 
# o con menos duplicación de código?

# mean(is.na(x))
pct_na <- function(x) {
  mean(is.na(x))
}

pct_na(c(1:10, NA, NaN, Inf))

# x / sum(x, na.rm = TRUE)
rescale_pct <- function(x) {
  x / sum(x, na.rm = TRUE)
}

rescale_pct(x = c(1:10, NA))
rescale_pct(x = c(1:10, NA, Inf))
## Aquí se presenta problema
### Inf + -Inf es igual a NaN
rescale_pct(x = c(1:10, NA, Inf, -Inf))

# sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
## https://en.wikipedia.org/wiki/Coefficient_of_variation
coef_var <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

coef_var(x = 1:10)

# Escribe tus propias funciones para computar la varianza 
# y la inclinación de un vector numérico.
variance <- function(x) {
  sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE) / (sum(!is.na(x)) - 1)
}

## Varianza
variance(1:10)
var(1:10)

## Asimetria
skewness1 <- function(x) {
  sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / (sum(!is.na(x)) - 2)*(var(x, na.rm = TRUE)^(3/2))
}

skewness1(1:10)
moments::skewness(x = 1:10, na.rm = TRUE)

# Escribe both_na() (ambos_na()), una función que toma 
# dos vectores de la misma longitud y retorna el número 
# de posiciones que tienen NA en ambos vectores.
both_na <- function(x, y) {
  which(is.na(x) & is.na(y)) 
}

both_na(
  c(NA, 1, 2 , NA),
  c(NA, 3, NA, NA)
)

# ¿Qué hacen las siguientes funciones? 
# ¿Por qué son tan útiles pese a ser tan cortas?
is_directory <- function(x) file.info(x)$isdir
is_readable <- function(x) file.access(x, 4) == 0

##
?file.info
file.info("019_funciones.Rmd")
## Verificar si es un directorio
file.info("019_funciones.Rmd")$isdir

?file.access
## Verificar si se tiene permisos de lectura
### 0 es exitoso y -1 es no exitoso
file.access("019_funciones.Rmd", mode = 4)

# Lee la letra completa de “Pequeño Conejito Foo Foo”. 
# Como ves, hay mucha duplicación en la letra de la canción. 
# Extiende el ejemplo inicial de pipes para recrear la 
# canción completa usando funciones para reducir la duplicación.

## Ver https://en.wikipedia.org/wiki/Little_Bunny_Foo_Foo
### No entiendo esta parte bien

# 19.3.1 Ejercicios ----

# Lee el código fuente para cada una de las siguientes tres 
# funciones, interpreta qué hacen y luego propone nombres mejores.

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

## Verificar si una cadena de caracteres
## tiene un determinado prefijo
str_has_prefix <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

str_has_prefix(string = "str_has_prefix", prefix = "str")


f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

## Eliminar el último componente de 
## un vector si este tiene una longitud
## mayor o igual a 2
delete_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

## Esta función no entiendo su utilidad
### Pero es por que no la he utilizado
### Por eso no encuentro un nombre adecuado
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}

f3(1:10, 2)

# Toma una función que hayas escrito recientemente 
# y tómate 5 minutos para pensar un mejor nombre 
# para la función y para sus argumentos.
skewness1 <- function(x) {
  sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / (sum(!is.na(x)) - 2)*(var(x, na.rm = TRUE)^(3/2))
}

skewness <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
    n <- length(x)
    sum((x - mean(x))^3) / (n - 2)*(var(x)^(3/2))
  } else {
    NA
  }
}

skewness(c(1:10, NA), na.rm = TRUE)
skewness(c(1:10, NA), na.rm = FALSE)

# Compara y contrasta rnorm() y MASS::mvrnorm(). 
# ¿Cómo podrías hacerlas más consistentes?

## https://en.wikipedia.org/wiki/Normal_distribution
## Los argumentos que utiliza son
## n, mean y sd
?rnorm

## https://en.wikipedia.org/wiki/Multivariate_normal_distribution
## Los argumentos que utiliza son
## n, mu y Sigma
### Sin embargo en este caso mu es un vector
### Sigma es una matriz que debe cumplir ciertas
### propiedades. Ademas el output de la función es 
### una matriz
?MASS::mvrnorm
Sigma <- matrix(c(2 , -1,  0,
                  -1,  2, -1,
                  0,  -1,  2),
                3, 3)

MASS::mvrnorm(n = 5, rep(0, 3), Sigma)

## Se podría adoptar la convención de utilizar el
## nombre de los parámetros tal cual se escribe 
## la función y utilizar mayúsculas al principio 
## para indicar que es un vector

## mvrnorm(n , Mu, Sigma, ...)
## rnorm(n, mu, Sigma)

# Argumenta por qué norm_r(),norm_d(), etc. 
# sería una mejor opción que rnorm(), dnorm(). }
# Argumenta lo contrario.

## https://www.johndcook.com/blog/distributions_r_splus/
## Esto depende de como se busque agrupar las funciones
## En el caso de R se agrupan en base a si:
### d: Función de densidad de probabilidad (probability density function)
### p: Función de distribución acumulada (cumulative distribution function)
#### Si lower.tail = FALSE entonces es el complemento de la  
#### Función de distribución acumulada
### q: Inversa de la función de distribución acumulada
#### Si lower.tail = FALSE entonces es la inversa del complemento 
#### de la función de distribución acumulada
### r: número aleatorio que se obtiene de la función de densidad de probabilidad 

# 19.4.4 Ejercicios ----

# ¿Cuál es la diferencia entre if e ifelse()? 
# Lee cuidadosamente la ayuda y construye tres 
# ejemplos que ilustren las diferencias clave.

## if utiliza una condición que puede tomar los valores
## TRUE ó FALSE para especificar que se hace 

## ifelse toma una condición que puede ser un vector
## y devuelve un resultado igual al tamaño de la condición

if (1:3 > c(0, 5, 10)) {
  1
} else {
  0  
}

ifelse(test = 1:3 > c(0, 5, 10), 
       yes = 1, 
       no = 0)

# Escribe una función de saludo que diga 
# “buenos días”, “buenas tardes” o “buenas noches”, 
# según la hora del día. (Sugerencia: usa un argumento 
# de tiempo que por defecto sea lubridate::now(); 
# eso hará que sea más fácil testear tu función).
saludo <- function(variables) {
  hour <- lubridate::hour(Sys.time())
  if (hour %in% 0:11) {
    print("Buenos días")
  } else if (hour %in% 12:18) {
    print("Buenos tardes")
  } else {
    print("Buenas noches")
  }
}

saludo()

# Implementa una función fizzbuzz que tenga un solo 
# número como input. Si el número es divisible por 
# tres, devuelve “fizz”. Si es divisible por cinco, 
# devuelve “buzz”. Si es divisible por tres y cinco, 
# devuelve “fizzbuzz”. De lo contrario, devuelve el 
# número. Asegúrate de escribir primero código que 
# funcione antes de crear la función.
fizzbuzz <- function(x) {
  if (((x %% 5) == 0) && ((x %% 3) == 0)) {
    print("fizzbuzz")
  } else if ((x %% 5) == 0) {
    print("buzz")
  } else if ((x %% 3) == 0) {
    print("fizz")
  } else {
    x
  }
}

fizzbuzz(15)
fizzbuzz(10)
fizzbuzz(9)
fizzbuzz(7)

# Cómo podrías usar cut() (cortar()) para simplificar 
# este conjunto de sentencias if-else anidadas?
if (temp <= 0) {
  "congelado"
} else if (temp <= 10) {
  "helado"
} else if (temp <= 20) {
  "fresco"
} else if (temp <= 30) {
  "tibio"
} else {
  "caluroso"
}

temp <- -100

# Sin etiquetas
cut(x      = temp, 
    breaks = c(-Inf, 0, 10, 20, 30, Inf))
# Con etiquetas
cut(x      = temp, 
    breaks = c(-Inf, 0, 10, 20, 30, Inf), 
    labels = c("congelado", "helado", "fresco", "tibio", "caluroso"))

# ¿Qué sucede si usas switch() con un valor numérico?

## El input es la tercera opción 
switch(3,
       "Lunes", "Martes",
       "Miércoles", "Jueves",
       "Viernes", "Sábado",
       "Domingo")

## Debido a que no existe una octava
## opción no se genera ningún input
switch(8,
       "Lunes", "Martes",
       "Miércoles", "Jueves",
       "Viernes", "Sábado",
       "Domingo")

# ¿Qué hace la llamada a switch()? 
# ¿Qué sucede si x fuera “e”?
x <- "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

# If there is a match then that element is evaluated 
# unless it is missing, in which case the next 
# non-missing element is evaluated
x <- "a"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

## In the case of no match, if there is an 
## unnamed element of ... its value is returned.  
x <- "e"
switch(x, 
       a = ,
       b = "ab",
       c = ,
       "cd"
)

# 19.5.5 Ejercicios ----

# ¿Qué realiza commas(letters, collapse = "-")? ¿Por qué?
commas <- function(...) stringr::str_c(..., collapse = ", ")

commas(letters, collapse = "-")
## En este caso ... captura letters y collapse = "-"
## En ese sentido se genera un error debido a que 
## collapse = "-" no es una cadena de caracteres

## Otra cosa es "collapse" sin "s"
commas(letters, collape = "-")

# Sería bueno si se pudiera suministrar múltiples caracteres 
# al argumento pad, por ejemplo, rule("Title", pad = "-+"). 
# ¿Por qué esto actualmente no funciona? ¿Cómo podrías solucionarlo?
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

## Ver # x = "width"
## en Options used in base R
?getOption
getOption("width")
?str_dup
str_dup("+", times = 4)

##
rule("Title", pad = "-+*")

rule <- function(..., pad = "-") {
  title <- paste0(x)
  width <- getOption("width") - nchar(title) - 5
  cat(title
      , " ", 
      # Aquí lo que hariamos sería truncar la cadena 
      # de caracteres para que sea de la longitug
      # determinanda por length
      stringr::str_trunc(stringr::str_dup(pad, width), 
                         width = width , side = "right", ellipsis = ""), 
      "\n", 
      sep = "")
}

rule("Title", pad = "-")

# ¿Qué realiza el argumento trim a la función mean()? 
# ¿Cuándo podrías utilizarlo?

## Revisar https://garstats.wordpress.com/2017/11/28/trimmed-means/
?mean
## El argumento trim elimina los valores de los extremos
mean(c(1, 3, 7, 9, 24, 4), trim = 0)
## Equivalencias
mean(c(1, 3, 4, 7, 9, 24), trim = 0.2)
### Se ordenan los vectores del menor al mayor
### Luego si 6*0.2 = 1.2
### Se realiza el redondeo hacia abajo que es 1 
### y se elimina 1 componente de cada extremo
mean(c(3, 4, 7, 9), trim = 0)

# El valor de defecto del argumento method para cor() 
# es c("pearson", "kendall", "spearman"). ¿Qué significa esto? 
# ¿Qué valor se utiliza por defecto?
?cor

## Verificar el código de cov donde encontramos
## que uno de los argumentos es 
## method = c("pearson", "kendall", "spearman")
### Adicionalmente encontramos 
### method <- match.arg(method)
cor
?match.arg
### Ver https://alistaire.rbind.io/blog/match.arg/ para
### entender match.arg
check_color <- name <- function(color) {
  match.arg(arg = color, 
            choices = c("azul", "verde", "amarillo"))
}

## Nada sorprendente
check_color(color = "azul")
## Esto si es mejor
check_color(color = "az")


check_color <- name <- function(color = c("azul", "verde", "amarillo")) {
  match.arg(arg = color)
}

# Ver ?match.arg en la sección Details
check_color()