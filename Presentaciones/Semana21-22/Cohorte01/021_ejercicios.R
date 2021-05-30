# Librerias ----
library(tidyverse)
library(datos)
library(microbenchmark)
library(broom)

# 21.2.1 Ejercicios ----

# Escribe bucles for para:
# Calcular la media de cada columna en datos::mtautos.
media <- vector(mode = "numeric", 
                length = ncol(datos::mtautos))

for (i in seq_along(datos::mtautos)) {
  media[[i]] <- mean(datos::mtautos[[i]])
}

media

# Determinar el tipo de cada columna en datos::vuelos.
tipo <- vector(mode = "numeric", length = ncol(datos::vuelos))

for (i in seq_along(datos::vuelos)) {
  tipo[[i]] <- typeof(datos::vuelos[[i]])
}

tipo

# Calcular el número de valores únicos en cada columna 
# de datos::flores.
unique_value <- vector(mode   = "numeric", 
                       length = ncol(datos::flores))

for (i in seq_along(datos::flores)) {
  unique_value[[i]] <- length(unique(datos::flores[[i]]))
}

unique_value

# Generar diez normales aleatorias de distribuciones 
# con medias -10, 0, 10 y 100.
# Piensa en el resultado, la secuencia y el cuerpo antes 
# de empezar a escribir el bucle.
medias <- c(-10, 0, 10, 100)
random <- vector(mode = "list", length = length(medias))

for (i in seq_along(medias)) {
  set.seed(seed = 1234)
  random[[i]] <- rnorm(n = 10L, mean = medias[[i]],sd = 1)
}

random

# Elimina el bucle for en cada uno de los siguientes 
# ejemplos aprovechando alguna función existente que 
# trabaje con vectores:

#
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}

##
str_c(letters, sep = "", collapse = "")

#
set.seed(1234)
     # En este caso si x es de longitud
     # 1 y x >=1 se obtienen los números
     # de 1:x
     # Como no se señalo la cantidad entonces
     # se genera un tamaño de x
x <- sample(x = 100, replace = FALSE)
std_dev <- 0
for (i in seq_along(x)) {
  # Se realiza la sumatoria de las
  # desviaciones al cuadrado
  std_dev <- std_dev + (x[i] - mean(x)) ^ 2
}
# Se calcula la desviación estandar
# muestral
std_dev <- sqrt(std_dev / (length(x) - 1))

##
sd(x, na.rm = FALSE) == std_dev

#
set.seed(1234)
x <- runif(100)
out <- vector("numeric", length(x))
## Aquí se esta haciendo la suma acumulada
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}

identical(out, cumsum(x = x))

# Combina tus habilidades para escribir funciones y 
# bucles for:

# Escribe un bucle for que imprima (prints()) la 
# letra de la canción de niños “Cinco ranitas verdes” 
# (u otra).

ranitas <- c("cinco ranitas verdes",
             "cuatro ranitas verdes",
             "tres ranitas verdes",
             "dos ranitas verdes",
             "una ranita verde", 
             "no hay ninguna ranita verde")
len <- length(ranitas)
cancion <- vector(mode = "character", length = len - 1)

for (i in (seq_along(ranitas)[-len])) {
  linea2 <- "en un palo de color café"
  linea5 <- "donde el agua es fresca"
  if (i %in% 1:(len - 2)) {
    cancion[i] <- str_glue('{str_to_sentence(ranitas[[i]])}, 
                        {linea2}, 
                        comían insectos muy sabrosos. 
                        Una saltó en la charca 
                        {linea5} 
                        ahora, hay {ranitas[[i + 1]]}.
                        
                        ')
  } else {
    cancion[i] <- str_glue('{str_to_sentence(ranitas[[i]])}, 
                      {linea2}, 
                      comía insectos muy sabrosos. 
                      Ella saltó en la charca 
                      {linea5} 
                      ahora, {ranitas[[i + 1]]}.'
                           )    
  }
}

str_c(cancion, sep = "\n", collapse = "\n") %>% 
  cat(sep = "\n")

# Convierte la canción infantil “Cinco monitos saltaban en 
# la cama” en una función. Generalizar a cualquier cantidad 
# de monitos en cualquier estructura para dormir.
cancion_monitos <- function(monitos) {
  for (i in rev(1:monitos)) {
    if (i == 1) {
      cat(str_glue("{i} monito saltando en la cama"))
      } else {
        cat(str_glue("{i} monitos saltando en la cama"))  
  }
    cat("\n")
    cat(str_glue("Uno cayó al piso y la cabeza se golpeó
                 Mamá llamó al doctor y el doctor la consejó
                 ¡Ya no más monos saltando en la cama!"))
    cat("\n")
    cat("\n")
    }
}

cancion_monitos(monitos = 3)

# Convierte la canción “99 botellas de cerveza en la pared” 
# en una función. Generalizar a cualquier cantidad, de cualquier 
# tipo de recipiente que contenga cualquier líquido sobre cualquier 
# superficie.
cancion_botellas <- function(len) {
  for (i in rev(1:len)) {
    cat(str_glue("{i} botellas de cerveza en la pared
             ¡{i} botellas de cerveza!
             Si a una de esas botellas
             Le ocurre caer,
             {i - 1} botellas de cerveza en la pared"))
    cat("\n")
    cat("\n")
  }
  cat(str_glue("Ya no hay botellas de cerveza en la pared,
               Ya no hay botellas de cerveza.
               Ve a la tienda y compra algunas más,
               {len} botellas de cerveza en la pared..."))
}

cancion_botellas(len = 3)

# Es común ver bucles for que no preasignan el 
# output y en su lugar aumentan la longitud de 
# un vector en cada paso:

##
no_fix_loop <- function(n) {
x <- 1:n
output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
  }
output
}

no_fix_loop(n = 10)

fix_loop <- function(n) {
  x <- 1:n
  output <- vector("integer", n)
  for (i in seq_along(x)) {
    output[[i]] <- lengths(x[[i]])
  }
  output
}

fix_loop(n = 10)

microbenchmark::microbenchmark(
  no_fix_loop(n = 10000),
  fix_loop(n = 10000), 
  times = 100
)

# 21.3.5 Ejercicios ----

# Imagina que tienes un directorio lleno de 
# archivos CSV que quieres importar. Tienes 
# sus ubicaciones en un vector, 
# files <- dir("data/", pattern = "\\.csv$", full.names = TRUE), 
# y ahora quieres leer cada uno con read_csv(). 
# Escribe un bucle for que los cargue en un solo data frame.

## Un buen ejemplo del tema es: https://youtu.be/An1bUIg-nVM

## Otra es crearlos
especies_lst <- split(x = datos::flores, 
                      f = datos::flores$Especies)

for (i in seq_along(especies_lst)) {
  write_csv(x = especies_lst[[i]], 
            file = str_glue("datos/{names(especies_lst)[[i]]}.csv"))
}

files <- dir(path = "datos/", pattern = r"((setosa|versicolor|virginica)\.csv$)", 
    full.names = TRUE)

file_contents <- vector(mode = "list", length = length(files))

for (i in seq_along(files)) {
  file_contents[[i]] <- read.csv(file = files[[i]])
}

especies_data_frame <- do.call(what = "rbind", args = file_contents)

# ¿Qué pasa si utilizamos for (nm in names(x)) 
# y x no tiene nombres (names)? 
# ¿Qué pasa si solo algunos elementos están nombrados? 
# ¿Qué pasa si los nombres no son únicos?

lst_null_names <- list(1, 2, 3)
names(lst_null_names)

for (i in 1:3) {
  print(names(lst_null_names)[[i]])
}

lst_partial_names <- list(a = 1, 2, c = 3)
names(lst_partial_names)  

for (i in 1:3) {
  print(names(lst_partial_names)[[i]])
}

lst_all_names <- list(a = 1, b = 2, c = 3)

for (i in 1:3) {
  print(names(lst_all_names)[[i]])
}

# Escribe una función que imprima el promedio 
# de cada columna numérica en un data frame, 
# junto con su nombre. Por ejemplo, 
# mostrar_promedio(flores) debe imprimir:

print_var_mean <- function(data_frame) {
  
  if (!is.data.frame(data_frame)) {
    warning("argument is not a data.frame")
  } else {
    is_numeric_col <- sapply(data_frame, is.numeric)
    data_frame_is_numeric <- data_frame[, is_numeric_col]
    for (i in seq_along(data_frame_is_numeric)) {
      cat(names(data_frame_is_numeric)[[i]], 
          ": ",
          mean(data_frame_is_numeric[[i]], na.rm = TRUE), 
          "\n",
          sep = "")
    }
  }
  invisible(data_frame)
}

print_var_mean(datos::flores)

# 21.4.1 Ejercicios ----

# Lee la documentación para apply(). 
# En el caso 2d, ¿qué dos bucles for generaliza?

## Para el caso de un objeto de dos dimensiones
## se puede hacer un loop iterando sobre columnas
## o sobre filas. Este aspecto se controla con el
## argumento MARGIN
mtx <- matrix(data = 1:6, nrow = 3, ncol = 2)

# MARGIN = 1 se hace el loop sobre filas y 
# MARGIN = 2 sobre columnas
apply(X = mtx, FUN = mean, MARGIN = 1)
apply(X = mtx, FUN = mean, MARGIN = 2)

vec_row_mean <- vector(mode = "numeric", length = nrow(mtx))
for (i in 1:nrow(mtx)) {
  vec_row_mean[[i]] <- mean(mtx[i, ])
}

vec_row_mean

vec_row_col <- vector(mode = "numeric", length = ncol(mtx))
for (j in 1:ncol(mtx)) {
  vec_row_col[[j]] <- mean(mtx[ , j])
}

vec_row_col

# Adapta col_resumen() para que solo se aplique 
# a las columnas numéricas. Podrías querer comenzar 
# con la función is_numeric () que devuelve un 
# vector lógico que tenga un TRUE por cada columna 
# numérica.

col_resumen <- function(df, fun) {
  # Esta parte es la que se agrega
  df  <- df[, sapply(df, is.numeric)]  
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_resumen(df = flores, fun = mean)

# 21.5.3 Ejercicios ----

# Escribe un código que use una de las funciones de map para:
  
# Calcular la media de cada columna en datos::mautos.
map_dfc(.x = datos::mtautos, .f = mean)

# Obtener de qué tipo es cada columna en datos::vuelos.
map_dfc(.x = datos::mtautos, .f = typeof)

# Calcular la cantidad de valores únicos en cada columna 
# de datos::flores.
map_dfc(.x = datos::mtautos, .f = ~ length(unique(x = .)))

# Generar diez normales aleatorias de distribuciones 
# con medias -10, 0, 10 y 100.
set.seed(1234)
map(.x = list(-10, 0, 10, 100), 
    .f = ~ rnorm(n = 10, mean = .))

# ¿Cómo puedes crear un vector tal que para cada columna en 
# un data frame indique si corresponde o no a un factor?
map_lgl(.x = flores, .f = is.factor)

# ¿Qué ocurre si usas las funciones map en vectores que no
# son listas? ¿Qué hace map(1:5, runif)? ¿Por qué?

## map acepta una lista o un vector atómico
## Esta función se utiliza para obtener números
## aleatorios de una distribución uniforme donde
## en este caso se está iterando sobre 1:5 que
## corresponden a el número de observaciones
set.seed(1234)

# ¿Qué hace map(-2:2, rnorm, n = 5)? ¿Por qué? 
# ¿Qué hace map_dbl(-2:2, rnorm, n = 5)? ¿Por qué?
# map(1:5, runif)
set.seed(1234)
map(-2:2, rnorm, n = 5)

## Lo anterior es equivalente a la siguiente 
## función donde se esta iterando sobre -2:2
## que corresponde a la media de la distribución
## normal
set.seed(1234)
map(-2:2, ~ rnorm(n = 5, mean = .))

# Reescribe map(x, function(df) lm(mpg ~ wt, data = df)) 
# para eliminar todas las funciones anónimas.
map(.x = split(x = mtcars, f = mtcars$cyl), 
    .f = lm, formula = mpg ~ wt)

map(.x = split(x = mtcars, f = mtcars$cyl), 
    .f = lm, formula = mpg ~ wt) %>% 
  # Utilizando broom para poder extraer mejor 
  # los datos
  map(.f = tidy) %>% 
  set_names(nm = c("cyl_4", "cyl_6", "cyl_8"))

# 21.9.3 Ejercicios ----

# Implementa tu propia versión de every() 
# usando un bucle for. Compárala con purrr::every(). 
# ¿Qué hace la versión de purrr que la tuya no?

every_2 <- function(x, f, ...) {
  
  for (i in seq_along(x)) {
    value <- f(x[[i]], ...)
    if (isFALSE(value)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

every_2(x = list(1, 3, "a"), f = is.numeric)


# Crea una mejora de col_resumen() que aplique una 
# función de resumen a cada columna numérica en un 
# data frame.
col_resumen <- function(df, fun) {
  df  <- df[ , sapply(df, is.numeric)]
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[[i]] <- fun(df[[i]])
  }
  out <- setNames(object = out, nm = names(df))
  out
}

col_resumen(df = flores, fun = mean)

# Un posible equivalente de col_resumen() es:
col_resumen3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  
  sapply(df_num, f)
}

df <- tibble(
  x = 1:3, 
  y = 3:1,
  z = c("a", "b", "c")
)

col_resumen3(df, mean)
class(col_resumen3(df, mean))  

## No veo problemas aquí
col_resumen3(df[1:2], mean)
class(col_resumen3(df[1:2], mean))

## No veo problemas aquí
col_resumen3(df[1], mean)
class(col_resumen3(df[1], mean))

## Este si no aplica dado que 
## no se puede hacer subsetting
## con un índice que no existe
col_resumen3(df[0], mean)