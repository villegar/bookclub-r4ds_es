# Librerias ----
library(tidyverse)
library(datos)
library(broom)
library(ggbeeswarm)
library(modeest)

# 25.2.5 Ejercicios ----

# Una tendencia lineal parece ser demasiado simple 
# para la tendencia general. ¿Puedes hacerlo mejor 
# con un polinomio cuadrático? ¿Cómo puedes interpretar 
# el coeficiente del término cuadrático? (Pista: puedes 
# querer transformar year para que tenga media cero.)
datos::paises %>% 
  nest(data = anio:pib_per_capita) %>% 
  mutate(model = map(data, ~ lm(data = ., 
                                formula = esperanza_de_vida ~ anio + I(anio^2))),
         residuals = map(model, ~ augment(.)$.resid)) %>% 
  unnest(c(data, residuals)) %>% 
  ggplot(aes(anio, residuals)) + 
  geom_line(aes(group = pais)) + 
  geom_hline(yintercept = 0, color = "red") + 
  geom_smooth() + 
  facet_wrap(facets = vars(continente))

datos::paises %>% 
  nest(data = anio:pib_per_capita) %>% 
  mutate(model = map(data, ~ lm(data = ., 
                                formula = esperanza_de_vida ~ anio + I(anio^2))),
         r_2 = map(model, ~ glance(.)$r.squared)) %>% 
  unnest(c(data, r_2)) %>% 
  ggplot(aes(continente, r_2)) + 
  geom_point(aes(fill = continente),
             shape = 21,
             color = "black",
             size = 2,
             position = position_jitter(width = 0.1))

# Mejora un poco con relación al modelo lineal
# Los residuos postivios disminuyen
# El R2 es más alto para algunos países
# Sin embargo se sigue presentando el problema 
# con países de algunos países de África y Asia

# Explora otros métodos para visualizar la distribución 
# del R2 por continente. Puedes querer probar el paquete 
# ggbeeswarm, que provee métodos similares para evitar 
# superposiciones como jitter, pero usa métodos determinísticos.
datos::paises %>% 
  nest(data = anio:pib_per_capita) %>% 
  mutate(model = map(data, ~ lm(data = ., 
                                formula = esperanza_de_vida ~ anio + I(anio^2))),
         r_2 = map(model, ~ glance(.)$r.squared)) %>% 
  unnest(c(data, r_2)) %>% 
  ggplot(aes(continente, r_2)) + 
  # Se ve un poco más ordenado el gráfico
  geom_quasirandom(aes(fill = continente),
                   shape = 21,
                   color = "black",
                   size = 2)

# Para crear el último gráfico (mostrando los datos para 
# los países con los peores ajustes del modelo), precisamos 
# dos pasos: creamos un data frame con una fila por país y 
# después hicimos un semi-join al conjunto de datos original.
# Es posible evitar este join si usamos unnest() en lugar de 
# unnest(.drop = TRUE). ¿Cómo?
datos::paises %>% 
  nest(data = anio:pib_per_capita) %>% 
  mutate(model = map(data, ~ lm(data = ., 
                                formula = esperanza_de_vida ~ anio + I(anio^2))),
         r_2 = map_dbl(model, ~ glance(.)$r.squared)) %>%
  slice_min(order_by = r_2, n = 6) %>% 
  unnest(c(data)) %>% 
  ggplot(aes(anio, esperanza_de_vida)) + 
  # Aparecen tanto países que ya habían aparecido
  # como páises que no habían aparecido
  # Paises que ya habían aparecido: Bostwana, Lesoto
  # Ruanda
  # Paises que no habían aparecido: Camboya, Liberia,
  # Uganda
  geom_line(aes(group = pais, color = pais))

# 25.4.5 Ejercicios ----

# Lista todas las funciones en las que puedas pensar 
# que tomen como input un vector atómico y retornen 
# una lista.
datos::paises %>% 
  nest(data = anio:pib_per_capita) %>% 
  # stringr: str_split, str_extract_all
  mutate(split_pais = str_split(pais, ""),
         extract_a  = str_extract_all(pais, "[aáAÁ]")) %>% 
  # La función map 
  # claro que en este caso estoy tomando una lista de tibbles
  # y retornando una lista pero también acepta vectores
  # atómicos
  mutate(model = map(data, ~ lm(data = .,
                                formula = pib_per_capita ~ anio)))

# Piensa en funciones de resumen útiles que, como quantile(), 
# retornen múltiples valores.

# En el caso de la moda entendiendo como
# el valor que más se repite. En este caso 
# puede que la moda no sea única donde se 
# puede retornar más de un valor
tibble(data = list(c(3,3,2,2), # Aqui la moda es 3, 2
                   c(1,2,3,3), # Aquí la moda es 3
                   c(1,2,3))) %>%  # Aquí la moda es 1, 2, 3
  mutate(mode = map(data, modeest::mfv))

# ¿Qué es lo que falta en el siguiente data frame? 
# ¿Cómo quantile() retorna eso que falta? ¿Por qué eso 
# no es tan útil aquí?
mtautos %>%
  group_by(cilindros) %>%
  summarise(q = list(quantile(millas))) %>%
  unnest(q)

## Usualmente uno quiere saber cada valor a que 
## cuantil pertenece
### La función lo indica nombrando el vector
quantile(1:10, 
         probs = c(0, 0.25, 0.5, 0.75, 1))

## unnest los elimina
## debido a que summarize si los sigue indicando
mtautos %>%
  group_by(cilindros) %>%
  summarise(q = list(quantile(millas))) %>% 
  .$q %>% 
  .[1]

# ¿Qué hace este código? ¿Por qué podría ser útil?
mtautos %>%
  group_by(cilindros) %>%
  # summarize_each() ya no se esta utilizando
  # fue reemplazada por across()
  # También funs() ya no se utiliza
  summarise(across(everything(), 
                   .fns = list))

## A cada columna la esta convirtiendo en una lista
## donde con group se está agrupando por cilindraje
## Sin embargo no se para que nos podría servir 

# 25.5.3 Ejercicios ----

# ¿Por qué podría ser útil la función lengths() para crear 
# columnas de vectores atómicos a partir de columnas-lista?
?lengths
# No conocia esta función
# aplica length para cada elemento
# de una lista
list(1, 1:2, 1:3, 1:4) %>% lengths()

paises %>% 
  nest(data = anio:pib_per_capita) %>%
  # No necesito utilizar map en un mutate
  # para verificar el número de columnas
  # dado que lengths funciona con listas
  mutate(columns = lengths(data))

# Lista los tipos de vectores más comúnes que se 
# encuentran en un data frame. ¿Qué hace que las 
# listas sean diferentes?

## Ver Capítulo 20 > 20.3 Tipos importantes de vectores atómicos
## https://es.r4ds.hadley.nz/vectores.html
## Lógico
## Numérico
## Caracter

## También ver 
## Capítulo 20 > 20.4 Usando vectores atómicos
## > 20.4.1 Coerción
## "Un vector atómico no puede contener una mezcla de 
## diferentes tipos, ya que el tipo es una propiedad 
## del vector completo, no de los elementos individuales. 
## Si necesitas mezclar diferentes tipos en el mismo vector, 
## entonces deberías utilizar una lista"
## 