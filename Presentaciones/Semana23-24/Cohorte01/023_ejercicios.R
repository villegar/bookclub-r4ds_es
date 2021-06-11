# Librerias ----
library(tidyverse)
library(datos)
library(patchwork)
library(modelr)
library(broom)

# 23.2.1 Ejercicios ----

# Una desventaja del modelo lineal es ser sensible 
# a valores inusuales debido a que la distancia incorpora 
# un término al cuadrado. Ajusta un modelo a los datos 
# simulados que se presentan a continuación y visualiza 
# los resultados. Corre el modelo varias veces para generar 
# diferentes conjuntos de datos simulados. ¿Qué puedes 
# observar respecto del modelo?
set.seed(1234)
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)

set.seed(1234)
sim1b <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rnorm(length(x))
)

g1 <- sim1a %>% 
  ggplot(aes(x,y)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ x) + 
  labs(title = "Student t Distribution")

g2 <- sim1b %>% 
  ggplot(aes(x,y)) + 
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ x) + 
  labs(title = "Normal Distribution")

g1 + g2

# Una forma de obtener un modelo lineal más 
# robusto es usar una métrica distinta para 
# la distancia. Por ejemplo, en lugar de la 
# raíz de la distancia media cuadrática (del 
# inglés root-mean-squared distance) se podría 
# usar la media de la distancia absoluta:

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

best <- optim(c(0, 0), measure_distance, data = sim1a)

sim1a %>% 
  ggplot(aes(x,y)) +
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x, se = FALSE) + 
  geom_abline(intercept = best$par[[1]], 
              slope = best$par[[2]],
              color = "red")
  
# Un desafío al realizar optimización numérica 
# es que únicamente garantiza encontrar un óptimo 
# local. ¿Qué problema se presenta al optimizar un 
# modelo de tres parámetros como el que se presenta 
# a continuación?
model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}

best0 <- optim(c(0, 0, 0), measure_distance, data = sim1a)
best0$par

best1 <- optim(c(1, 1, 1), measure_distance, data = sim1a)
best1$par

## Encuentra optimos locales donde por ejemplo
## best0$par es diferente a best1$par

# 23.3.3 Ejercicios ----

# En lugar de usar lm() para ajustar una línea recta, 
# puedes usar loess() para ajustar una curva suave. 
# Repite el proceso de ajustar el modelo, generar 
# la cuadrícula, predicciones y visualización con 
# sim1 usando loess() en vez de lm(). ¿Cómo se compara 
# el resultado a geom_smooth().
sim1a %>% 
  ggplot(aes(x,y)) +
  geom_point() + 
  geom_smooth(method = "lm",
              formula = y ~ x, se = FALSE,
              color = "blue") + 
  geom_smooth(method = "loess",
              span = 0.75,
              formula = y ~ x, se = FALSE,
              color = "red") 

# add_predictions() está pareada con gather_predictions() 
# y spread_predictions(). ¿Cómo difieren estas tres 
# funciones?
?add_predictions
?gather_predictions
?spread_predictions

model1 <- lm(data = sim1a, formula = y ~ x)
model2 <- lm(data = sim1a, formula = y ~ poly(x, degree = 3))

add_predictions(data = sim1a, model = model1)
## En cada columna existe la predicción de cada modelo
spread_predictions(data = sim1a, model1, model2)
## Existe una columna que identifica al modelo y 
## otra columna que contiene las predicciones
gather_predictions(data = sim1a, model1, model2)

## ¿Qué hace geom_ref_line()? ¿De qué paquete proviene? 
## ¿Por qué es útil e importante incluir una línea de 
## referencia en los gráficos que muestran residuos?
?geom_ref_line

sim1a %>% 
  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x, se = FALSE,
              color = "blue") + 
  geom_ref_line(h = mean(sim1a$y), 
                colour = "red", size = 0.5)
## Aplicación para caso de los errores
add_predictions(data = sim1a, model = model1) %>% 
  mutate(error = y - pred) %>% 
  ggplot(aes(x, error)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,
              formula = y ~ x) + 
  geom_ref_line(h = 0, 
                colour = "red", size = 0.5)

# ¿Por qué quisieras mirar un polígono de frecuencias 
# con los residuos absolutos? ¿Cuáles son las ventajas 
# y desventajas de los residuos crudos?

## Usando geom_freqpoly con errores 
## No se elimina el signo y se verifica si
## esta alrededor de cero
add_predictions(data = sim1a, model = model1) %>% 
  mutate(error = y - pred) %>% 
  ggplot(aes(error)) + 
  geom_freqpoly(bins = 10)

## Usando geom_freqpoly con errores absolutos
## Eliminanos el signo y verificamos si estan cerca
## de cero
add_predictions(data = sim1a, model = model1) %>% 
  mutate(error = abs(y - pred)) %>% 
  ggplot(aes(error)) + 
  geom_freqpoly(bins = 10)

# 23.4.5 Ejercicios ----

# ¿Qué pasa si repites el análisis de sim2 usando 
# un modelo sin intercepto? ¿Qué ocurre con la ecuación 
# del modelo? ¿Qué ocurre con las predicciones?

modelr::sim2
modelo_con_intercept <- lm(data = sim2, formula = y ~ x)
modelo_con_intercept
## En este caso la interpretación de los coeficientes
## es diferente
modelo_sin_intercept <- lm(data = sim2, formula = y ~ x)
lm(data = sim2, formula = y ~ x - 1)
## Sin embargo las prediciones son iguales
spread_predictions(data = sim2, 
                   modelo_con_intercept,
                   modelo_sin_intercept) %>% 
  mutate(diff = modelo_con_intercept == modelo_sin_intercept) %>% 
  count(diff)

# Usa model_matrix() para explorar las ecuaciones generadas 
# por los modelos ajustados a sim3 y sim4. ¿Por qué * es un 
# atajo para la interacción?
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
model_matrix(data = sim3, formula = y ~ x1 + x2)
## x1*x2 corresponde a x1 + x2 + x1:x2
## Como x2 es una variable categorica y tenemos 
## un intercepto existen (categorias x2 - 1)
## interacciones
model_matrix(data = sim3, formula = y ~ x1*x2)

# Usando los principios básicos, convierte las fórmulas 
# de los siguientes modelos en funciones. (Sugerencia: 
# comienza por convertir las variables categóricas en 
# ceros y unos.)
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

## Esta parte no la entiendo

# Para sim4, ¿Es mejor mod1 o mod2? Yo creo que 
# mod2 es ligeramente mejor removiendo las tendencias, 
# pero es bastante sutil. ¿Puedes generar un gráfico que 
# de sustento a esta hipótesis?
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

# broom::glance(mod1)
broom::augment(mod1)

tibble(models = list(mod1 = mod1, mod2 = mod2)) %>% 
  mutate(id = names(models),
         # Aquí parece que mod2 se ajusta mejor a los datos
         # en el caso de un modelo lineal
         # Por otro lado el rmse es menor para mod2
         r2 = purrr::map_dbl(models, .f = ~ glance(.)[[1]]),
         r2_adjusted = purrr::map_dbl(models, .f = ~ glance(.)[[2]]),
         rmse = purrr::map_dbl(models, .f = ~ sqrt(mean((augment(.)$.resid)^2))))
