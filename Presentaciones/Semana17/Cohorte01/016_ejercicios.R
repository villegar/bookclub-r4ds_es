# Librerias ----
library(tidyverse)
library(lubridate)
library(datos)

# 16.2.4 Ejercicios ----

#¿Qué sucede si analizas una cadena de caracteres que 
# contiene fechas inválidas?

## Genera un mensaje de advertencia
## convierte el valor problemático en NA
ymd(c("2010-10-10", "bananas"))

## Este es el equivalente de ymd
## Consultar https://rdrr.io/github/tidyverse/lubridate/src/R/parse.r
as.Date.POSIXct(parse_date_time(x = c("2010-10-10", "bananas"), 
                                orders = "ymd", quiet = FALSE,
                                tz = "UTC", truncated = 0,
                                locale = Sys.getlocale("LC_TIME")))

# ¿Qué hace el argumento tzone (time zone = huso horario)
# para today()? ¿Por qué es importante?
?today
?OlsonNames

## +12:00
today(tzone = "Pacific/Fiji")
## −11:00
today(tzone = "US/Samoa")

# Utiliza la función de lubridate apropiada para analizar 
# las siguientes fechas:
d1 <- "Enero 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("Agosto 19 (2015)", "Julio 1 (2015)")
d5 <- "12/30/14"

##
parse_date_time(x = d1, orders = "mdy", locale = "es_CO.utf8")
ymd(d2, locale = "en_US.UTF-8")
parse_date_time(x = d3, orders = "dmy", locale = "es_CO.utf8")
parse_date_time(x = d4, orders = "mdy", locale = "es_CO.utf8")
parse_date_time(x = d5, orders = "mdy", locale = "es_CO.utf8")

# 16.3.4 Ejercicios ----

# ¿Cómo cambia la distribución de las horas de los vuelos 
# dentro de un día a lo largo del año?

## Se requiere utilizar el siguiente código
hacer_fechahora_100 <- function(anio, mes, dia, tiempo) {
  make_datetime(anio, mes, dia, tiempo %/% 100, tiempo %% 100)
}

vuelos_dt <- vuelos %>%
  filter(!is.na(horario_salida), !is.na(horario_llegada)) %>%
  mutate(
    horario_salida = hacer_fechahora_100(anio, mes, dia, horario_salida),
    horario_llegada = hacer_fechahora_100(anio, mes, dia, horario_llegada),
    salida_programada = hacer_fechahora_100(anio, mes, dia, salida_programada),
    llegada_programada = hacer_fechahora_100(anio, mes, dia, llegada_programada)
  ) %>%
  select(origen, destino, starts_with("atraso"), 
         starts_with("horario"), 
         ends_with("programada"), 
         tiempo_vuelo)

vuelos_dt %>% 
  mutate(horario_salida = update(horario_salida, yday = 1)) %>%
  ggplot(aes(horario_salida)) + 
  geom_histogram(binwidth = 60*60, 
                 boundary = 0, 
                 color = "black") + 
  scale_x_datetime(date_labels = "%H:%S", 
                   date_breaks = "1 hour", 
                   date_minor_breaks = "1 hour") + 
  coord_cartesian(xlim = c(ymd_hms("2013-01-01 00:30:00"), 
                           ymd_hms("2013-01-01 23:30:00"))) +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5))

# Compara horario_salida, salida_programada y atraso_salida. 
# ¿Son consistentes? Explica tus hallazgos.
vuelos_dt %>% 
  mutate(horario_salida2 = salida_programada + minutes(atraso_salida)) %>% 
  filter(horario_salida2 != horario_salida) %>%
  mutate(diff = horario_salida2 - horario_salida) %>% 
  select(horario_salida, horario_salida2, diff) %>% 
  ## problemas con la función hacer_fechahora_100 
  count(diff)

# Compara tiempo_vuelo con la duración entre la salida y la llegada. 
# Explica tus hallazgos. (Pista: considera la ubicación del aeropuerto).
vuelos_dt %>%
  select(origen, destino, horario_salida, horario_llegada, tiempo_vuelo) %>% 
  mutate(tiempo_vuelo2 = horario_llegada - horario_salida,
         tiempo_vuelo2 = as.numeric(tiempo_vuelo2), 
         diff = abs(tiempo_vuelo - tiempo_vuelo2)) %>% 
  filter(diff == 0) %>% 
  count(origen, destino, sort = TRUE)

# ¿Cómo cambia la demora promedio durante el curso de un día? 
# ¿Deberías usar horario_salida o salida_programada? ¿Por qué?
vuelos_dt %>% 
  # utilizo la salida_programada dado que esa es la hora de 
  # referencia que tienen las personas para planear estar en
  # en el aeropuerto
  mutate(hora_salida_programada = hour(salida_programada)) %>% 
  select(salida_programada, hora_salida_programada, atraso_salida) %>% 
  group_by(hora_salida_programada) %>% 
  summarize(media_atraso_salida = mean(atraso_salida)) %>% 
  ggplot(aes(hora_salida_programada, media_atraso_salida)) + 
  geom_point() +
  geom_line()

# ¿En qué día de la semana deberías salir si quieres minimizar 
# las posibilidades de una demora?
vuelos_dt %>% 
  mutate(dia_salida_programada = wday(salida_programada, 
                                      label = TRUE, 
                                      abbr = FALSE, 
                                      week_start = 1)) %>%
  group_by(dia_salida_programada) %>% 
  summarize(media_atraso_salida = mean(atraso_salida)) %>% 
  ggplot(aes(dia_salida_programada, media_atraso_salida)) + 
  geom_col()

# ¿Qué hace que la distribución de diamantes$quilate y 
# vuelos$salida_programada sea similar?
datos::diamantes %>% 
  ggplot(aes(quilate)) + 
  geom_histogram(boundary = 0, binwidth = 0.05, color = "black") +
  scale_x_continuous(breaks = seq.int(from = 0, to = 6, by = 0.25)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

vuelos_dt %>% 
  mutate(min_salida_programada = minute(salida_programada),
         min_salida_programada = as.numeric(min_salida_programada)) %>% 
  ggplot(aes(min_salida_programada)) + 
  geom_histogram(boundary = 0, binwidth = 1, color = "black",
                 # incluir el límite inferior en el bin
                 closed = "left") +
  scale_x_continuous(breaks = seq.int(from = 0, to = 59, by = 5))

# Confirma nuestra hipótesis de que las salidas programadas en los
# minutos 20-30 y 50-60 están casuadas por los vuelos programados 
# que salen más temprano. Pista: crea una variable binaria que te 
# diga si un vuelo tuvo o no demora.
vuelos_dt %>% 
  mutate(min_salida_programada = minute(salida_programada), 
         vuelo_temprano        = ifelse(atraso_salida < 0, 
                                        1, 0)) %>% 
  select(min_salida_programada, vuelo_temprano, atraso_salida) %>%
  group_by(min_salida_programada) %>% 
  summarize(prop_vuelo_temprano = mean(vuelo_temprano, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(min_salida_programada, prop_vuelo_temprano,
             color = min_salida_programada %in% c(20:30, 50:60))) +
  geom_point(show.legend = FALSE) +
  geom_line(aes(group = 1), show.legend = FALSE) + 
  scale_x_continuous(breaks = seq.int(from = 0, to = 60, by = 5)) 

# 16.4.5 Ejercicios ----

# ¿Por qué hay months() pero no dmonths() (días del mes)?
?months
?dmonths

## Existe. Sin embargo entiendo el sentido de esta pregunta
## dado que se necesita especificar un número fijo de segundos
## para un mes
dmonths(x = 1)
60*60*24*7*4.348214

# Explica days(nocturno * 1) a alguien que apenas comienza a 
# aprender R. ¿Cómo funciona?

vuelos_dt <- vuelos_dt %>%
  mutate(
    nocturno = horario_llegada < horario_salida,
    horario_llegada = horario_llegada + days(nocturno * 1),
    llegada_programada = llegada_programada + days(nocturno * 1)
  )

## La columna nocturno genera un valor lógico
## En el caso en que horario_llegada < horario_salida no
## se cumpla nocturno * 1 genera un valor de days(0) por lo
## que se obtendrían 0 dias
## En el caso que horario_llegada < horario_salida si se cumpla
## se generarían days(1) por lo que se obtendrían 0 días

# Crea un vector de fechas dando el primer día de cada mes de 2015. 
# Crea un vector de fechas dando el primer día de cada mes del año 
# actual.
seq.Date(from = ymd("2015-01-01"), 
         by = "month", 
         length.out = 12)

ymd("2015-01-01") + months(x = 0:11)

(today(tzone = "America/Bogota") %>% 
  update(yday = 1)) + months(x = 0:11) 

# Crea una función en la que, dado tu cumpleaños 
# (como una fecha), retorne qué edad tienes en años.

## Problema con el huso horario
edad <- function(year, month, day) {
  interval(start = make_date(year = year, 
                             month = month, 
                             day = day), 
           end = today()) / years(x = 1)
}

## Mejora
edad <- function(year, month, day) {
  interval(start = make_date(year = year, 
                             month = month, 
                             day = day), 
           end   = today(tzone = Sys.timezone(location = TRUE)), 
           tzone = Sys.timezone(location = TRUE)) / years(x = 1)
}

edad(year = 1986, month = 6, day = 5)

# ¿Por qué no funciona (today() %--% (today() + years(1)) / months(1) ?

## Si se incluye un paréntesis funciona
## El original le falta un paréntesis
### Que ejercicio tan extraño
(today() %--% (today() + years(1)) / months(1))

## No funciona en un añop bisiesto
(ymd("2020-02-29") %--% (ymd("2020-02-29") + years(1)) / months(1))