# install.packages('datos')
library(datos)
library(tidyverse)

# Los tibbles son data frames, pero ligeramente ajustados para que
# funcionen mejor en el tidyverse. 
filter(vuelos, mes == 11 | mes == 12)
nov_dic <- filter(vuelos, mes %in% c(11,12))
filter(vuelos,!(atraso_llegada) > 120 | atraso_salida > 120)
filter(vuelos, atraso_llegada <= 120, atraso_salida <= 120)

# Valores faltantes
is.na(x)
df <- tibble(x = c(1,NA,3))
filter(df,x > 1)
filter(df,is.na(x) | x > 1)

# Reordenar las filas
arrange(vuelos, anio, mes, dia)
arrange(vuelos, desc(atraso_salida))
df <- tibble(x = c(5,2,NA))
arrange(df,x)
# Los valores faltantes siempre se ordenan al final
arrange(df, desc(x))

# Seleccionar columnas con select()
select(vuelos,anio,mes,dia)
select(vuelos,anio:dia)
# Seleccionar todas las columnas excepto aquellas entre anio en dia (incluyente)
select(vuelos, -(anio:dia))
rename(vuelos, cola_num = codigo_cola)

select(vuelos, fecha_hora, tiempo_vuelo, everything())
mi_arreglo <- array(c(1:18),dim = (3,3,2))

vuelos_sml <- select(vuelos,
                     anio:dia,
                     starts_with('atraso'),
                     distancia,
                     tiempo_vuelo)
mutate(vuelos_sml,
       ganancia = atraso_salida - atraso_llegada,
       velocidad = distancia / tiempo_vuelo * 60)

mutate(vuelos_sml,
       ganancia = atraso_salida - atraso_llegada,
       horas = tiempo_vuelo / 60,
       ganancia_por_hora = ganancia / horas)

transmute(vuelos,
          ganancia = atraso_salida - atraso_llegada,
          horas = tiempo_vuelo / 60,
          ganacia_por_hora = ganacia / horas)

transmute(vuelos,
          horario_salida,
          hora = horario_salida %/% 100,
          minuto = horario_salida %% 100)

(x <- 1:10)
lag(x)
lead(x)

x
cumsum(x)
cummean(x)

y <- c(1,2,2,NA,3,4)
min_rank(y)
min_rank(desc(y))

row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

# summarise()
summarise(vuelos, atraso = mean(atraso_salida,na.rm = TRUE))

# Retraso promedio por fecha:
por_dia <- group_by(vuelos,anio,mes,dia)
summarise(por_dia, atraso = mean(atraso_salida, na.rm = TRUE))

# Combinación de múltiples operaciones con el pipe
por_destino <- group_by(vuelos,destino)
atraso <- summarise(por_destino,
                    conteo = n(),
                    distancia = mean(distancia,na.rm = TRUE),
                    atraso = mean(atraso_llegada,na.rm = TRUE))

atraso <- filter(atraso,conteo > 20, destino != 'HNL')

# Parece que las demoras aumentan con las distancias hasta ~ 750 millas
# y luego disminuyen. ¿Tal vez a medida que los vuelos se hacen más
# largos, hay más habilidad para compensar las demoras en el aire?

ggplot(data = atraso, mapping = aes(x = distancia, y = atraso)) + 
  geom_point(aes(size = conteo), alpha = 1/3) +
  geom_smooth(se = FALSE)

#> `geom_smooth()`using method = 'loess' and formula 'y ~ x'

atrasos <- vuelos %>%
  group_by(destino) %>%
  summarise(
    conteo = n(),
    distancia = mean(distancia, na.rm = TRUE),
    atraso = mean(atraso_llegada, na.rm = TRUE)
  ) %>%
  filter(conteo > 20, destino != 'HNL')

#> 'summarise()' ungrouping output (override with 
#  '.groups' argument)

vuelos %>%
  group_by(anio,mes,dia) %>%
  summarise(mean = mean(atraso_salida))

# 'summarise()' regrouping output by 'anio','mes'
# (override with '.groups' argument)

vuelos %>%
  group_by(anio,mes,dia) %>%
  summarise(mean = mean(atraso_salida,na.rm = TRUE))

no_cancelados <- vuelos %>%
  filter(!is.na(atraso_salida),!is.na(atraso_llegada))

no_cancelados %>% 
  group_by(anio,mes,dia) %>%
  summarise(mean = mean(atraso_salida))

atrasos <- no_cancelados %>%
  group_by(codigo_cola) %>%
  summarise(
    atraso = mean(atraso_llegada)
  )

ggplot(data = atrasos, mapping = aes(x = atraso)) + 
  geom_freqpoly(binwidth = 10)

atrasos <- no_cancelados %>%
  group_by(codigo_cola) %>%
  summarise(
    atraso = mean(atraso_llegada, na.rm = TRUE),
    n = n()
  )

ggplot(data = atrasos, mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)

atrasos %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x = n, y = atraso)) + 
  geom_point(alpha = 1/10)

# Convierte a tibble para que puedas imprimirlo de una manera legible
bateo <- as_tibble(datos::bateadores)

rendimiento_bateadores <- bateo %>%
  group_by(id_jugador) %>%
  summarise(
    pb = sum(golpes, na.rm  = TRUE) / sum(al_bate,na.rm = TRUE),
    ab = sum(al_bate,na.rm = TRUE)
  )

rendimiento_bateadores %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = pb)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

desc(pb)

rendimiento_bateadores %>%
  arrange(desc(pb))

no_cancelados %>%
  group_by(anio, mes, dia) %>%
  summarise(
    prom_atraso1 = mean(atraso_llegada),
    prom_atraso2 = mean(atraso_llegada[atraso_llegada > 0])
    # El promedio de atrasos positivos
  )


# Medidas de dispersión: sd(x), IQR(x), mad(x)
# IQR() -> Rango intercuartil
# mad(x) -> Desviación media absoluta

# ¿Por qué la distancia a algunos destinos es más variable que la de otros?
no_cancelados %>%
  group_by(destino) %>%
  summarise(distancia_sd = sd(distancia)) %>%
  arrange(desc(distancia_sd))

# ¿Cuándo salen los primeros y los últimos vuelos cada día?
no_cancelados %>%
  summarise(
    primero = min(horario_salida),
    ultimo = max(horario_salida)
  )

no_cancelados %>%
  group_by(anio, mes, dia) %>%
  summarise(
    primera_salida = first(horario_salida),
    ultima_salida = last(horario_salida)
  )

no_cancelados %>%
  group_by(anio, mes, dia) %>%
  mutate(r = min_rank(desc(horario_salida))) %>%
  filter(r %in% range(r))

# Para contar la cantidad de valores no faltantes
sum(!is.na(x))
# Para contar la cantidad de valores distintos (únicos)
n_distinct(x)

# ¿Qué destinos tienen la mayoría de las aerolíneas?
no_cancelados %>%
  group_by(destino) %>%
  summarise(aerolineas = n_distinct(aerolinea)) %>%
  arrange(desc(aerolineas))

no_cancelados %>%
  count(destino)

count(codigo_cola, wt = distancia)

