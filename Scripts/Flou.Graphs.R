
# Suponiendo que 'datos' es tu dataframe
# Filtrar datos para obtener los grupos específicos

datos <- Fluo.Data
datos_filtrados <- datos[datos$Area_Head_sqrmicro >= 0 & datos$Area_Head_sqrmicro <= 1.5 & datos$Ratio > 50,]

# Si se consideran los datos como un todo, no es posible encontrar diferencias importantes.

p <- ggbetweenstats(datos_filtrados, Short_ATF, Area_Head_sqrmicro, grouping.var = ATF, type = "np")

# Es necesario mirar la distribución de los datos y realizar una comparación de la distribución.

NoED <- datos_filtrados %>%
  filter(Short_ATF == "NoED") %>%
  pull(Area_Head_sqrmicro)

VP64 <- datos_filtrados %>%
  filter(Short_ATF == "VP64") %>%
  pull(Area_Head_sqrmicro)

# Calcular frecuencias acumuladas
freq_acum_grupo1 <- ecdf(NoED)
freq_acum_grupo2 <- ecdf(VP64)

# Crear un dataframe para ggplot2
df_freq_acum <- data.frame(
  x = seq(min(datos_filtrados$Area_Head_sqrmicro), max(datos_filtrados$Area_Head_sqrmicro), length.out = 100),
  Grupo1 = freq_acum_grupo1(seq(min(NoED), max(NoED), length.out = 100)),
  Grupo2 = freq_acum_grupo2(seq(min(VP64), max(VP64), length.out = 100))
)

# Crear el gráfico
ggplot(df_freq_acum, aes(x = x)) +
  geom_line(aes(y = Grupo1, color = "NoED"), size = 1) +
  geom_line(aes(y = Grupo2, color = "VP64"), size = 1) +
  labs(title = "Gráfico de Frecuencia Acumulada",
       x = "Área de la Cabeza (micrones cuadrados)",
       y = "Frecuencia Acumulada") +
  scale_color_manual(values = unique(datos$ColorCode)) +
  theme_classic()

#-----
# Suponiendo que df_freq_acum contiene las frecuencias acumuladas para los dos grupos
ks_test <- ks.test(df_freq_acum$Grupo1, df_freq_acum$Grupo2)
print(ks_test)

# Tamaños de las muestras
n1 <- length(df_freq_acum$Grupo1)
n2 <- length(df_freq_acum$Grupo2)

# Estadístico KS
D <- ks_test$statistic
print(D)

# Calcular Hedges' g
g_hedges <- D * sqrt((n1 * n2) / (n1 + n2))
print(g_hedges)

####



# Suponiendo que tus datos están en un dataframe llamado datos
correlation <- cor(datos_filtrados$Length_spine_micro, datos$Area_Head_sqrmicro)
correlation

# Gráfico de dispersión por categoría
ggplot(datos_filtrados, aes(x = Length_spine_micro, y = Area_Head_sqrmicro, color = ATF)) +
  geom_point() +
  facet_wrap(~ATF) +
  labs(title = "Relación entre Longitud y Tamaño de la Cabeza por Categoría",
       x = "Longitud de la Espina (micrones)",
       y = "Área de la Cabeza (micrones cuadrados)") +
  scale_color_manual(values = unique(datos_filtrados$ColorCode)) +
  theme_classic()

