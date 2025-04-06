library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

archivo <- "Datos_Proyecto.xlsx"
datos <- read_excel(archivo)
str(datos)
print(datos)

#Convertir datos
datos <- datos %>%
  mutate(
    `NOTA ESTADÍSTICA` = as.numeric(as.character(`NOTA ESTADÍSTICA`)),
    `NOTA ÁLGEBRA` = as.numeric(as.character(`NOTA ÁLGEBRA`)),
    `NOTA CÁLCULO` = as.numeric(as.character(`NOTA CÁLCULO`)),
    `NOTA FUND. PROG.` = as.numeric(as.character(`NOTA FUND. PROG.`))
  )


#Calcular el potencial
datos <- datos %>%
  mutate(promedio = ifelse(is.na(`NOTA ÁLGEBRA`), 
                           rowMeans(select(., `NOTA ESTADÍSTICA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE),
                           rowMeans(select(., `NOTA ESTADÍSTICA`, `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE)
  ))


##########################################################################

      #ANALISIS DE POTENCIAL POR CANTIDAD DE MATERIAS VISTAS

datos <- datos %>%
  mutate(cantidad_de_materias = ifelse(is.na(`NOTA ÁLGEBRA`), 3, 4))

# ------- TRES MATERIAS ---------------
estudiantes_3_materias <- datos %>%
  filter(cantidad_de_materias == 3)

estadisticas_3_promedio <- estudiantes_3_materias %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )

# Mostrar las estadísticas
print("Estadísticas para estudiantes que ven 3 materias (promedio):")
print(estadisticas_3_promedio)
print("Gráfica de Sesgo:")
estudiantes_3_materias <- estudiantes_3_materias %>%
  mutate(rango_promedio = cut(promedio, breaks = c(4, 5, 6, 7, 8, 9, 10), 
                              labels = c("4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                              right = FALSE))

# Graficar histograma agrupado por los rangos de promedio
ggplot(estudiantes_3_materias, aes(x = rango_promedio)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Promedios - Estudiantes que ven 3 materias",
       x = "Rango de Promedio", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  



# ------- CUATRO MATERIAS ---------------
estudiantes_4_materias <- datos %>%
  filter(cantidad_de_materias == 4)

estadisticas_4_promedio <- estudiantes_4_materias %>%
  summarise(
    media = mean(promedio, na.rm = TRUE), # na.rm(na.remove) = evitamos los datos perdidos los NA
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )


estudiantes_4_materias <- estudiantes_4_materias %>%
  mutate(rango_promedio = cut(promedio, breaks = c(4, 5, 6, 7, 8, 9, 10), 
                              labels = c("4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                              right = FALSE))


# Graficar histograma agrupado por los rangos de promedio
ggplot(estudiantes_4_materias, aes(x = rango_promedio)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Promedios - Estudiantes que ven 4 materias",
       x = "Rango de Promedio", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 


# ------- BOXPLOT CONJUNTO ---------------
promedios_3_materias <- estudiantes_3_materias$promedio
promedios_4_materias <- estudiantes_4_materias$promedio


boxplot(promedios_3_materias, promedios_4_materias, names = c("Est. con 3 materias", "Est. con 4 materias"),
        main = "Promedio de Estudiantes", ylab = "Potencial", col = c("blue", "green"))


##########################################################################

