library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)
library(corrplot)
library(ggcorrplot)
library(tidyr)
library(tidyr) 

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

# Filtrar solo estudiantes de la carrera de Mecatrónica ó Computación
datos <- datos %>%
  filter(CARRERA == "Mecatrónica") # "Computación" <----- (Puede cambiarse para observar los detalles)

# Calcular el promedio
datos <- datos %>%
  mutate(promedio = rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE))


##########################################################################

              #ANALISIS BIVARIANTE (Promedio vs Género)

##########################################################################

print("Comparacion de potencial por género") 
print("¿Hay diferencias en el promedio entre hombres y mujeres?")
print("¿Algún género tiende a tener mejor rendimiento?")

# Estudiantes Hombres
estudiantes_hombre <- datos %>%
  filter(SEXO == "H")

estadisticas_hombres <- estudiantes_hombre %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )

# Estudiantes Mujeres
estudiantes_mujer <- datos %>%
  filter(SEXO == "M")

estadisticas_mujeres <- estudiantes_mujer %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )

# Histograma comparativo por género
# Histograma comparativo por género (ajustando rangos válidos)
datos <- datos %>%
  mutate(rango_promedio = cut(promedio, 
                              breaks = c(6, 7, 8, 9, 10), 
                              labels = c("6-7", "7-8", "8-9", "9-10"),
                              right = FALSE))

ggplot(datos, aes(x = rango_promedio, fill = SEXO)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("H" = "blue", "M" = "pink"), 
                    labels = c("H" = "Hombres", "M" = "Mujeres")) +
  labs(title = "Distribución de Potencial por Género",
       x = "Rango de Promedio | Potencial",
       y = "Frecuencia",
       fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Boxplot comparativo
print("Boxplot - Potencial vs Est. hombres y mujeres")
promedios_hombres <- estudiantes_hombre$promedio
promedios_mujeres <- estudiantes_mujer$promedio

boxplot(promedios_hombres, promedios_mujeres, 
        names = c("Estudiantes Hombres", "Estudiantes Mujeres"),
        main = "Promedio de Estudiantes", 
        ylab = "Promedio", 
        col = c("blue", "pink"))



##########################################################################

         #ANALISIS BIVARIANTE (Nota Estadística vs Género)

##########################################################################

print("Comparación de nota en Estadística por género") 
print("¿Hay diferencias en la nota de Estadística entre hombres y mujeres?")
print("¿Algún género tiende a tener mejor desempeño en Estadística?")

# Estudiantes Hombres
estudiantes_hombre <- datos %>%
  filter(SEXO == "H")

estadisticas_esta_hombres <- estudiantes_hombre %>%
  summarise(
    media = mean(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    mediana = median(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    moda = as.numeric(names(sort(table(`NOTA ESTADÍSTICA`), decreasing = TRUE)[1])),
    desviacion_estandar = sd(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    rango_intercuartilico = IQR(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    sesgo = skewness(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    curtosis = kurtosis(`NOTA ESTADÍSTICA`, na.rm = TRUE)
  )

# Estudiantes Mujeres
estudiantes_mujer <- datos %>%
  filter(SEXO == "M")

estadisticas_esta_mujeres <- estudiantes_mujer %>%
  summarise(
    media = mean(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    mediana = median(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    moda = as.numeric(names(sort(table(`NOTA ESTADÍSTICA`), decreasing = TRUE)[1])),
    desviacion_estandar = sd(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    rango_intercuartilico = IQR(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    sesgo = skewness(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    curtosis = kurtosis(`NOTA ESTADÍSTICA`, na.rm = TRUE)
  )

# Histograma comparativo por género
datos <- datos %>%
  mutate(rango_estadistica = cut(`NOTA ESTADÍSTICA`, 
                                 breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                 labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                                 right = FALSE))

ggplot(datos, aes(x = rango_estadistica, fill = SEXO)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("H" = "blue", "M" = "pink"), 
                    labels = c("H" = "Hombres", "M" = "Mujeres")) +
  labs(title = "Distribución de Nota en Estadística por Género",
       x = "Rango de Nota | Estadística",
       y = "Frecuencia",
       fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Boxplot comparativo
print("Boxplot - Estadística vs Est. hombres y mujeres")
nota_esta_hombres <- estudiantes_hombre$`NOTA ESTADÍSTICA`
nota_esta_mujeres <- estudiantes_mujer$`NOTA ESTADÍSTICA`

boxplot(nota_esta_hombres, nota_esta_mujeres, 
        names = c("Estudiantes Hombres", "Estudiantes Mujeres"),
        main = "Nota en Estadística por Género", 
        ylab = "Nota", 
        col = c("blue", "pink"))



##########################################################################

                #ANALISIS BIVARIANTE (Sexo vs Horario)

##########################################################################

print("Comparación del Sexo vs Horario")
print("¿Hay relación entre el sexo del estudiante y el horario en que estudia?")
print("¿Ciertos turnos están dominados por hombres o mujeres?")

tabla_contingencia <- table(datos$SEXO, datos$`HORARIO TOMADO`)
print("Tabla de Contingencia entre Género y Horario:")
print(tabla_contingencia)

chi2_test <- chisq.test(tabla_contingencia)
print("Resultados de la prueba Chi-cuadrado de independencia:")
print(chi2_test)

if(chi2_test$p.value < 0.05) {
  print("Existe una relación significativa entre el sexo del estudiante y el horario en que estudia.")
} else {
  print("No existe una relación significativa entre el sexo del estudiante y el horario en que estudia.")
}

# Gráfico de barras apiladas para visualizar la relación
ggplot(datos, aes(x = `HORARIO TOMADO`, fill = `SEXO`)) +
  geom_bar(position = "fill", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Género por Horario Académico",
       x = "Horario", y = "Proporción", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

