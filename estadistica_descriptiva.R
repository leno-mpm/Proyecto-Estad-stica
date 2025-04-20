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


#Calcular el promedio
datos <- datos %>%
  mutate(promedio = ifelse(is.na(`NOTA ÁLGEBRA`), 
                           rowMeans(select(., `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE),
                           rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE)
  ))

##########################################################################

             #ANALISIS POR ALGEBRA ~ Variable cuantitativa

##########################################################################

print("Estadísticas descriptivas de Álgebra")

algebra <- datos %>%
  filter(!is.na(`NOTA ÁLGEBRA`)) # Excluye los NA

estadisticas_algebra <- algebra %>%
  summarise(
    media = mean(`NOTA ÁLGEBRA`, na.rm = TRUE),
    mediana = median(`NOTA ÁLGEBRA`, na.rm = TRUE),
    moda = as.numeric(names(sort(table(`NOTA ÁLGEBRA`), decreasing = TRUE)[1])),
    desviacion_estandar = sd(`NOTA ÁLGEBRA`, na.rm = TRUE),
    rango_intercuartilico = IQR(`NOTA ÁLGEBRA`, na.rm = TRUE),
    sesgo = skewness(`NOTA ÁLGEBRA`, na.rm = TRUE),
    curtosis = kurtosis(`NOTA ÁLGEBRA`, na.rm = TRUE)
  )
print(estadisticas_algebra)

print("Histograma de frecuencias - Álgebra")
ggplot(algebra, aes(x = `NOTA ÁLGEBRA`)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Notas de Álgebra", x = "Nota", y = "Frecuencia") +
  theme_minimal()

print("Boxplot - Álgebra")
ggplot(algebra, aes(y = `NOTA ÁLGEBRA`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Notas de Álgebra", y = "Nota") +
  theme_minimal()



##########################################################################

          #ANALISIS POR ESTADÍSTICA ~ Variable cuantitativa

##########################################################################

print("Estadísticas descriptivas de Estadística")
print("Histogramas de frecuencia")
print("Boxplot")

estadistica <- datos %>%
  filter(!is.na(`NOTA ESTADÍSTICA`)) 

estadisticas_estadistica <- estadistica %>%
  summarise(
    media = mean(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    mediana = median(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    moda = as.numeric(names(sort(table(`NOTA ESTADÍSTICA`), decreasing = TRUE)[1])),
    desviacion_estandar = sd(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    rango_intercuartilico = IQR(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    sesgo = skewness(`NOTA ESTADÍSTICA`, na.rm = TRUE),
    curtosis = kurtosis(`NOTA ESTADÍSTICA`, na.rm = TRUE)
  )
print(estadisticas_estadistica)

print("Histograma de frecuencias - Estadística")
ggplot(estadistica, aes(x = `NOTA ESTADÍSTICA`)) +
  geom_histogram(binwidth = 0.45, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Notas de Estadística", x = "Nota", y = "Frecuencia") +
  theme_minimal()

print("Boxplot - Estadística")
ggplot(estadistica, aes(y = `NOTA ESTADÍSTICA`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Notas de Estadística", y = "Nota") +
  theme_minimal()



##########################################################################

            #ANALISIS POR CÁLCULO ~ Variable cuantitativa

##########################################################################

print("Estadísticas descriptivas de Cálculo")

calculo <- datos %>%
  filter(!is.na(`NOTA CÁLCULO`)) # Por si acaso hay NA

estadisticas_calculo <- calculo %>%
  summarise(
    media = mean(`NOTA CÁLCULO`, na.rm = TRUE),
    mediana = median(`NOTA CÁLCULO`, na.rm = TRUE),
    moda = as.numeric(names(sort(table(`NOTA CÁLCULO`), decreasing = TRUE)[1])),
    desviacion_estandar = sd(`NOTA CÁLCULO`, na.rm = TRUE),
    rango_intercuartilico = IQR(`NOTA CÁLCULO`, na.rm = TRUE),
    sesgo = skewness(`NOTA CÁLCULO`, na.rm = TRUE),
    curtosis = kurtosis(`NOTA CÁLCULO`, na.rm = TRUE)
  )
print(estadisticas_calculo)

print("Histograma de frecuencias - Cálculo")
ggplot(calculo, aes(x = `NOTA CÁLCULO`)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Notas de Cálculo", x = "Nota", y = "Frecuencia") +
  theme_minimal()

print("Boxplot - Cálculo")
ggplot(calculo, aes(y = `NOTA CÁLCULO`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot de Notas de Cálculo", y = "Nota") +
  theme_minimal()



##########################################################################

     #ANALISIS POR FUNDAMENTOS DE PROGRAMACIÓN ~ Variable cuantitativa

##########################################################################

print("Estadísticas descriptivas de Fundamentos de Programación")

programacion <- datos$`NOTA FUND. PROG.`
programacion <- na.omit(programacion)
summary(programacion)
describe(programacion)
media <- mean(programacion)
mediana <- median(programacion)
moda <- as.numeric(names(sort(table(programacion), decreasing = TRUE)[1]))
varianza <- var(programacion)
desviacion <- sd(programacion)
coef_var <- sd(programacion) / mean(programacion)
asimetria <- skewness(programacion)
curtosis <- kurtosis(programacion)

print("Estadísticos de Programación")
cat("Media:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Moda:", moda, "\n")
cat("Varianza:", varianza, "\n")
cat("Desviación estándar:", desviacion, "\n")
cat("Coeficiente de variación:", coef_var, "\n")
cat("Asimetría:", asimetria, "\n")
cat("Curtosis:", curtosis, "\n")

print("Histogramas de frecuencia")
ggplot(datos, aes(x = `NOTA FUND. PROG.`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Histograma de Frecuencias - Fundamentos de Programación",
    x = "Nota",
    y = "Frecuencia"
  ) +
  theme_minimal()

print("Boxplot")
# Boxplot
ggplot(datos, aes(y = `NOTA FUND. PROG.`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Boxplot - Fundamentos de Programación",
    y = "Nota"
  ) +
  theme_minimal()



##########################################################################

          #ANALISIS POR PROMEDIO ~ Variable cuantitativa

##########################################################################

print("Estadísticas descriptivas del Promedio de Materias Previas")

promedio_df <- datos %>%
  filter(!is.na(promedio)) 

estadisticas_promedio <- promedio_df %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )
print(estadisticas_promedio)

print("Histograma de Frecuencias - Promedio")
ggplot(promedio_df, aes(x = promedio)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Promedio",
       x = "Promedio", y = "Frecuencia") +
  theme_minimal()

print("Boxplot - Promedio")
ggplot(promedio_df, aes(y = promedio)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot del Promedio", y = "Promedio") +
  theme_minimal()



##########################################################################

                #ANALISIS POR GÉNERO ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por género")
print(table(datos$SEXO))
print("Proporciones de estudiantes por género")
print(prop.table(table(datos$SEXO)))

# Gráfico de barras por género
ggplot(datos, aes(x = `SEXO`)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Estudiantes por Género",
       x = "Género", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##########################################################################

              #ANALISIS POR CARRERA ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por carrera")
print(table(datos$CARRERA))
print("Proporciones de estudiantes por carrera")
print(prop.table(table(datos$CARRERA)))

print("Gráfico de barras")
ggplot(datos, aes(x = CARRERA)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribución de Estudiantes por Carrera",
    x = "Carrera",
    y = "Cantidad de Estudiantes"
  ) +
  theme_minimal()



##########################################################################

          #ANALISIS POR HORARIO ACADÉMICO ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por horario")
print(table(datos$`HORARIO TOMADO`))
print("Proporciones de estudiantes por horario en Estadística")
print(prop.table(table(datos$`HORARIO TOMADO`)))

print("Gráfico de barras")
ggplot(datos, aes(x = `HORARIO TOMADO`)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Estudiantes por Horario Académico",
       x = "Horario", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


##########################################################################

    #ANALISIS POR RENDIMIENTO ACADÉMICO ~ Variable cualitativa

##########################################################################

#El rendimiento se clasificará en dos:
# Rendimiento alto/bajo basado en el promedio de materias previas (Algebra, Cálculo, Fundamentos)
#Alto rendiemiento para las notas >= 7.5 y Bajo rendimiento para las notas < 7.5

# Clasificamos el rendimiento
datos <- datos %>%
  mutate(RENDIMIENTO = ifelse(promedio >= 7.5, "ALTO", "BAJO"))

print("Distribución de estudiantes por rendimiento académico")
print(table(datos$RENDIMIENTO))
print("Proporciones de estudiantes por rendimiento académico")
print(prop.table(table(datos$RENDIMIENTO)))

# Gráfico de barras de rendimiento académico
ggplot(datos, aes(x = RENDIMIENTO)) +
  geom_bar(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Estudiantes por Rendimiento Académico",
       x = "Rendimiento", y = "Frecuencia") +
  theme_minimal()

