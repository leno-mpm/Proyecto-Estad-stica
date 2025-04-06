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


##########################################################################

             #ANALISIS POR ALGEBRA ~ Variable cuantitativa

##########################################################################
print("Estadísticas descriptivas de Álgebra")
print("Histogramas de frecuencia")
print("Boxplot")
#GABRIEL Y JAIRO








##########################################################################

          #ANALISIS POR ESTADÍSTICA ~ Variable cuantitativa

##########################################################################
print("Estadísticas descriptivas de Estadística")
print("Histogramas de frecuencia")
print("Boxplot")
#GABRIEL Y JAIRO










##########################################################################

            #ANALISIS POR CÁLCULO~ Variable cuantitativa

##########################################################################
print("Estadísticas descriptivas de Cálculo")
print("Histogramas de frecuencia")
print("Boxplot")
#GABRIEL Y JAIRO









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
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Histograma de Frecuencias - Fundamentos de Programación",
    x = "Nota",
    y = "Frecuencia"
  ) +
  theme_minimal()

print("Boxplot")
# Boxplot
ggplot(datos, aes(y = `NOTA FUND. PROG.`)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(
    title = "Boxplot - Fundamentos de Programación",
    y = "Nota"
  ) +
  theme_minimal()





##########################################################################

                #ANALISIS POR GÉNERO ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por género")
print("Gráfico de barras")
print("Boxplot")
#FRANCISCO Y MILENA




##########################################################################

              #ANALISIS POR CARRERA ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por carrera")
print(table(datos$CARRERA))


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
print("Gráfico de barras")
#FRANCISCO Y MILENA

