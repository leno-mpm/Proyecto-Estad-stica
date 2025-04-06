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
print("Histogramas de frecuencia")
print("Boxplot")
#FRANCISCO Y MILENA








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
print("Gráfico de barras")
print("Boxplot")
#FRANCISCO Y MILENA






##########################################################################

          #ANALISIS POR HORARIO ACADÉMICO ~ Variable cualitativa

##########################################################################

print("Distribución de estudiantes por horario")
print("Gráfico de barras")
print("Boxplot")
#FRANCISCO Y MILENA

