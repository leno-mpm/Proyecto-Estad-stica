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


##########################################################################

            #ANÁLISIS INFERENCIAL ~ CARRERA DE MECATRÓNICA
  
##########################################################################

datos_meca <- datos %>%
  filter(CARRERA == "Mecatrónica") %>%
  mutate(promedio = rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE))


#PRUEBA T DE DIFERENCIA DE MEDIAS DE ALTO Y BAJO RENDIMIENTO
  # Grupo de alto rendimiento (promedio previo ≥ 7.5)
  # Grupo de bajo rendimiento (promedio previo < 7.5)

#H₀: μA - μB ≤ 1.30    → La diferencia entre grupos NO es mayor a 0.80 (no vale la pena separar)
#Hₐ: μA - μB > 1.30    → La diferencia entre grupos SÍ es mayor a 0.80 (sí vale la pena separar)

datos_meca$rendimiento <- ifelse(datos_meca$promedio >= 7.5, "Alto", "Bajo")

prueba_t <- t.test(`NOTA ESTADÍSTICA` ~ rendimiento, data = datos_meca, mu = 0.8, alternative = "greater")
print(prueba_t)

if (prueba_t$p.value < 0.05) {
  print("Existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
} else {
  print("No hay suficiente evidencia para afirmar que existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
}

print("La prueba t realizada no mostró una diferencia estadísticamente significativa entre las medias de la nota 
de Estadística de los estudiantes clasificados como de alto y bajo rendimiento previo (p > 0.05). Esto indica 
que, bajo los datos analizados, no hay suficiente evidencia para afirmar que el rendimiento académico previo 
influye de manera clara y diferenciadora en el desempeño en la asignatura de Estadística. Por lo tanto, no se 
justifica separar a los estudiantes en dos grupos para el análisis en función de su rendimiento previo.")




#PRUEBA T DE DIFERENCIAS DE MEDIAS EN BASE AL HORARIO TOMADO
  # A: Estudiantes que tomaron el horario de 07:00 - 09:00
  # B: Estudiantes que tomaron el horario de 09:00 - 11:00

# H₀: μA = μB
# Hₐ: μA ≠ μB

prueba_t_horario <- t.test(`NOTA ESTADÍSTICA` ~ `HORARIO TOMADO`, data = datos_meca)
print(prueba_t_horario)

if (prueba_t_horario$p.value < 0.05) {
  print("Existe una diferencia significativa en las notas de Estadística según el horario tomado.")
} else {
  print("No hay suficiente evidencia para afirmar que existe una diferencia significativa en las notas de Estadística según el horario tomado.")
}





##########################################################################

            #ANÁLISIS INFERENCIAL ~ CARRERA DE COMPUTACIÓN

##########################################################################

datos_comp <- datos %>%
  filter(CARRERA == "Computación") %>%
  mutate(promedio = rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE))


#PRUEBA T DE DIFERENCIA DE MEDIAS DE ALTO Y BAJO RENDIMIENTO
  # Grupo de alto rendimiento (promedio previo ≥ 7.5)
  # Grupo de bajo rendimiento (promedio previo < 7.5)

#H₀: μA - μB ≤ 1.30    → La diferencia entre grupos NO es mayor a 0.80 (no vale la pena separar)
#Hₐ: μA - μB > 1.30    → La diferencia entre grupos SÍ es mayor a 0.80 (sí vale la pena separar)

datos_comp$rendimiento <- ifelse(datos_comp$promedio >= 7.5, "Alto", "Bajo")

prueba_t2 <- t.test(`NOTA ESTADÍSTICA` ~ rendimiento, data = datos_comp, mu = 0.8, alternative = "greater")
print(prueba_t2)

if (prueba_t2$p.value < 0.05) {
  print("Existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
} else {
  print("No hay suficiente evidencia para afirmar que existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
}

print("La prueba t realizada no mostró una diferencia estadísticamente significativa entre las medias de la nota 
de Estadística de los estudiantes clasificados como de alto y bajo rendimiento previo (p > 0.05). Esto indica 
que, bajo los datos analizados, no hay suficiente evidencia para afirmar que el rendimiento académico previo 
influye de manera clara y diferenciadora en el desempeño en la asignatura de Estadística. Por lo tanto, no se 
justifica separar a los estudiantes en dos grupos para el análisis en función de su rendimiento previo.")




#PRUEBA T DE DIFERENCIAS DE MEDIAS EN BASE AL HORARIO TOMADO
  # A: Estudiantes que tomaron el horario de 07:00 - 09:00
  # B: Estudiantes que tomaron el horario de 09:00 - 11:00

# H₀: μA = μB
# Hₐ: μA ≠ μB

prueba_t_horario2 <- t.test(`NOTA ESTADÍSTICA` ~ `HORARIO TOMADO`, data = datos_comp)
print(prueba_t_horario2)

if (prueba_t_horario2$p.value < 0.05) {
  print("Existe una diferencia significativa en las notas de Estadística según el horario tomado.")
} else {
  print("No hay suficiente evidencia para afirmar que existe una diferencia significativa en las notas de Estadística según el horario tomado.")
}
