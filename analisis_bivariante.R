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

    # ANALISIS BIVARIANTE (Potencial vs Cantidad de materias vistas)

##########################################################################

print("Comparación de potencial por cantidad de materias vistas") 
print("¿Influye la cantidad de materias clave (Álgebra, Cálculo, Estadística, Programación) cursadas en el potencial del estudiante?")
print("¿Tienen mayor potencial quienes han cursado más de estas materias?")


datos <- datos %>%
  mutate(cantidad_de_materias = ifelse(is.na(`NOTA ÁLGEBRA`), 3, 4))

# ------- Estadísticas para estudiantes con 3 materias -------
estudiantes_3_materias <- datos %>% filter(cantidad_de_materias == 3)
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
print("Estadísticas para estudiantes que ven 3 materias (promedio|potencial):")
print(estadisticas_3_promedio)


# ------- Estadísticas para estudiantes con 4 materias -------
estudiantes_4_materias <- datos %>% filter(cantidad_de_materias == 4)
estadisticas_4_promedio <- estudiantes_4_materias %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )
print("Estadísticas para estudiantes que ven 4 materias (promedio|potencial):")
print(estadisticas_4_promedio)


# ------- Análisis conjunto ---------------
datos <- datos %>%
  mutate(rango_promedio = cut(promedio, breaks = c(4, 5, 6, 7, 8, 9, 10), 
                              labels = c("4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                              right = FALSE))
datos$cantidad_de_materias <- factor(datos$cantidad_de_materias,
                                     levels = c(3, 4),
                                     labels = c("3 materias", "4 materias"))


print("Gráfico de Frecuencias Combinado")
ggplot(datos, aes(x = rango_promedio, fill = cantidad_de_materias)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribución de Potencial por Cantidad de Materias Vistas",
       x = "Rango de Promedio | Potencial", y = "Frecuencia",
       fill = "Materias Vistas") +
  scale_fill_manual(values = c("3 materias" = "skyblue", "4 materias" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))



print("Boxplot - Comparación de Potencial por Cantidad de Materias")
ggplot(datos, aes(x = cantidad_de_materias, y = promedio, fill = cantidad_de_materias)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  labs(title = "Boxplot de Potencial por Cantidad de Materias Vistas",
       x = "Cantidad de Materias", y = "Potencial") +
  scale_fill_manual(values = c("3 materias" = "skyblue", "4 materias" = "seagreen")) +
  theme_minimal()

#FALTA HACER MATRIZ DE CORRELACION



##########################################################################

          #ANALISIS BIVARIANTE (Potencial vs Materias)

##########################################################################

print("Comparación de potencial vs Materias (Álgebra, Cálculo, Estadística, Programación)")
print("¿Hay relación entre la nota en una materia específica (como Cálculo o Estadística) y el Potencial?")
print("¿Cuál materia tiene mayor influencia en el rendimiento total?")
#Estadísticos
#Boxplot
#Matriz de Correlacion

#GABRIEL Y JAIRO





##########################################################################

             #ANALISIS BIVARIANTE (Potencial vs Género)

##########################################################################

print("Comparacion de potencial por género") 
print("¿Hay diferencias en el Potencial promedio entre hombres y mujeres?")
print("¿Algún género tiende a tener mejor rendimiento?")
#Estadísticos
#Boxplot
#Matriz de Correlacion

#FRANCISCO Y MILENA









##########################################################################

            #ANALISIS BIVARIANTE (Potencial vs Horario)

##########################################################################

print("Comparación del Potencial por horario")
print("¿Influye el horario de estudio en el rendimiento?")
datos$`HORARIO TOMADO` <- as.factor(trimws(datos$`HORARIO TOMADO`))


# ------- HORARIO 7:00 - 9:00 ---------------
estudiantes_7a9 <- datos %>% filter(`HORARIO TOMADO` == "07h00 - 09h00")
estadisticas_7a9 <- estudiantes_7a9 %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )
print("Estadísticas para estudiantes en el horario 07h00 - 09h00:")
print(estadisticas_7a9)


# ------- HORARIO 9:00 - 11:00 ---------------

estudiantes_9a11 <- datos %>% filter(`HORARIO TOMADO` == "09h00 - 11h00")
estadisticas_9a11 <- estudiantes_9a11 %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE)
  )
print("Estadísticas para estudiantes en el horario 09h00 - 11h00:")
print(estadisticas_9a11)


# ------- Análisis conjunto ---------------
datos <- datos %>%
  mutate(rango_promedio = cut(promedio, breaks = c(4, 5, 6, 7, 8, 9, 10), 
                              labels = c("4-5", "5-6", "6-7", "7-8", "8-9", "9-10"),
                              right = FALSE))

ggplot(datos, aes(x = rango_promedio, fill = `HORARIO TOMADO`)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Distribución de Potencial por Horario",
       x = "Rango de Promedio | Potencial", y = "Frecuencia") +
  scale_fill_manual(values = c("07h00 - 09h00" = "skyblue", "09h00 - 11h00" = "orange")) +
  theme_minimal()

ggplot(datos, aes(x = `HORARIO TOMADO`, y = promedio, fill = `HORARIO TOMADO`)) +
  geom_boxplot() +
  labs(title = "Boxplot de Potencial según Horario",
       x = "Horario Tomado", y = "Promedio | Potencial") +
  scale_fill_manual(values = c("07h00 - 09h00" = "skyblue", "09h00 - 11h00" = "orange")) +
  theme_minimal()






##########################################################################
  
                 #ANALISIS BIVARIANTE (Potencial vs Carrera)

##########################################################################

print("Comparación del Potencial por carrera")
print("¿Hay carreras cuyos estudiantes presentan mayor Potencial?")
#Estadísticos
#Boxplot
#Matriz de Correlacion
#GABRIEL Y JAIRO










##########################################################################

                #ANALISIS BIVARIANTE (Sexo vs Horario)

##########################################################################

print("Comparación del Sexo vs Horario")
print("¿Hay relación entre el sexo del estudiante y el horario en que estudia?")
print("¿Ciertos turnos están dominados por hombres o mujeres?")
#Estadísticos
#Tabla de contigencia
#PRuebas de independencia Chi-cuadrado

#FRANCISCO Y MILENA

