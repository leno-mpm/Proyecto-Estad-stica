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

datos <- datos %>%
  mutate(promedio = ifelse(is.na(`NOTA ÁLGEBRA`), 
                           rowMeans(select(., `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE),
                           rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE)
  ))




              #Análisis Bivariante ~ Promedio vs Carrera


datos <- datos %>%
  mutate(cantidad_de_materias = ifelse(is.na(`NOTA ÁLGEBRA`), 3, 4))


# Carreras con 2 materias (CÁLCULO Y FUNDAMENTOS DE PROGRAMACIÓN)
estudiantes_3_materias <- datos %>% filter(cantidad_de_materias == 3)
estadisticas_3_por_carrera <- estudiantes_3_materias %>%
  group_by(CARRERA) %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE),
    n_estudiantes = n()
  )

print("Estadísticas para carreras de 2 materias:")
print(estadisticas_3_por_carrera)


# Carreras con 3 materias (ÁLGEBRA, CÁLCULO Y FUNDAMENTOS DE PROGRAMACIÓN)
estudiantes_4_materias <- datos %>% filter(cantidad_de_materias == 4)
estadisticas_4_por_carrera <- estudiantes_4_materias %>%
  group_by(CARRERA) %>%
  summarise(
    media = mean(promedio, na.rm = TRUE),
    mediana = median(promedio, na.rm = TRUE),
    moda = as.numeric(names(sort(table(promedio), decreasing = TRUE)[1])),
    desviacion_estandar = sd(promedio, na.rm = TRUE),
    rango_intercuartilico = IQR(promedio, na.rm = TRUE),
    sesgo = skewness(promedio, na.rm = TRUE),
    curtosis = kurtosis(promedio, na.rm = TRUE),
    n_estudiantes = n()
  )

print("Estadísticas para promedio con 3 materias:")
print(estadisticas_4_por_carrera)


#Boxplot
# Gráfico para Carreras con 2 materias
ggplot(estudiantes_3_materias, aes(x = CARRERA, y = promedio)) +
  geom_boxplot(fill = "tomato", alpha = 0.7) +
  labs(title = "Promedio por Carrera - 2 materias",
       x = "Carrera", y = "Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico para estudiantes con 3 materias
ggplot(estudiantes_4_materias, aes(x = CARRERA, y = promedio)) +
  geom_boxplot(fill = "turquoise4", alpha = 0.7) +
  labs(title = "Potencial por Carrera - Estudiantes con 3 materias",
       x = "Carrera", y = "Promedio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




##########################################################################

            #Análisis Bivariante ~ CARRERA DE MECATRÓNICA

##########################################################################



# Filtrar solo estudiantes de la carrera de Mecatrónica
datos_meca <- datos %>%
  filter(CARRERA == "Mecatrónica")


              #Análisis Bivariante ~ Promedio vs Nota de Estadística

print("Comparación del promedio vs Nota Estadística")
materias_df <- datos_meca%>% 
  select(`NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`, `NOTA ESTADÍSTICA`, promedio)
correlaciones <- cor(materias_df, use = "complete.obs")
print("Matriz de correlación entre promedio y materias:")
print(correlaciones["promedio", ])
corrplot(cor(materias_df, use = "complete.obs"), method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

print("Comparación del Rendimiento vs Nota de Estadística")
#El rendimiento se clasificará en dos:
#Rendimiento alto/bajo basado en el promedio de materias previas (Algebra, Cálculo, Fundamentos)
#Alto rendimiento para las notas >= 7.5 y Bajo rendimiento para las notas < 7.5
datos_meca <- datos_meca %>%
  mutate(rendimiento = ifelse(promedio >= 7.5, "Alto", "Bajo"))
ggplot(datos_meca, aes(x = rendimiento, y = `NOTA ESTADÍSTICA`, fill = rendimiento)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Boxplot de Nota de Estadística según Rendimiento General",
       x = "Rendimiento (según promedio materias previas)",
       y = "Nota de Estadística") +
  scale_fill_manual(values = c("Alto" = "seagreen", "Bajo" = "tomato")) +
  theme_minimal()

print("Gráfico de dispersión entre promedio de materias vs Nota de estadísta pero agrupadolo por rendimiento)")
datos_meca <- datos_meca%>%
  mutate(grupo_color = ifelse(promedio >= 7.5, "Alto", "Bajo"))

ggplot(datos_meca, aes(x = promedio, y = `NOTA ESTADÍSTICA`, color = grupo_color)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Dispersión: Promedio Materias vs Nota de Estadística",
       x = "Promedio de materias previas",
       y = "Nota de Estadística",
       color = "Rendimiento") +
  scale_color_manual(values = c("Alto" = "seagreen", "Bajo" = "firebrick")) +
  theme_minimal()


        #Análisis Bivariante  ~ Nota de Estadística vs Horario

# Boxplot de Nota de Estadística por Horario Tomado
ggplot(datos_meca, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`, fill = `HORARIO TOMADO`)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Boxplot de Nota de Estadística según Horario Tomado",
       x = "Horario Tomado",
       y = "Nota de Estadística") +
  theme_minimal() +
  theme(legend.position = "none")

print("El análisis comparativo evidencia que los estudiantes del horario de 09h00-11h00 presentan un rendimiento 
      académico superior en Estadística, con una distribución de notas más elevada y consistente en comparación con 
      el grupo de 07h00-09h00, el cual muestra mayor dispersión y valores atípicos extremos, lo que sugiere que 
      factores asociados al horario —como la predisposición cognitiva matutina o condiciones logísticas— podrían 
      influir significativamente en los resultados, aunque se requieren estudios adicionales para controlar variables 
      de confusión y establecer causalidad.")


            #Análisis Bivariante ~ Rendimiento vs Horario

# Convertir a factor para orden visual
datos_meca$rendimiento <- factor(datos_meca$rendimiento, levels = c("Bajo", "Alto"))

# Gráfico de barras: Rendimiento previo vs Horario de Estadística
ggplot(datos_meca, aes(x = `HORARIO TOMADO`, fill = rendimiento)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Comparación de Rendimiento Previo según Horario de Estadística",
       x = "Horario Tomado de Estadística",
       y = "Cantidad de Estudiantes",
       fill = "Rendimiento Previo") +
  scale_fill_manual(values = c("Bajo" = "salmon", "Alto" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tabla_chi <- table(datos_meca$`HORARIO TOMADO`, datos_meca$rendimiento)
chi_resultado <- chisq.test(tabla_chi)
print("Prueba Chi-cuadrado: Asociación entre Horario y Rendimiento Previo")
print(chi_resultado)
print("Esto mostró que son independientes")

print("El análisis estadístico (prueba de Chi-cuadrado, p = 1.000) no muestra evidencia de una asociación 
      significativa entre el horario de Estadística (07h00-09h00 vs. 09h00-11h00) y el rendimiento académico 
      previo (Alto/Bajo) de los estudiantes. El valor p=1.000 indica que la distribución de estudiantes con 
      alto y bajo rendimiento es exactamente igual en ambos horarios, lo que descarta cualquier relación entre 
      la elección de horario y su preparación académica anterior.")


#Análisis Bivariante ~ Nota de Estadística en Estudiantes de Alto Rendimiento según el Horario Académico

alto_rendimiento <- datos_meca %>%
  filter(promedio >= 7.5, !is.na(`NOTA ESTADÍSTICA`), !is.na(`HORARIO TOMADO`))

estadisticas_por_horario <- alto_rendimiento %>%
  group_by(`HORARIO TOMADO`) %>%
  summarise(
    cantidad = n(),
    media = mean(`NOTA ESTADÍSTICA`),
    mediana = median(`NOTA ESTADÍSTICA`),
    desviacion = sd(`NOTA ESTADÍSTICA`),
    minimo = min(`NOTA ESTADÍSTICA`),
    maximo = max(`NOTA ESTADÍSTICA`)
  )
print("Estadísticas descriptivas por horario (alto rendimiento):")
print(estadisticas_por_horario)

#Boxplot comparativo
ggplot(alto_rendimiento, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Notas de Estadística según Horario (Estudiantes de Alto Rendimiento)",
    x = "Horario Tomado",
    y = "Nota Estadística"
  ) +
  theme_minimal()


#Análisis Bivariante ~ Nota de Estadística en Estudiantes de Bajo Rendimiento según el Horario Académico

bajo_rendimiento <- datos_meca %>%
  filter(promedio < 7.5 & !is.na(`NOTA ESTADÍSTICA`) & !is.na(`HORARIO TOMADO`))

ggplot(bajo_rendimiento, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(
    title = "Notas de Estadística según Horario Académico (Bajo Rendimiento)",
    x = "Horario Académico",
    y = "Nota en Estadística"
  ) +
  theme_minimal()


##########################################################################

              #ANÁLISIS BIVARIANTE ~ CARRERA DE COMPUTACIÓN

##########################################################################

# Filtrar solo estudiantes de la carrera de Mecatrónica
datos_comp <- datos %>%
  filter(CARRERA == "Computación")


              #Análisis Bivariante ~ Promedio vs Nota de Estadística

print("Comparación del promedio vs Nota Estadística")
materias_df2 <- datos_comp%>% 
  select(`NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`, `NOTA ESTADÍSTICA`, promedio)
correlaciones2 <- cor(materias_df2, use = "complete.obs")
print("Matriz de correlación entre promedio y materias:")
print(correlaciones2["promedio", ])
corrplot(cor(materias_df2, use = "complete.obs"), method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45)

print("Comparación del Rendimiento vs Nota de Estadística")
#El rendimiento se clasificará en dos:
#Rendimiento alto/bajo basado en el promedio de materias previas (Algebra, Cálculo, Fundamentos)
#Alto rendimiento para las notas >= 7.5 y Bajo rendimiento para las notas < 7.5
datos_comp  <- datos_comp %>%
  mutate(rendimiento = ifelse(promedio >= 7.5, "Alto", "Bajo"))
ggplot(datos_comp, aes(x = rendimiento, y = `NOTA ESTADÍSTICA`, fill = rendimiento)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Boxplot de Nota de Estadística según Rendimiento General",
       x = "Rendimiento (según promedio materias previas)",
       y = "Nota de Estadística") +
  scale_fill_manual(values = c("Alto" = "seagreen", "Bajo" = "tomato")) +
  theme_minimal()

print("Gráfico de dispersión entre promedio de materias vs Nota de estadísta pero agrupadolo por rendimiento)")
datos_comp <- datos_comp%>%
  mutate(grupo_color = ifelse(promedio >= 7.5, "Alto", "Bajo"))

ggplot(datos_comp, aes(x = promedio, y = `NOTA ESTADÍSTICA`, color = grupo_color)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Dispersión: Promedio Materias vs Nota de Estadística",
       x = "Promedio de materias previas",
       y = "Nota de Estadística",
       color = "Rendimiento") +
  scale_color_manual(values = c("Alto" = "seagreen", "Bajo" = "firebrick")) +
  theme_minimal()


                #Análisis Bivariante  ~ Nota de Estadística vs Horario

# Boxplot de Nota de Estadística por Horario Tomado
ggplot(datos_comp , aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`, fill = `HORARIO TOMADO`)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Boxplot de Nota de Estadística según Horario Tomado",
       x = "Horario Tomado",
       y = "Nota de Estadística") +
  theme_minimal() +
  theme(legend.position = "none")

print("El análisis comparativo evidencia que los estudiantes del horario de 09h00-11h00 presentan un rendimiento 
      académico superior en Estadística, con una distribución de notas más elevada y consistente en comparación con 
      el grupo de 07h00-09h00, el cual muestra mayor dispersión y valores atípicos extremos, lo que sugiere que 
      factores asociados al horario —como la predisposición cognitiva matutina o condiciones logísticas— podrían 
      influir significativamente en los resultados, aunque se requieren estudios adicionales para controlar variables 
      de confusión y establecer causalidad.")


                  #Análisis Bivariante ~ Rendimiento vs Horario

# Convertir a factor para orden visual
datos_comp $rendimiento <- factor(datos_comp$rendimiento, levels = c("Bajo", "Alto"))

# Gráfico de barras: Rendimiento previo vs Horario de Estadística
ggplot(datos_comp , aes(x = `HORARIO TOMADO`, fill = rendimiento)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Comparación de Rendimiento Previo según Horario de Estadística",
       x = "Horario Tomado de Estadística",
       y = "Cantidad de Estudiantes",
       fill = "Rendimiento Previo") +
  scale_fill_manual(values = c("Bajo" = "salmon", "Alto" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tabla_chi2 <- table(datos_comp $`HORARIO TOMADO`, datos_comp $rendimiento)
chi_resultado2 <- chisq.test(tabla_chi2)
print("Prueba Chi-cuadrado: Asociación entre Horario y Rendimiento Previo")
print(chi_resultado2)
print("Esto mostró que son independientes")


print("La prueba Chi-cuadrado no mostró evidencia significativa de una asociación entre el horario 
      tomado y el rendimiento previo (p = 0.8764 > 0.05). 
      Esto indica que el rendimiento académico previo de los estudiantes no depende del 
      horario en el que cursaron la materia, al menos según los datos analizados.")


#Análisis Bivariante ~ Nota de Estadística en Estudiantes de Alto Rendimiento según el Horario Académico

alto_rendimiento2 <- datos_comp %>%
  filter(promedio >= 7.5, !is.na(`NOTA ESTADÍSTICA`), !is.na(`HORARIO TOMADO`))

estadisticas_por_horario2 <- alto_rendimiento2 %>%
  group_by(`HORARIO TOMADO`) %>%
  summarise(
    cantidad = n(),
    media = mean(`NOTA ESTADÍSTICA`),
    mediana = median(`NOTA ESTADÍSTICA`),
    desviacion = sd(`NOTA ESTADÍSTICA`),
    minimo = min(`NOTA ESTADÍSTICA`),
    maximo = max(`NOTA ESTADÍSTICA`)
  )
print("Estadísticas descriptivas por horario (alto rendimiento):")
print(estadisticas_por_horario2)

#Boxplot comparativo
ggplot(alto_rendimiento2, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(
    title = "Notas de Estadística según Horario (Estudiantes de Alto Rendimiento)",
    x = "Horario Tomado",
    y = "Nota Estadística"
  ) +
  theme_minimal()


#Análisis Bivariante ~ Nota de Estadística en Estudiantes de Bajo Rendimiento según el Horario Académico

bajo_rendimiento2 <- datos_comp %>%
  filter(promedio < 7.5 & !is.na(`NOTA ESTADÍSTICA`) & !is.na(`HORARIO TOMADO`))

ggplot(bajo_rendimiento2, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(
    title = "Notas de Estadística según Horario Académico (Bajo Rendimiento)",
    x = "Horario Académico",
    y = "Nota en Estadística"
  ) +
  theme_minimal()



