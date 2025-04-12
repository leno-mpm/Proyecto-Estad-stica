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


#Calcular el promedio
datos <- datos %>%
  mutate(promedio = ifelse(is.na(`NOTA ÁLGEBRA`), 
                           rowMeans(select(., `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE),
                           rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE)
  ))




##########################################################################

                         #ANALISIS BIVARIANTE

##########################################################################

print("Comparación del promedio vs Nota Estadística")
materias_df <- datos %>% 
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
datos <- datos %>%
  mutate(rendimiento = ifelse(promedio >= 7.5, "Alto", "Bajo"))
ggplot(datos, aes(x = rendimiento, y = `NOTA ESTADÍSTICA`, fill = rendimiento)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  labs(title = "Boxplot de Nota de Estadística según Rendimiento General",
       x = "Rendimiento (según promedio materias previas)",
       y = "Nota de Estadística") +
  scale_fill_manual(values = c("Alto" = "seagreen", "Bajo" = "tomato")) +
  theme_minimal()




print("Gráfico de dispersión entre promedio de materias vs Nota de estadísta pero agrupadolo por rendimiento)")
datos <- datos %>%
  mutate(grupo_color = ifelse(promedio >= 7.5, "Alto", "Bajo"))

ggplot(datos, aes(x = promedio, y = `NOTA ESTADÍSTICA`, color = grupo_color)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "Dispersión: Promedio Materias vs Nota de Estadística",
       x = "Promedio de materias previas",
       y = "Nota de Estadística",
       color = "Rendimiento") +
  scale_color_manual(values = c("Alto" = "seagreen", "Bajo" = "firebrick")) +
  theme_minimal()


print("El gráfico muestra una clara relación positiva entre el promedio de las materias previas (Álgebra, Cálculo y Programación) y la nota obtenida en Estadística.
Al agrupar por promedio, se observa que los estudiantes con un promedio mayor o igual a 7.5 (color verde) tienden a alcanzar notas más altas en Estadística, en comparación con quienes tienen un promedio menor a 7.5 (color rojo), quienes tienden a concentrarse en calificaciones más bajas.
Esta visualización refuerza la idea de que el buen desempeño previo está fuertemente asociado con un mejor rendimiento en Estadística, lo que convierte al promedio de las materias anteriores en un indicador valioso para predecir el desempeño académico futuro.")





##########################################################################

    #ANALISIS BIVARIANTE (Nota de Estadística vs Horario)

##########################################################################

# Boxplot de Nota de Estadística por Horario Tomado
ggplot(datos, aes(x = `HORARIO TOMADO`, y = `NOTA ESTADÍSTICA`, fill = `HORARIO TOMADO`)) +
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



##########################################################################
 
             #ANALISIS BIVARIANTE (Rendimiento vs Horario)
 
##########################################################################

datos <- datos %>%
  mutate(rendimiento_previo = ifelse(promedio >= 7.5, "Alto", "Bajo"))

# Convertir a factor para orden visual
datos$rendimiento_previo <- factor(datos$rendimiento_previo, levels = c("Bajo", "Alto"))

# Gráfico de barras: Rendimiento previo vs Horario de Estadística
ggplot(datos, aes(x = `HORARIO TOMADO`, fill = rendimiento_previo)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Comparación de Rendimiento Previo según Horario de Estadística",
       x = "Horario Tomado de Estadística",
       y = "Cantidad de Estudiantes",
       fill = "Rendimiento Previo") +
  scale_fill_manual(values = c("Bajo" = "salmon", "Alto" = "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


tabla_chi <- table(datos$`HORARIO TOMADO`, datos$rendimiento_previo)
chi_resultado <- chisq.test(tabla_chi)
print("Prueba Chi-cuadrado: Asociación entre Horario y Rendimiento Previo")
print(chi_resultado)
print("Esto mostró que son independientes Y SEGUN GABRIEL TIENE TODO EL SENTIDO DEL MUNDO")


print("El análisis estadístico (Chi-cuadrado, p = 0.503) no evidencia una asociación significativa entre el 
      horario de Estadística (07h00-09h00 vs. 09h00-11h00) y el rendimiento académico previo (Alto/Bajo) 
      de los estudiantes. Esto sugiere que la distribución de estudiantes con alto/bajo desempeño previo es 
      similar en ambos horarios, descartando que la elección de horario esté vinculada a diferencias en su 
      preparación académica anterior.")









