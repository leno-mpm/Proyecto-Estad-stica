library(readxl)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

archivo <- "Datos_Proyecto.xlsx"
datos <- read_excel(archivo)

datos <- datos %>%
  mutate(
    `NOTA ESTADÍSTICA` = as.numeric(`NOTA ESTADÍSTICA`),
    `NOTA ÁLGEBRA` = as.numeric(`NOTA ÁLGEBRA`),
    `NOTA CÁLCULO` = as.numeric(`NOTA CÁLCULO`),
    `NOTA FUND. PROG.` = as.numeric(`NOTA FUND. PROG.`)
  )
  


##########################################################################
    
           #MODELO DE REGRESIÓN LINEAL ~ ESTUDIANTES DE MECATRÓNICA

########################################################################## 

datos_meca <- datos %>%
  filter(CARRERA == "Mecatrónica") %>%
  mutate(promedio = rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE))


modelo_Meca <- lm(`NOTA ESTADÍSTICA` ~ promedio, data = datos_meca)
summary_modelo <- summary(modelo_Meca)
summary_modelo


# Gráfico con línea de regresión y línea horizontal de la media
media_y <- mean(datos_meca$`NOTA ESTADÍSTICA`)
ggplot(datos_meca, aes(x = promedio, y = `NOTA ESTADÍSTICA`)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  geom_hline(yintercept = media_y, color = "forestgreen", linetype = "dotted", size = 1) +
  labs(
    title = "Relación entre Promedio y Nota de Estadística (Mecatrónica)",
    subtitle = paste("Línea verde punteada: ȳ =", round(media_y, 2)),
    x = "Promedio (Álgebra, Cálculo, Fund. de Prog.)",
    y = "Nota de Estadística"
  ) +
  theme_minimal()


# Estimación y prueba para el intercepto (β0)
beta0 <- summary_modelo$coefficients["(Intercept)", ]
cat("Prueba para β0 (intercepto):\n")
cat("H0: β0 = 0\n")
cat("Ha: β0 ≠ 0\n")
cat(paste0("t = ", round(beta0["t value"], 4), ", p = ", round(beta0["Pr(>|t|)"], 4), "\n\n"))
print("CONCLUSIÓN: No hay suficiente evidencia estadística para rechazar la hipótesis nula. 
      Con un valor p = 0.1702 (> 0.05), no se puede afirmar que el intercepto (β₀) sea 
      significativamente diferente de cero. Esto indica que cuando el promedio es igual a cero, 
      la predicción de la nota de Estadística no es significativamente distinta de cero, aunque 
      este resultado tiene poca relevancia práctica dado que un promedio de cero no es realista 
      en este contexto.")



# Estimación y prueba para la pendiente (β1)
beta1 <- summary_modelo$coefficients["promedio", ]
cat("Prueba para β1 (pendiente):\n")
cat("H0: β1 = 0\n")
cat("Ha: β1 ≠ 0\n")
cat(paste0("t = ", round(beta1["t value"], 4), ", p = ", round(beta1["Pr(>|t|)"], 4), "\n\n"))
print("CONCLUSIÓN: Existe suficiente evidencia estadística para rechazar la hipótesis nula. 
      Con un valor p = 0.0001 (< 0.05), se concluye que la pendiente (β₁) es significativamente
      diferente de cero. Esto indica que el promedio en las materias previas tiene un efecto 
      significativo sobre la nota de Estadística en los estudiantes de Mecatrónica.")


# Análisis de Varianza ~ Modelo Anova
anova_modelo <- anova(modelo_Meca)
cat("Prueba ANOVA:\n")
cat("H0: β1 = 0 (el modelo no explica varianza)\n")
cat("Ha: β1 ≠ 0 (el modelo explica varianza en la nota de Estadística)\n")
print(anova_modelo)
print("La prueba ANOVA realizada para el modelo de regresión lineal de la variable 'Nota de Estadística'
      en función del 'Promedio' revela que la pendiente (coeficiente de 'promedio') tiene un efecto 
      significativo sobre la nota de Estadística.
      Específicamente, el valor F de la prueba es 24.687, con un valor p de 5.685e-05, que es mucho menor 
      que el umbral de significancia comúnmente utilizado de 0.05.
      Esto indica que hay suficiente evidencia para rechazar la hipótesis nula (H₀: β₁ = 0) y concluir
      que el promedio tiene un impacto significativo sobre la nota de Estadística.
      En resumen, el modelo sugiere que la variación en el promedio de las materias previas
      tiene un efecto significativo sobre la nota de Estadística de los estudiantes.")





##########################################################################

        #MODELO DE REGRESIÓN LINEAL ~ ESTUDIANTES DE COMPUTACIÓN

########################################################################## 

datos_comp <- datos %>%
  filter(CARRERA == "Computación") %>%
  mutate(promedio = rowMeans(select(., `NOTA ÁLGEBRA`, `NOTA CÁLCULO`, `NOTA FUND. PROG.`), na.rm = TRUE))


modelo_Comp <- lm(`NOTA ESTADÍSTICA` ~ promedio, data = datos_comp)
summary_modelo2 <- summary(modelo_Comp)
summary_modelo2


# Gráfico con línea de regresión y línea horizontal de la media
media_y <- mean(datos_comp$`NOTA ESTADÍSTICA`)
ggplot(datos_comp, aes(x = promedio, y = `NOTA ESTADÍSTICA`)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  geom_hline(yintercept = media_y, color = "forestgreen", linetype = "dotted", size = 1) +
  labs(
    title = "Relación entre Promedio y Nota de Estadística (Mecatrónica)",
    subtitle = paste("Línea verde punteada: ȳ =", round(media_y, 2)),
    x = "Promedio (Álgebra, Cálculo, Fund. de Prog.)",
    y = "Nota de Estadística"
  ) +
  theme_minimal()


# Estimación y prueba para el intercepto (β0)
beta0 <- summary_modelo2$coefficients["(Intercept)", ]
cat("Prueba para β0 (intercepto):\n")
cat("H0: β0 = 0\n")
cat("Ha: β0 ≠ 0\n")
cat(paste0("t = ", round(beta0["t value"], 4), ", p = ", round(beta0["Pr(>|t|)"], 4), "\n\n"))
print("Conclusión para la prueba del intercepto (β₀): 
Dado que el valor p es 0.684, el cual es mayor que 0.05, no se rechaza la hipótesis nula H₀: β₀ = 0. 
Esto indica que no hay evidencia estadísticamente significativa para afirmar que el intercepto del modelo 
de regresión lineal es diferente de cero. En otras palabras, el valor del intercepto podría no tener un 
impacto relevante en la predicción de la nota de Estadística cuando el promedio previo es cero.")



# Estimación y prueba para la pendiente (β1)
beta1 <- summary_modelo2$coefficients["promedio", ]
cat("Prueba para β1 (pendiente):\n")
cat("H0: β1 = 0\n")
cat("Ha: β1 ≠ 0\n")
cat(paste0("t = ", round(beta1["t value"], 4), ", p = ", round(beta1["Pr(>|t|)"], 4), "\n\n"))
print("Conclusión para la prueba de la pendiente (β₁): 
Dado que el valor p es 0, el cual es menor que 0.05, se rechaza la hipótesis nula H₀: β₁ = 0. 
Esto indica que existe evidencia estadísticamente significativa para afirmar que la pendiente del modelo 
(lineal entre el promedio previo y la nota de Estadística) es diferente de cero. 
Por lo tanto, el promedio previo tiene un efecto significativo en la predicción de la nota de Estadística.")


# Análisis de Varianza ~ Modelo Anova
anova_modelo2 <- anova(modelo_Comp)
cat("Prueba ANOVA:\n")
cat("H0: β1 = 0 (el modelo no explica varianza)\n")
cat("Ha: β1 ≠ 0 (el modelo explica varianza en la nota de Estadística)\n")
print(anova_modelo2)
print("Dado que el valor p (p = 1.673e-05) es menor que 0.05, se rechaza la hipótesis nula H₀: β₁ = 0. 
Esto indica que el modelo de regresión lineal es significativo en su conjunto. 
Es decir, existe evidencia estadística suficiente para afirmar que el promedio previo tiene un efecto 
significativo sobre la nota de Estadística.")










##########################################################################

                                 #PREDICCIÓN

########################################################################## 

# Ingresar notas manualmente
algebra <- as.numeric(readline(prompt = "Ingrese la nota de Álgebra: "))
calculo <- as.numeric(readline(prompt = "Ingrese la nota de Cálculo: "))
fund_prog <- as.numeric(readline(prompt = "Ingrese la nota de Fund. de Programación: "))

carrera <- readline(prompt = "Ingrese la carrera (Mecatrónica o Computación): ")
promedio_nuevo <- mean(c(algebra, calculo, fund_prog), na.rm = TRUE)
nuevo_dato <- data.frame(promedio = promedio_nuevo)

# Realizar la predicción
if (tolower(carrera) == "mecatrónica") {
  prediccion <- predict(modelo_Meca, newdata = nuevo_dato)
} else if (tolower(carrera) == "computación") {
  prediccion <- predict(modelo_Comp, newdata = nuevo_dato)
} else {
  stop("Carrera no válida. Usa 'Mecatrónica' o 'Computación'.")
}

# Mostrar resultado
cat("\nPromedio ingresado:", round(promedio_nuevo, 2), "\n")
cat("Predicción de Nota de Estadística para", carrera, ":", round(prediccion, 2), "\n")


