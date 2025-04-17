#ola aqui van sus modelos 

#########################################################################################

                              # MODELO REGRESION LINEAL: 
                      #NOTA DE ESTADÍSTICA SEGUN PROMEDIOS BAJOS

#########################################################################################

notas_estadistica_bajoR <- bajo_rendimiento$`NOTA ESTADÍSTICA`
print(notas_estadistica_bajoR)

promedio_bajoR <- bajo_rendimiento$promedio
print(promedio_bajoR)

#Grafico de dispersion
datos_modelo_bajoR <- data.frame(promedio_bajoR, notas_estadistica_bajoR)
ggplot(datos_modelo_bajoR, aes(x=promedio_bajoR, y= notas_estadistica_bajoR)) +
  geom_point() + 
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, col= 'dodgerblue1') + 
  theme_light()


modelo_bajoR <- lm(notas_estadistica_bajoR ~ promedio_bajoR, data = datos_modelo_bajoR)
summary(modelo_bajoR)


print("El modelo no es bueno, ya que el R salió demasiado pequeño para ser considerado buen modelo
      es decir hay mucha variación de notas. Para estudiantes de bajo rendimiento existen muchos factores
      a tomar en cuenta, el comportamiento humano es complejo y no se puede predecir la nota de un estudiante
      de bajo rendimiento con una sola variable, ya que pueden haber otras variables q influyan, como el gusto
      de las materias, presión social, profesor, etc.")

