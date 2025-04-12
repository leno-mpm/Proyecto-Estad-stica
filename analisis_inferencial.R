

##########################################################################

      #PRUEBA T DE DIFERENCIA DE MEDIAS DE ALTO Y BAJO RENDIMIENTO

##########################################################################


print("Ayuda a validar o descartar la hipótesis de que el desempeño en materias básicas es predictivo del éxito en Estadística.")

# Grupo de alto rendimiento (promedio previo ≥ 7.5)
# Grupo de bajo rendimiento (promedio previo < 7.5)

# Hipótesis nula (H₀): No existe una diferencia significativa en la capacidad predictiva del promedio de las materias 
# previas sobre la nota de Estadística entre los estudiantes de alto y bajo rendimiento.

# Hipótesis alternativa (Hₐ): Existe una diferencia significativa en la capacidad predictiva del promedio de las materias 
# previas sobre la nota de Estadística entre los estudiantes de alto y bajo rendimiento.


# H₀: μA = μB
# Hₐ: μA ≠ μB


datos$rendimiento <- ifelse(datos$promedio >= 7.5, "Alto", "Bajo")

prueba_t <- t.test(`NOTA ESTADÍSTICA` ~ rendimiento, data = datos)
# prueba_t <- t.test(`NOTA ESTADÍSTICA` ~ rendimiento, data = datos)
print(prueba_t)

if (prueba_t$p.value < 0.05) {
  print("Rechazamos la hipótesis nula - Existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
} else {
  print("No rechazamos la hipótesis nula - No hay suficiente evidencia para afirmar que existe una diferencia significativa en las medias de los estudiantes de alto y bajo rendimiento en la nota de Estadística.")
}

print("Existe una diferencia significativa en las medias de las notas de Estadística entre 
      los estudiantes de alto y bajo rendimiento (según el promedio de materias previas). 
      Esto sugiere que el desempeño en las materias previas (como Álgebra, Cálculo y 
      Fundamentos de Programación) tiene un impacto real sobre el rendimiento en Estadística. 
      Los estudiantes con un promedio más alto en estas materias tienden a obtener mejores 
      resultados en Estadística, mientras que los estudiantes con un promedio más bajo en las 
      materias previas tienen un rendimiento inferior en la asignatura de Estadística")





####################################################################################################

  #PRUEBA T DE DIFERENCIAS DE MEDIAS CON ESTUDIANTES DE ALTO RENDIMIENTO EN BASE AL HORARIO TOMADO

####################################################################################################

 
# Hipótesis nula (H₀): No existe una diferencia significativa en las notas de Estadística entre 
#los estudiantes de alto rendimiento que tomaron diferentes horarios.
 
# Hipótesis alternativa (Hₐ): Existen diferencias significativas en las notas de Estadística 
#entre los estudiantes de alto rendimiento según el horario tomado.
 
# A: Estudiantes que tomaron el horario de 07:00 - 09:00
# B: Estudiantes que tomaron el horario de 09:00 - 11:00


# H₀: μA = μB
# Hₐ: μA ≠ μB

datos_alto_rendimiento <- datos %>%
  filter(promedio >= 7.5)

prueba_t_horario <- t.test(`NOTA ESTADÍSTICA` ~ `HORARIO TOMADO`, data = datos_alto_rendimiento)
print(prueba_t_horario)

if (prueba_t_horario$p.value < 0.05) {
  print("Rechazamos la hipótesis nula - Existe una diferencia significativa en las notas de 
        Estadística entre los estudiantes de alto rendimiento según el horario tomado.")
} else {
  print("No rechazamos la hipótesis nula - No hay suficiente evidencia para afirmar que existe una diferencia significativa en las notas de Estadística entre los estudiantes de alto rendimiento según el horario tomado.")
}





####################################################################################################

  #PRUEBA T DE DIFERENCIAS DE MEDIAS CON ESTUDIANTES DE BAJO RENDIMIENTO EN BASE AL HORARIO TOMADO

####################################################################################################

# Hipótesis nula (H₀): No existe una diferencia significativa en las notas de Estadística entre 
#los estudiantes de bajo rendimiento que tomaron diferentes horarios.

# Hipótesis alternativa (Hₐ): Existen diferencias significativas en las notas de Estadística 
#entre los estudiantes de bajo rendimiento según el horario tomado.

# A: Estudiantes que tomaron el horario de 07:00 - 09:00
# B: Estudiantes que tomaron el horario de 09:00 - 11:00


# H₀: μA = μB
# Hₐ: μA ≠ μB

prueba_t_horario_bajo <- t.test(`NOTA ESTADÍSTICA` ~ `HORARIO TOMADO`, data = bajo_rendimiento)

print(prueba_t_horario_bajo)

if (prueba_t_horario_bajo$p.value < 0.05) {
  print("Rechazamos la hipótesis nula - Existe una diferencia significativa en las notas de Estadística entre los estudiantes de bajo rendimiento según el horario tomado.")
} else {
  print("No rechazamos la hipótesis nula - No hay suficiente evidencia para afirmar que existe una diferencia significativa en las notas de Estadística entre los estudiantes de bajo rendimiento según el horario tomado.")
}





####################################################################################################

    #COMPARACIÓN DE PROPORCIONES DE ESTUDIANTES DE ALTO RENDIMIENTO ENTRE HORARIOS DE CLASE

####################################################################################################

print("¿La proporción de buenos estudiantes es diferente entre 7-9 y 9-11?")

datos$rendimiento <- ifelse(datos$promedio >= 7.5, "Alto", "Bajo")
datos_9_11 <- datos %>%
  filter(`HORARIO TOMADO` == "07h00 - 09h00")
datos_11_1 <- datos %>%
  filter(`HORARIO TOMADO` == "09h00 - 11h00")

prop_9_11 <- mean(datos_9_11$rendimiento == "Alto")
prop_11_1 <- mean(datos_11_1$rendimiento == "Alto")
tabla_contingencia <- table(datos$rendimiento, datos$`HORARIO TOMADO`)

prueba_chi <- chisq.test(tabla_contingencia)
print(prueba_chi)

if (prueba_chi$p.value < 0.05) {
  print("La proporción de estudiantes con rendimiento alto es diferente entre los horarios 7-9 y 9-11.")
} else {
  print("No hay suficiente evidencia para afirmar que la proporción de estudiantes con rendimiento alto es diferente entre los horarios 9-11 y 11-1.")
}

print("Aunque la proporción de estudiantes de alto rendimiento no varía significativamente entre 
      los horarios 7-9 y 9-11, sí se observa una diferencia significativa en las notas de 
      Estadística dentro del grupo de alto rendimiento, lo que sugiere que el horario podría influir 
      en el desempeño de los estudiantes con buen historial académico.")


