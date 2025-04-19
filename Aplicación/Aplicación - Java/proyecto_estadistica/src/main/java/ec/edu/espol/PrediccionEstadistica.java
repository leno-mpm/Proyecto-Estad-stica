package ec.edu.espol;

import java.util.Scanner;

public class PrediccionEstadistica {
    public static void main(String[] args) {
        @SuppressWarnings("resource")
        Scanner sc = new Scanner(System.in);
        System.out.print("Ingrese la carrera (Mecatrónica o Computación): ");
        String carrera = sc.next();
        System.out.print("Ingrese la nota de Álgebra: ");
        double algebra = sc.nextDouble();
        System.out.print("Ingrese la nota de Cálculo: ");
        double calculo = sc.nextDouble();
        System.out.print("Ingrese la nota de Fund. de Programación: ");
        double fundProg = sc.nextDouble();
        sc.nextLine();
 
        double promedio = (algebra + calculo + fundProg) / 3.0;
        double prediccion;

        if (carrera.equalsIgnoreCase("mecatronica"))  {
            prediccion = predecirMeca(promedio);
        } else if (carrera.equalsIgnoreCase("computacion")) {
            prediccion = predecirComp(promedio);
        } else {
            System.out.println("Carrera no válida. Usa 'Mecatrónica' o 'Computación'.");
            return;
        }
 
        System.out.printf("\nPromedio ingresado: %.2f\n", promedio);
        System.out.printf("Predicción de Nota de Estadística para %s: %.2f\n", carrera, prediccion);
     }
 
    public static double predecirMeca(double promedio) {
        return 1.2467 * promedio -2.5655; 
    }
    public static double predecirComp(double promedio) {
        return 1.0199 * promedio -0.6576;  
    }
 }
 

    