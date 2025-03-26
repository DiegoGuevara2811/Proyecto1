package Proyecto;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Interpreter interpreter = new Interpreter();

        System.out.println("Intérprete Lisp en Java. Escribe 'exit' para salir.");

        while (true) {
            System.out.print("Lisp> ");
            String input = scanner.nextLine().trim();

            if (input.equalsIgnoreCase("exit")) {
                break;
            }

            if (input.isEmpty()) {
                continue;
            }

            try {
                Object result = interpreter.interpret(input);
                // Solo imprimir si el resultado no es null y no es una llamada a print
                if (result != null && !input.trim().startsWith("(print")) {
                    System.out.println(result.toString());
                }
            } catch (Exception e) {
                System.err.println("Error: " + e.getMessage());
            }
        }

        scanner.close();
        System.out.println("Saliendo del intérprete Lisp.");
    }
}