package Proyecto;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

public class Main {
    public static void main(String[] args) {
        try {
            // Leer el archivo desde resources
            InputStream inputStream = Main.class.getClassLoader().getResourceAsStream("codigo.lisp");
            if (inputStream == null) {
                System.err.println("Error: No se encontró codigo.lisp en resources");
                System.exit(1);
            }

            String content = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
            inputStream.close();

            // Interpretar el contenido
            Interpreter interpreter = new Interpreter();
            Object result = interpreter.interpret(content);

            // Mostrar resultado
            if (result != null) {
                System.out.println("Resultado de codigo.lisp: " + result);
            }
        } catch (IOException e) {
            System.err.println("Error al leer el archivo: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Error al interpretar: " + e.getMessage());
            System.exit(1);
        }
    }
}