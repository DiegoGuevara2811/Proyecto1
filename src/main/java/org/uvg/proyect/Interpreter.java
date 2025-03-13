package org.uvg.proyect;
import java.util.*;
import java.util.Scanner;

public class Interpreter {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Environment env = new Environment();
        Evaluator evaluator = new Evaluator(env);

        System.out.println("Ingrese código Lisp:");
        String code = scanner.nextLine();

        List<String> tokens = Lexer.tokenize(code);
        Parser parser = new Parser(tokens);
        Object parsedExpression = parser.parse();

        System.out.println("Parseada: " + parsedExpression);

        Object result = evaluator.eval(parsedExpression);
        System.out.println("Resultado: " + result);

        scanner.close();
    }
}
