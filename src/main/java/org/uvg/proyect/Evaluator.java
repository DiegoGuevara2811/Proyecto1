package org.uvg.proyect;

import java.util.*;

public class Evaluator {
    private Environment env;

    public Evaluator(Environment env) {
        this.env = env;
    }

    public Object eval(Object expr) {
        if (expr instanceof String) {
            // Si es una variable, la obtenemos del entorno
            return env.getVar((String) expr);
        } else if (expr instanceof Integer) {
            // Valores numéricos se devuelven directamente
            return expr;
        } else if (expr instanceof List) {
            List<?> list = (List<?>) expr;
            if (list.isEmpty()) {
                throw new RuntimeException("Expresión vacía");
            }

            Object firstElem = list.get(0);
            // Verificamos que el primer elemento sea una operación (String)
            if (!(firstElem instanceof String)) {
                throw new RuntimeException("Operador inválido: " + firstElem);
            }

            String op = (String) firstElem;
            switch (op) {
                case "+":
                    return toInt(eval(list.get(1))) + toInt(eval(list.get(2)));
                case "-":
                    return toInt(eval(list.get(1))) - toInt(eval(list.get(2)));
                case "*":
                    return toInt(eval(list.get(1))) * toInt(eval(list.get(2)));
                case "/":
                    return toInt(eval(list.get(1))) / toInt(eval(list.get(2)));
                case "quote":
                    if (list.size() < 2) {
                        throw new RuntimeException("quote requiere un argumento");
                    }
                    return list.get(1);
                case "setq":
                    if (list.size() < 3) {
                        throw new RuntimeException("setq requiere dos argumentos");
                    }
                    if (!(list.get(1) instanceof String)) {
                        throw new RuntimeException("El primer argumento de setq debe ser un nombre de variable");
                    }
                    String varName = (String) list.get(1);
                    Object value = eval(list.get(2));
                    env.setVar(varName, value);
                    return value;
                default:
                    throw new RuntimeException("Operación desconocida: " + op);
            }
        } else {
            return expr; // Es un número o valor literal
        }
    }

    // Metodo para convertir a entero
    private int toInt(Object obj) {
        if (obj instanceof Integer) {
            return (Integer) obj;
        } else if (obj instanceof String) {
            try {
                return Integer.parseInt((String) obj);
            } catch (NumberFormatException e) {
                throw new RuntimeException("No se puede convertir a número: " + obj);
            }
        } else {
            throw new RuntimeException("No se puede convertir a número: " + obj);
        }
    }
}