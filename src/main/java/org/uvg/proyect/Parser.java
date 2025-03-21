package org.uvg.proyect;

import java.util.*;

public class Parser {
    private List<String> tokens;
    private int position;

    public Parser(List<String> tokens) {
        this.tokens = tokens;
        this.position = 0;
    }

    public Object parse() {
        if (position >= tokens.size()) {
            throw new RuntimeException("Error de sintaxis: Expresión incompleta.");
        }

        String token = tokens.get(position++);

        if (token.equals("(")) {
            List<Object> list = new ArrayList<>();
            // Comprobación de fin de tokens
            while (position < tokens.size() && !tokens.get(position).equals(")")) {
                list.add(parse());
                // Comprobación adicional después de cada elemento analizado
                if (position >= tokens.size()) {
                    throw new RuntimeException("Error de sintaxis: Paréntesis de cierre faltante.");
                }
            }

            if (position >= tokens.size()) {
                throw new RuntimeException("Error de sintaxis: Paréntesis de cierre faltante.");
            }

            position++; // Saltar ')'
            return list;
        } else if (token.equals(")")) {
            throw new RuntimeException("Error de sintaxis: Paréntesis de cierre inesperado.");
        } else {
            // Intenta convertir a número si es posible
            try {
                return Integer.parseInt(token);
            } catch (NumberFormatException e) {
                // Si no es un número, devuelve el token como string
                return token;
            }
        }
    }
}
