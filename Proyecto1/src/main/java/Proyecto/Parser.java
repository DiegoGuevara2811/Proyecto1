package Proyecto;

import java.util.*;

public class Parser {
    private final List<String> tokens;
    private int position;

    public Parser(List<String> tokens) {
        this.tokens = tokens;
        this.position = 0;
    }

    public List<Object> parse() {
        if (tokens.isEmpty()) throw new RuntimeException("No hay expresiones para evaluar");
        List<Object> parsed = new ArrayList<>();
        while (position < tokens.size()) parsed.add(parseExpression());
        return parsed;
    }

    private List<Object> parseExpression() {
        if (position >= tokens.size()) throw new RuntimeException("Se esperaba una expresión");
        String token = consumeToken("Se esperaba una expresión");

        if (!token.equals("(")) {
            position--;
            return Collections.singletonList(parseAtom());
        }

        List<Object> expression = new ArrayList<>();
        while (position < tokens.size() && !peekToken().equals(")")) {
            token = peekToken();
            if (token.equals("(")) {
                expression.add(parseExpression());
            } else {
                expression.add(parseAtom());
            }
        }

        if (position >= tokens.size()) throw new RuntimeException("Expresión no terminada, falta ')'");
        consumeToken("Se esperaba ')' para cerrar la expresión");
        return expression;
    }

    private Object parseAtom() {
        String token = consumeToken("Se esperaba un átomo");
        //System.out.println(token);
        if (token.startsWith("\"") && token.endsWith("\"")) {
            return token.substring(1, token.length() - 1);
        }

        if (token.equalsIgnoreCase("t")) return true;
        if (token.equalsIgnoreCase("nil")) return false;

        try {
            return Integer.parseInt(token);
        } catch (NumberFormatException e1) {
            try {
                return Double.parseDouble(token);
            } catch (NumberFormatException e2) {
                return token;
            }
        }
    }

    private String consumeToken(String errorMessage) {
        if (position >= tokens.size()) throw new RuntimeException(errorMessage);
        return tokens.get(position++);
    }

    private String peekToken() {
        return position >= tokens.size() ? ")" : tokens.get(position);
    }
}