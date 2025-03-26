package Proyecto;

import java.util.ArrayList;
import java.util.List;

public class Parser {
    private final List<String> tokens;
    private int position;

    public Parser(List<String> tokens) {
        this.tokens = tokens;
        this.position = 0;
    }

    public List<Object> parse() {
        if (tokens.isEmpty()) {
            throw new RuntimeException("No hay tokens para parsear");
        }

        List<Object> parsed = parseExpression();

        if (position < tokens.size()) {
            throw new RuntimeException("Tokens adicionales no esperados: " + tokens.subList(position, tokens.size()));
        }

        return parsed;
    }

    private List<Object> parseExpression() {
        String token = consumeToken();

        if (!token.equals("(")) {
            throw new RuntimeException("Se esperaba '(' al inicio de expresión");
        }

        List<Object> expression = new ArrayList<>();

        while (position < tokens.size() && !peekToken().equals(")")) {
            if (peekToken().equals("(")) {
                expression.add(parseExpression());
            } else {
                expression.add(parseAtom());
            }
        }

        if (position >= tokens.size()) {
            throw new RuntimeException("Se esperaba ')' al final de expresión");
        }

        consumeToken(); // Consume the closing ')'
        return expression;
    }

    private Object parseAtom() {
        String token = consumeToken();

        try {
            return Integer.parseInt(token);
        } catch (NumberFormatException e1) {
            try {
                return Double.parseDouble(token);
            } catch (NumberFormatException e2) {
                if (token.startsWith("\"") && token.endsWith("\"")) {
                    return token.substring(1, token.length() - 1);
                }
                return token;
            }
        }
    }

    private String consumeToken() {
        if (position >= tokens.size()) {
            throw new RuntimeException("Se esperaban más tokens");
        }
        return tokens.get(position++);
    }

    private String peekToken() {
        if (position >= tokens.size()) {
            throw new RuntimeException("Se esperaban más tokens");
        }
        return tokens.get(position);
    }
}

