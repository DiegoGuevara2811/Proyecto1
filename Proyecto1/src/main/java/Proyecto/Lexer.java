package Proyecto;

import java.util.ArrayList;
import java.util.List;

public class Lexer {
    private final String input;
    private int position;

    public Lexer(String input) {
        this.input = input;
        this.position = 0;
    }

    public List<String> tokenize() {
        List<String> tokens = new ArrayList<>();
        skipWhitespace();

        while (position < input.length()) {
            char current = input.charAt(position);

            if (current == '(' || current == ')') {
                tokens.add(String.valueOf(current));
                position++;
            } else if (current == '"') {
                tokens.add(readString());
            } else if (Character.isDigit(current) || (current == '-' && position + 1 < input.length() && Character.isDigit(input.charAt(position + 1)))) {
                tokens.add(readNumber());
            } else {
                tokens.add(readSymbol());
            }

            skipWhitespace();
        }

        return tokens;
    }

    private String readString() {
        position++; // Skip opening quote
        StringBuilder sb = new StringBuilder();
        sb.append('"');

        while (position < input.length() && input.charAt(position) != '"') {
            sb.append(input.charAt(position));
            position++;
        }

        if (position >= input.length()) {
            throw new RuntimeException("String no terminada");
        }

        sb.append('"');
        position++; // Skip closing quote
        return sb.toString();
    }

    private String readNumber() {
        StringBuilder sb = new StringBuilder();
        if (input.charAt(position) == '-') {
            sb.append('-');
            position++;
        }

        while (position < input.length() && Character.isDigit(input.charAt(position))) {
            sb.append(input.charAt(position));
            position++;
        }

        return sb.toString();
    }

    private String readSymbol() {
        StringBuilder sb = new StringBuilder();
        while (position < input.length() && !Character.isWhitespace(input.charAt(position)) &&
                input.charAt(position) != '(' && input.charAt(position) != ')') {
            sb.append(input.charAt(position));
            position++;
        }
        return sb.toString();
    }

    private void skipWhitespace() {
        while (position < input.length() && Character.isWhitespace(input.charAt(position))) {
            position++;
        }
    }
}