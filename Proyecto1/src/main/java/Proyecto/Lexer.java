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

            if (current == '(' || current == ')' || current == '\'') {
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
        StringBuilder sb = new StringBuilder();
        sb.append('"');
        position++;

        while (position < input.length() && input.charAt(position) != '"') {
            sb.append(input.charAt(position));
            position++;
        }

        if (position >= input.length()) throw new RuntimeException("String no terminada");
        sb.append('"');
        position++;
        return sb.toString();
    }

    private String readNumber() {
        StringBuilder sb = new StringBuilder();
        boolean hasDecimal = false;

        if (input.charAt(position) == '-') {
            sb.append('-');
            position++;
        }

        while (position < input.length()) {
            char c = input.charAt(position);
            if (Character.isDigit(c)) {
                sb.append(c);
                position++;
            } else if (c == '.' && !hasDecimal) {
                hasDecimal = true;
                sb.append(c);
                position++;
            } else {
                break;
            }
        }
        return sb.toString();
    }

    private String readSymbol() {
        StringBuilder sb = new StringBuilder();
        while (position < input.length()) {
            char c = input.charAt(position);
            if (Character.isWhitespace(c) || c == '(' || c == ')') break;
            sb.append(c);
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