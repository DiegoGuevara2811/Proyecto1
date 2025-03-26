package Proyecto;

import java.util.*;

public class Evaluator {
    private final Environment env;

    public Evaluator(Environment env) {
        this.env = env;
    }

    public Object evaluate(List<Object> expression) {
        if (expression.isEmpty()) {
            return null;
        }

        Object first = expression.get(0);

        if (!(first instanceof String)) {
            return evalAtom(expression);
        }

        String operator = (String) first;

        switch (operator) {
            case "print":
                return evalPrint(expression);
            case "+":
                return evalArithmetic(expression, 0.0, (a, b) -> a + b);
            case "-":
                return evalArithmetic(expression, 0.0, (a, b) -> a - b);
            case "*":
                return evalArithmetic(expression, 1.0, (a, b) -> a * b);
            case "/":
                return evalDivision(expression);
            case "setq":
                return evalSetq(expression);
            case "defun":
                return evalDefun(expression);
            case "if":
                return evalIf(expression);
            case "quote":
                return expression.size() > 1 ? expression.get(1) : null;
            default:
                return evalFunctionCall(expression);
        }
    }

    private Object evalAtom(Object expr) {
        if (expr instanceof List) {
            return evaluate((List<Object>) expr);
        } else if (expr instanceof String) {
            String str = (String) expr;
            if (str.startsWith("\"")) {
                return str.substring(1, str.length() - 1);
            }
            return env.getVariable(str);
        }
        return expr;
    }

    private Object evalPrint(List<Object> expression) {
        if (expression.size() < 2) {
            throw new RuntimeException("print requiere al menos un argumento");
        }
        Object value = evalAtom(expression.get(1));
        System.out.println(value);  // Solo se imprime aquí
        return value;  // También se devuelve el valor
    }

    private Number evalArithmetic(List<Object> expression, double initial, ArithmeticOperation op) {
        if (expression.size() < 2) {
            throw new RuntimeException("Operación requiere al menos un argumento");
        }

        double result = initial;
        boolean allIntegers = true;

        for (int i = 1; i < expression.size(); i++) {
            Object evaluated = evalAtom(expression.get(i));
            if (!(evaluated instanceof Number)) {
                throw new RuntimeException("Argumento no numérico: " + evaluated);
            }

            Number num = (Number) evaluated;
            double currentValue = num.doubleValue();

            if (allIntegers && !isInteger(num)) {
                allIntegers = false;
            }

            if (i == 1 && expression.size() == 2) {
                result = currentValue;
            } else {
                result = op.apply(result, currentValue);
            }
        }

        if (allIntegers && result == (int)result) {
            return (int)result;
        }
        return result;
    }

    private Number evalDivision(List<Object> expression) {
        if (expression.size() != 3) {
            throw new RuntimeException("División requiere exactamente 2 argumentos");
        }

        Object first = evalAtom(expression.get(1));
        Object second = evalAtom(expression.get(2));

        if (!(first instanceof Number) || !(second instanceof Number)) {
            throw new RuntimeException("División requiere argumentos numéricos");
        }

        double numerator = ((Number) first).doubleValue();
        double denominator = ((Number) second).doubleValue();

        if (denominator == 0) {
            throw new RuntimeException("División por cero");
        }

        double result = numerator / denominator;

        if (isInteger((Number)numerator) && isInteger((Number)denominator) && result == (int)result) {
            return (int)result;
        }
        return result;
    }

    private Object evalSetq(List<Object> expression) {
        if (expression.size() != 3) {
            throw new RuntimeException("setq requiere exactamente 2 argumentos");
        }

        if (!(expression.get(1) instanceof String)) {
            throw new RuntimeException("setq requiere un nombre de variable como primer argumento");
        }

        String varName = (String) expression.get(1);
        Object value = evalAtom(expression.get(2));
        env.setVariable(varName, value);
        return value;
    }

    private Object evalDefun(List<Object> expression) {
        if (expression.size() != 4) {
            throw new RuntimeException("defun requiere 3 argumentos: nombre, parámetros y cuerpo");
        }

        if (!(expression.get(1) instanceof String)) {
            throw new RuntimeException("Nombre de función debe ser un símbolo");
        }

        if (!(expression.get(2) instanceof List)) {
            throw new RuntimeException("Parámetros deben ser una lista");
        }

        String name = (String) expression.get(1);
        List<String> params = (List<String>) expression.get(2);
        List<Object> body = (List<Object>) expression.get(3);

        env.defineFunction(name, new Function(name, params, body));
        return name;
    }

    private Object evalIf(List<Object> expression) {
        if (expression.size() != 4) {
            throw new RuntimeException("if requiere 3 argumentos: condición, entonces, sino");
        }

        Object condition = evalAtom(expression.get(1));
        if (!(condition instanceof Boolean)) {
            throw new RuntimeException("Condición debe evaluar a booleano");
        }

        return (Boolean) condition ? evalAtom(expression.get(2)) : evalAtom(expression.get(3));
    }

    private Object evalFunctionCall(List<Object> expression) {
        String functionName = (String) expression.get(0);
        Function function = env.getFunction(functionName);

        List<Object> args = new ArrayList<>();
        for (int i = 1; i < expression.size(); i++) {
            args.add(evalAtom(expression.get(i)));
        }

        return function.call(args, env);
    }

    private boolean isInteger(Number num) {
        if (num instanceof Integer) {
            return true;
        }
        if (num instanceof Double) {
            double d = num.doubleValue();
            return d == (int)d;
        }
        return false;
    }

    @FunctionalInterface
    private interface ArithmeticOperation {
        double apply(double a, double b);
    }
}