package Proyecto;

import java.util.*;
import java.util.function.BiFunction;

public class Evaluator {
    private final Environment env;
    private static final int MAX_RECURSION_DEPTH = 1000000000;
    private int recursionDepth = 0;

    public Evaluator(Environment env) {
        this.env = env;
    }

    public Object evaluate(List<Object> expression) {
        if (recursionDepth > MAX_RECURSION_DEPTH) {
            throw new RuntimeException("Profundidad de recursión máxima excedida");
        }

        recursionDepth++;
        try {
            if (expression.isEmpty()) return null;

            Object first = expression.get(0);
            if (!(first instanceof String)) return evalAtom(expression);

            String operator = ((String) first).toLowerCase();

            if (expression.size() == 1 && env.variableExists(operator)) {
                return env.getVariable(operator);
            }

            switch (operator) {
                case "print": return evalPrint(expression);
                case "+": return evalArithmetic(expression, 0.0, (a, b) -> a + b);
                case "-": return evalArithmetic(expression, 0.0, (a, b) -> a - b);
                case "*": return evalArithmetic(expression, 1.0, (a, b) -> a * b);
                case "/": return evalDivision(expression);
                case "mod": return evalModulo(expression);
                case "setq": return evalSetq(expression);
                case "defun": return evalDefun(expression);
                case "quote": return expression.size() > 1 ? expression.get(1) : null;
                case "'": return expression.size() > 1 ? expression.get(1) : null;
                case "=": return evalEquality(expression);
                case ">": return evalComparison(expression, (a, b) -> a > b);
                case "<": return evalComparison(expression, (a, b) -> a < b);
                case ">=": return evalComparison(expression, (a, b) -> a >= b);
                case "<=": return evalComparison(expression, (a, b) -> a <= b);
                case "equal": return evalEqual(expression);
                case "atom": return evalAtomPredicate(expression);
                case "listp": return evalListPredicate(expression);
                case "cond": return evalCond(expression);
                case "cons": return evalCons(expression);
                case "car": return evalCar(expression);
                case "cdr": return evalCdr(expression);
                default: return evalFunctionCall(expression);
            }
        } finally {
            recursionDepth--;
        }
    }

    private boolean evalEquality(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("Operación inválida. Se requieren 3 argumentos: operador, a, b");

        // Extraemos el operador
        String operator = (String) expression.get(0); // El operador como String (=)

        // Usamos evalAtom() para obtener los valores en los índices 1 y 2
        Object evaluatedA = evalAtom(expression.get(1));
        Object evaluatedB = evalAtom(expression.get(2));

        // Intentamos convertir los valores a Double
        double a = convertToDouble(evaluatedA);
        double b = convertToDouble(evaluatedB);

        // Comprobamos si el operador es "="
        if ("=".equals(operator)) {
            // Comparamos los dos operandos
            return a == b; // Devuelve true si son iguales, false si no lo son
        }

        throw new RuntimeException("Operador no soportado: " + operator);
    }

    // Función auxiliar para convertir a Double
    private double convertToDouble(Object obj) {
        try {
            if (obj instanceof Number) {
                return ((Number) obj).doubleValue();
            } else {
                throw new RuntimeException("Operando no numérico: " + obj);
            }
        } catch (Exception e) {
            throw new RuntimeException("Error al convertir operando a Double: " + obj);
        }
    }


    private Object evalAtom(Object expr) {
        if (expr instanceof List) {
            List<Object> list = (List<Object>) expr;
            //System.out.println("ESTA ES LA EXPRESION ---- "+expr);
            if (list.isEmpty()) {
                return Collections.emptyList();
            }
            // ¡Forzar evaluación recursiva!
            return evaluate(list);
        }

        if (expr instanceof String) {
            String str = (String) expr;
            if (str.startsWith("\"") && str.endsWith("\"")) {
                return str.substring(1, str.length() - 1);
            }
            if (str.equalsIgnoreCase("t")) return true;
            if (str.equalsIgnoreCase("nil")) return false;

            try {
                return Integer.parseInt(str);
            } catch (NumberFormatException e1) {
                try {
                    return Double.parseDouble(str);
                } catch (NumberFormatException e2) {
                    if (env.variableExists(str)) {
                        return env.getVariable(str);
                    }
                    return str;
                }
            }
        }

        return expr;  // Si no es lista ni string, devolver tal cual (números, booleanos)
    }


    private Object evalPrint(List<Object> expression) {
        if (expression.size() < 2) throw new RuntimeException("print requiere al menos un argumento");
        StringBuilder output = new StringBuilder();
        for (int i = 1; i < expression.size(); i++) {
            Object value = evalAtom(expression.get(i));
            if (i > 1) output.append(" ");
            output.append(value != null ? value.toString() : "nil");
        }
        System.out.println(output);
        return output.length() > 0 ? output.toString() : null;
    }

    private double evalModulo(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("Operación inválida. Se requieren 3 argumentos: operador, a, b");

        // Extraemos el operador
        String operator = (String) expression.get(0); // El operador como String (%)

        // Usamos evalAtom() para obtener los valores en los índices 1 y 2
        Object evaluatedA = evalAtom(expression.get(1));
        Object evaluatedB = evalAtom(expression.get(2));

        // Intentamos convertir los valores a Double
        double a = convertToDouble(evaluatedA);
        double b = convertToDouble(evaluatedB);

        // Comprobamos si el operador es "%"
        if ("mod".equals(operator)) {
            // Aplicamos la operación de módulo, similar a Python
            double result = a % b;
            // Si el resultado es negativo, lo ajustamos para que sea positivo como en Python
            return result < 0 ? result + Math.abs(b) : result;
        }

        throw new RuntimeException("Operador no soportado: " + operator);
    }

    private Number evalArithmetic(List<Object> expression, double initial, ArithmeticOperation op) {
        if (expression.size() < 2) throw new RuntimeException("Operación requiere al menos un argumento");
        double result = initial;
        boolean allIntegers = true;

        // Iterar sobre la lista de expresiones (empezando desde el segundo elemento)
        for (int i = 1; i < expression.size(); i++) {

                // Evaluar la subexpresión
                Object evaluated = evalAtom(expression.get(i));
                //System.out.println(evaluated + " " + result);
                // Verificar que sea un número
                if (!(evaluated instanceof Number)) {
                    throw new RuntimeException("Argumento no numérico: " + evaluated);
                }

                // Convertir el valor a tipo double
                Number num = (Number) evaluated;
                double currentValue = num.doubleValue();
            //System.out.println("currentValue: " + currentValue);

                // Comprobar si todos los números son enteros
                if (allIntegers && !isInteger(num)) allIntegers = false;
                // Si es el primer valor, asignar directamente
                if(i == 1){
                    result = currentValue;
                }
                else if (i == 1 && expression.size() == 2) {
                    result = currentValue;
                    //System.out.println("RESULTADO: " + result);
                } else {
                    result = op.apply(result, currentValue); // Aplicar la operación (suma, multiplicación, etc.)
                    //System.out.println("RESULTADO: " + result);
                }
        }

        // Si todos los valores son enteros, devuelve un entero; de lo contrario, devuelve un número decimal
        return allIntegers && result == (int) result ? (int) result : result;
    }




    private Number evalDivision(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("División requiere exactamente 2 argumentos");
        Object first = evalAtom(expression.get(1));
        Object second = evalAtom(expression.get(2));
        if (!(first instanceof Number) || !(second instanceof Number)) {
            throw new RuntimeException("División requiere argumentos numéricos");
        }
        double denominator = ((Number) second).doubleValue();
        if (denominator == 0) throw new RuntimeException("División por cero");
        double result = ((Number) first).doubleValue() / denominator;
        return isInteger((Number)first) && isInteger((Number)second) && result == (int)result ? (int)result : result;
    }

    private Object evalSetq(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("setq requiere exactamente 2 argumentos");
        if (!(expression.get(1) instanceof String)) throw new RuntimeException("setq requiere nombre de variable");
        String varName = (String) expression.get(1);
        Object value = evalAtom(expression.get(2));
        env.setVariable(varName, value);
        return value;
    }

    private Object evalDefun(List<Object> expression) {
        if (expression.size() < 4) throw new RuntimeException("defun requiere nombre, parámetros y cuerpo");
        String name = (String) expression.get(1);
        List<String> params = new ArrayList<>();
        for (Object param : (List<?>) expression.get(2)) params.add((String) param);
        List<Object> body = expression.subList(3, expression.size());
        env.getGlobal().defineFunction(name, new Function(name, params, body));
        return name;
    }

    private Boolean evalComparison(List<Object> expression, BiFunction<Double, Double, Boolean> comparator) {
        if (expression.size() != 3) throw new RuntimeException("Comparación requiere exactamente 2 argumentos");
        Object first = evalAtom(expression.get(1));
        Object second = evalAtom(expression.get(2));
        if (!(first instanceof Number) || !(second instanceof Number)) {
            throw new RuntimeException("Comparación requiere argumentos numéricos");
        }
        return comparator.apply(((Number)first).doubleValue(), ((Number)second).doubleValue());
    }

    private Boolean evalEqual(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("equal requiere exactamente 2 argumentos");
        Object first = evalAtom(expression.get(1));
        Object second = evalAtom(expression.get(2));
        if (first == null && second == null) return true;
        if (first == null || second == null) return false;
        return first.equals(second);
    }

    private Boolean evalAtomPredicate(List<Object> expression) {
        if (expression.size() != 2) throw new RuntimeException("atom requiere exactamente un argumento");
        return !(evalAtom(expression.get(1)) instanceof List);
    }

    private Boolean evalListPredicate(List<Object> expression) {
        if (expression.size() != 2) throw new RuntimeException("listp requiere exactamente un argumento");
        return evalAtom(expression.get(1)) instanceof List;
    }

    private Object evalCond(List<Object> expression) {
        if (expression.size() < 2) throw new RuntimeException("cond requiere al menos una cláusula");
        for (int i = 1; i < expression.size(); i++) {
            if (!(expression.get(i) instanceof List)) throw new RuntimeException("Cláusula debe ser lista");
            List<?> clause = (List<?>) expression.get(i);
            if (clause.isEmpty()) throw new RuntimeException("Cláusula vacía en cond");
            Object condition = clause.get(0);
            boolean condValue;
            if (condition instanceof String && ((String)condition).equalsIgnoreCase("t")) condValue = true;
            else {
                condition = evalAtom(condition);
                condValue = condition instanceof Boolean ? (Boolean) condition : condition != null;
            }
            if (condValue) {
                if (clause.size() == 1) return condition;
                Object result = null;
                for (int j = 1; j < clause.size(); j++) result = evalAtom(clause.get(j));
                return result;
            }
        }
        return null;
    }

    private Object evalCons(List<Object> expression) {
        if (expression.size() != 3) throw new RuntimeException("cons requiere exactamente 2 argumentos");
        Object first = evalAtom(expression.get(1));
        Object second = evalAtom(expression.get(2));

        List<Object> result = new ArrayList<>();
        result.add(first);

        if (second instanceof List) {
            result.addAll((List<?>) second);
        } else if (!second.equals(false)) {  // 'nil' se representa como false en tu implementación
            result.add(second);
        }

        return result;
    }
    private Object evalCar(List<Object> expression) {
        if (expression.size() != 2) throw new RuntimeException("car requiere exactamente un argumento");
        Object evaluated = evalAtom(expression.get(1));
        if (!(evaluated instanceof List)) throw new RuntimeException("car requiere lista");
        List<?> list = (List<?>) evaluated;
        return list.isEmpty() ? null : list.get(0);
    }

    private List<Object> evalCdr(List<Object> expression) {
        if (expression.size() != 2) throw new RuntimeException("cdr requiere exactamente un argumento");
        Object evaluated = evalAtom(expression.get(1));

        if (!(evaluated instanceof List)) throw new RuntimeException("cdr requiere lista");

        List<?> list = (List<?>) evaluated;
        return list.isEmpty() ? Collections.emptyList() : new ArrayList<>(list.subList(1, list.size()));
    }

    private boolean isInteger(Number num) {
        if (num instanceof Integer) return true;
        if (num instanceof Double) {
            double d = num.doubleValue();
            return d == (int)d;
        }
        return false;
    }
    private Object evalFunctionCall(List<Object> expression) {
        if (expression.isEmpty()) {
            throw new RuntimeException("Llamada a función vacía");
        }

        String functionName = (String) expression.get(0);

        // 1. Buscar la función en el entorno
        if (env.functionExists(functionName)) {
            Function function = env.getFunction(functionName);

            // 2. Evaluar todos los argumentos
            List<Object> args = new ArrayList<>();
            for (int i = 1; i < expression.size(); i++) {
                args.add(evalAtom(expression.get(i)));
            }

            // 3. Verificar que el número de argumentos coincida
            if (args.size() != function.getParams().size()) {
                throw new RuntimeException(
                        String.format("Función '%s' espera %d argumentos pero recibió %d",
                                functionName, function.getParams().size(), args.size()));
            }

            // 4. Crear nuevo entorno local con el global como padre
            Environment localEnv = new Environment(env.getGlobal());

            // 5. Asignar los parámetros
            for (int i = 0; i < function.getParams().size(); i++) {
                localEnv.setVariable(function.getParams().get(i), args.get(i));
            }

            // 6. Evaluar el cuerpo de la función
            Evaluator localEvaluator = new Evaluator(localEnv);
            Object result = null;
            for (Object expr : function.getBody()) {
                if (!(expr instanceof List)) {
                    throw new RuntimeException("Cuerpo de función debe contener listas evaluables");
                }
                result = localEvaluator.evaluate((List<Object>) expr);
            }
            return result;
        }

        throw new RuntimeException("Función no definida: " + functionName);
    }

    @FunctionalInterface
    private interface ArithmeticOperation {
        double apply(double a, double b);
    }
}