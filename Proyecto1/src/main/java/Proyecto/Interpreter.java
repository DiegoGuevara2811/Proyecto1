package Proyecto;

import java.util.List;

public class Interpreter {
    private final Environment globalEnv;

    public Interpreter() {
        this.globalEnv = new Environment();
        setupGlobalEnvironment();
    }

    public Object interpret(String code) {
        Lexer lexer = new Lexer(code);
        List<String> tokens = lexer.tokenize();
        Parser parser = new Parser(tokens);
        List<Object> parsed = parser.parse();
        Evaluator evaluator = new Evaluator(globalEnv);
        return evaluator.evaluate(parsed);
    }

    private void setupGlobalEnvironment() {
        // Funciones matemáticas
        globalEnv.defineFunction("+", new BuiltInFunction("+", args -> {
            double sum = 0;
            for (Object arg : args) {
                if (!(arg instanceof Number)) {
                    throw new RuntimeException("Argumento no numérico para +: " + arg);
                }
                sum += ((Number) arg).doubleValue();
            }
            return sum;
        }));

        globalEnv.defineFunction("-", new BuiltInFunction("-", args -> {
            if (args.isEmpty()) throw new RuntimeException("- requiere al menos un argumento");
            if (args.size() == 1) {
                return -((Number) args.get(0)).doubleValue();
            }
            double result = ((Number) args.get(0)).doubleValue();
            for (int i = 1; i < args.size(); i++) {
                result -= ((Number) args.get(i)).doubleValue();
            }
            return result;
        }));

        // Otras funciones predefinidas
        globalEnv.defineFunction("list", new BuiltInFunction("list", args -> args));

        globalEnv.defineFunction("car", new BuiltInFunction("car", args -> {
            if (args.size() != 1 || !(args.get(0) instanceof List)) {
                throw new RuntimeException("car requiere una lista como argumento");
            }
            List<?> list = (List<?>) args.get(0);
            if (list.isEmpty()) throw new RuntimeException("car de lista vacía");
            return list.get(0);
        }));

        // Más funciones predefinidas pueden agregarse aquí
    }
}
