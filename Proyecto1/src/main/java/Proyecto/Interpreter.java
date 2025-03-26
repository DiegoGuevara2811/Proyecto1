package Proyecto;

import java.util.Collections;
import java.util.List;

public class Interpreter {
    private final Environment globalEnv;

    public Interpreter() {
        this.globalEnv = new Environment();
        setupGlobalEnvironment();
    }

    public Object interpret(String code) {
        try {
            Lexer lexer = new Lexer(code);
            List<String> tokens = lexer.tokenize();
            if (tokens.isEmpty()) return null;

            Parser parser = new Parser(tokens);
            List<Object> parsed = parser.parse();

            Evaluator evaluator = new Evaluator(globalEnv);
            Object result = null;
            for (Object expr : parsed) {
                result = expr instanceof List ?
                        evaluator.evaluate((List<Object>) expr) :
                        evaluator.evaluate(Collections.singletonList(expr));
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException("Error al interpretar: " + e.getMessage(), e);
        }
    }

    private void setupGlobalEnvironment() {
        globalEnv.setVariable("t", true);
        globalEnv.setVariable("nil", false);
    }
}