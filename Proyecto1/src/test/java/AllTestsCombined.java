import Proyecto.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.List;

/**
 * Todas las pruebas unitarias combinadas en un solo archivo
 */
public class AllTestsCombined {

    // --------------------------------------------
    // Pruebas para Lexer
    // --------------------------------------------

    @Test
    void testLexerTokenizeSimpleExpression() {
        Lexer lexer = new Lexer("(+ 1 2)");
        List<String> tokens = lexer.tokenize();
        Assertions.assertAll(
                () -> Assertions.assertEquals(5, tokens.size()),
                () -> Assertions.assertEquals("(", tokens.get(0)),
                () -> Assertions.assertEquals("+", tokens.get(1)),
                () -> Assertions.assertEquals("1", tokens.get(2)),
                () -> Assertions.assertEquals("2", tokens.get(3)),
                () -> Assertions.assertEquals(")", tokens.get(4))
        );
    }

    @Test
    void testLexerTokenizeString() {
        Lexer lexer = new Lexer("\"hello world\"");
        List<String> tokens = lexer.tokenize();
        Assertions.assertEquals("\"hello world\"", tokens.get(0));
    }

    // --------------------------------------------
    // Pruebas para Parser
    // --------------------------------------------

    @Test
    void testParserSimpleExpression() {
        Parser parser = new Parser(List.of("(", "+", "1", "2", ")"));
        List<Object> parsed = parser.parse();
        Assertions.assertTrue(parsed.get(0) instanceof List);
        List<?> expr = (List<?>) parsed.get(0);
        Assertions.assertEquals("+", expr.get(0));
        Assertions.assertEquals(1, expr.get(1));
        Assertions.assertEquals(2, expr.get(2));
    }

    @Test
    void testParserAtom() {
        Parser parser = new Parser(List.of("42"));
        List<Object> parsed = parser.parse();
        Assertions.assertEquals(42, parsed.get(0));
    }

    // --------------------------------------------
    // Pruebas para Environment
    // --------------------------------------------

    @Test
    void testEnvironmentVariableScope() {
        Environment global = new Environment();
        Environment local = new Environment(global);

        global.setVariable("x", 10);
        Assertions.assertEquals(10, local.getVariable("x"));

        local.setVariable("y", 20);
        Assertions.assertThrows(RuntimeException.class, () -> global.getVariable("y"));
    }

    // --------------------------------------------
    // Pruebas para Function
    // --------------------------------------------

    @Test
    void testFunctionCall() {
        Environment env = new Environment();
        Function func = new Function("suma", List.of("a", "b"), List.of(List.of("+", "a", "b")));

        Object result = func.call(List.of(2, 3), env);
        Assertions.assertEquals(5, result);
    }

    // --------------------------------------------
    // Pruebas para Evaluator
    // --------------------------------------------

    private Evaluator evaluator;
    private Environment env;

    @BeforeEach
    void setUpEvaluator() {
        env = new Environment();
        evaluator = new Evaluator(env);
    }

    @Test
    void testEvaluateArithmetic() {
        Object result = evaluator.evaluate(List.of("+", 2, 3));
        Assertions.assertEquals(5, result);
    }

    @Test
    void testEvaluateSetq() {
        Object result = evaluator.evaluate(List.of("setq", "x", 10));
        Assertions.assertEquals(10, result);
        Assertions.assertEquals(10, env.getVariable("x"));
    }

    // --------------------------------------------
    // Pruebas de Integración
    // --------------------------------------------

    @Test
    void testFullIntegration() {
        String code = "(setq x 10) (setq y 20) (+ x y)";
        Lexer lexer = new Lexer(code);
        Parser parser = new Parser(lexer.tokenize());
        Evaluator evaluator = new Evaluator(new Environment());

        List<Object> parsed = parser.parse();
        Object result = null;
        for (Object expr : parsed) {
            result = evaluator.evaluate((List<Object>) expr);
        }

        Assertions.assertEquals(30, result);
    }
}