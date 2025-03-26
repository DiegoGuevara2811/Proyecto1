package Proyecto;

import java.util.List;

public class Function {
    private final String name;
    private final List<String> parameters;
    private final List<Object> body;

    public Function(String name, List<String> parameters, List<Object> body) {
        this.name = name;
        this.parameters = parameters;
        this.body = body;
    }

    public Object call(List<Object> arguments, Environment outerEnv) {
        if (arguments.size() != parameters.size()) {
            throw new RuntimeException("Número incorrecto de argumentos para " + name +
                    ". Esperados: " + parameters.size() + ", recibidos: " + arguments.size());
        }

        Environment localEnv = new Environment(outerEnv);

        for (int i = 0; i < parameters.size(); i++) {
            localEnv.setVariable(parameters.get(i), arguments.get(i));
        }

        Evaluator evaluator = new Evaluator(localEnv);
        return evaluator.evaluate(body);
    }
}