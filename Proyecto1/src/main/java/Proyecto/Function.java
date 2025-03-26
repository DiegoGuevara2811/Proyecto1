package Proyecto;

import java.util.List;

public class Function {
    private final String name;
    private final List<String> params;
    private final List<Object> body;

    public Function(String name, List<String> params, List<Object> body) {
        this.name = name;
        this.params = params;
        this.body = body;
    }

    public String getName() {
        return name;
    }

    public List<String> getParams() {
        return params;
    }

    public List<Object> getBody() {
        return body;
    }

    public Object call(List<Object> args, Environment env) {
        Environment localEnv = new Environment(env.getGlobal());

        if (args.size() != params.size()) {
            throw new RuntimeException("Número incorrecto de argumentos para " + name);
        }

        for (int i = 0; i < params.size(); i++) {
            localEnv.setVariable(params.get(i), args.get(i));
        }

        Evaluator evaluator = new Evaluator(localEnv);
        Object result = null;

        for (Object expr : body) {
            result = evaluator.evaluate((List<Object>) expr);
        }

        return result;
    }
}