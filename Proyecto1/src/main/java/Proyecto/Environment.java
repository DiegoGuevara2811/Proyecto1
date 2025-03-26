package Proyecto;

import java.util.HashMap;
import java.util.Map;

public class Environment {
    private final Map<String, Object> variables;
    private final Map<String, Function> functions;
    private final Environment parent;

    public Environment() {
        this(null);
    }

    public Environment(Environment parent) {
        this.variables = new HashMap<>();
        this.functions = new HashMap<>();
        this.parent = parent;
    }

    public void setVariable(String name, Object value) {
        variables.put(name, value);
    }

    public Object getVariable(String name) {
        if (variables.containsKey(name)) {
            return variables.get(name);
        } else if (parent != null) {
            return parent.getVariable(name);
        }
        throw new RuntimeException("Variable no definida: " + name);
    }

    public boolean variableExists(String name) {
        if (variables.containsKey(name)) {
            return true;
        } else if (parent != null) {
            return parent.variableExists(name);
        }
        return false;
    }

    public void defineFunction(String name, Function function) {
        functions.put(name, function);
    }

    public Function getFunction(String name) {
        if (functions.containsKey(name)) {
            return functions.get(name);
        } else if (parent != null) {
            return parent.getFunction(name);
        }
        throw new RuntimeException("Función no definida: " + name);
    }

    public boolean functionExists(String name) {
        if (functions.containsKey(name)) {
            return true;
        } else if (parent != null) {
            return parent.functionExists(name);
        }
        return false;
    }
}