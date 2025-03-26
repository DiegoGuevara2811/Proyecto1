package Proyecto;

import java.util.HashMap;
import java.util.Map;

public class Environment {
    private final Map<String, Object> variables = new HashMap<>();
    private final Map<String, Function> functions = new HashMap<>();
    private final Environment parent;

    public Environment() {
        this.parent = null;
    }

    public Environment(Environment parent) {
        this.parent = parent;
    }

    public void setVariable(String name, Object value) {
        variables.put(name, value);
    }

    public Object getVariable(String name) {
        if (variables.containsKey(name)) {
            return variables.get(name);
        }
        if (parent != null) {
            return parent.getVariable(name);
        }
        throw new RuntimeException("Variable no definida: " + name);
    }

    public boolean variableExists(String name) {
        return variables.containsKey(name) || (parent != null && parent.variableExists(name));
    }

    public void defineFunction(String name, Function function) {
        functions.put(name, function);
    }

    public Function getFunction(String name) {
        if (functions.containsKey(name)) {
            return functions.get(name);
        }
        if (parent != null) {
            return parent.getFunction(name);
        }
        throw new RuntimeException("Función no definida: " + name);
    }

    public boolean functionExists(String name) {
        return functions.containsKey(name) || (parent != null && parent.functionExists(name));
    }

    public Environment getGlobal() {
        return parent == null ? this : parent.getGlobal();
    }
}