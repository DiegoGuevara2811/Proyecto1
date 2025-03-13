package org.uvg.proyect;

import java.util.*;

/**
 * Environment: Maneja variables y funciones definidas en el intérprete.
 */
public class Environment {
    private Map<String, Object> variables;

    public Environment() {
        this.variables = new HashMap<>();
    }

    public void setVar(String name, Object value) {
        variables.put(name, value);
    }

    public Object getVar(String name) {
        if (!variables.containsKey(name)) {
            throw new RuntimeException("Variable no definida: " + name);
        }
        return variables.get(name);
    }
}

