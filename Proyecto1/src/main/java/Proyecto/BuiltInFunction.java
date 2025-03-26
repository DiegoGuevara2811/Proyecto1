package Proyecto;

import java.util.List;
import java.util.function.Function;

public class BuiltInFunction extends Function {
    private final Function<List<Object>, Object> implementation;

    public BuiltInFunction(String name, Function<List<Object>, Object> implementation) {
        super(name, null, null);
        this.implementation = implementation;
    }

    @Override
    public Object call(List<Object> arguments, Environment outerEnv) {
        return implementation.apply(arguments);
    }
}
