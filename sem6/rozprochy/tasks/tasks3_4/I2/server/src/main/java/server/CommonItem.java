package server;

import com.zeroc.Ice.Current;
import gen.Demo.Item;

import java.util.HashMap;
import java.util.Map;

public class CommonItem implements Item {
    private final static Map<String, String> states = new HashMap<>();

    @Override
    public String getState(Current current) {
        String state = states.getOrDefault(current.id.name, "stateless");
        System.out.printf("[Servant %s]: Got state for common/%s - %s\n",
                this.hashCode(), current.id.name, state);
        return state;
    }

    @Override
    public void setState(String newState, Current current) {
        String state = states.getOrDefault(current.id.name, "stateless");
        System.out.printf("[Servant %s]: Set state for common/%s from %s to %s\n",
                this.hashCode(), current.id.name, state, newState);
        states.put(current.id.name, newState);
    }
}
