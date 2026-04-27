package server;

import com.zeroc.Ice.Current;
import gen.Demo.Item;

public class DedicatedItem implements Item {
    private String state;

    public DedicatedItem() {
        state = "stateless";
    }

    @Override
    public String getState(Current current) {
        System.out.printf("[Servant %s]: Got state for dedicated/%s - %s\n",
                this.hashCode(), current.id.name, state);
        return state;
    }

    @Override
    public void setState(String newState, Current current) {
        System.out.printf("[Servant %s]: Set state for dedicated/%s from %s to %s\n",
                this.hashCode(), current.id.name, state, newState);
        state = newState;
    }
}
