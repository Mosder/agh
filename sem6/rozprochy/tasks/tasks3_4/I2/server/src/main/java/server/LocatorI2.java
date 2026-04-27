package server;

import com.zeroc.Ice.Current;
import com.zeroc.Ice.ServantLocator;
import com.zeroc.Ice.UserException;

public class LocatorI2 implements ServantLocator {
    @Override
    public LocateResult locate(Current current) {
        // locator will only be run once, because we are adding servant to ASM here
        // we need to use unchecked cast in client to avoid creating the servant too early
        DedicatedItem servant = new DedicatedItem();
        System.out.println("Dedicated servant not found for " + current.id.name + " - new one created");
        current.adapter.add(servant, current.id);
        return new ServantLocator.LocateResult(servant, null);
    }

    @Override
    public void finished(Current current, com.zeroc.Ice.Object object, Object o) {

    }

    @Override
    public void deactivate(String s) {

    }
}
