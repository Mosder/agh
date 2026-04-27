package server;

import com.zeroc.Ice.Communicator;
import com.zeroc.Ice.Identity;
import com.zeroc.Ice.ObjectAdapter;
import com.zeroc.Ice.Util;
import com.zeroc.Ice.InitializationException;

public class ServerI2 {
    public void start(String[] args) {
        int status = 0;
        try (Communicator communicator = Util.initialize(args)){
            ObjectAdapter adapter = null;
            try {
                adapter = communicator.createObjectAdapter("Adapter1");
            }
            catch(InitializationException e) {
                System.exit(1);
            }
            System.out.println("Created object adapter");

            // register locator for dedicated services
            LocatorI2 locator = new LocatorI2();
            adapter.addServantLocator(locator, "dedicated");
            System.out.println("Registered servant locator for dedicated servants");

            // add default servant for common objects
            CommonItem commonServant = new CommonItem();
            adapter.addDefaultServant(commonServant, "common");
            System.out.println("Created default servant for common objects");

            adapter.activate();
            System.out.println("Activated adapter");
            communicator.waitForShutdown();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            status = 1;
        }
        System.exit(status);
    }


    public static void main(String[] args) {
        ServerI2 server = new ServerI2();
        server.start(args);
    }
}
