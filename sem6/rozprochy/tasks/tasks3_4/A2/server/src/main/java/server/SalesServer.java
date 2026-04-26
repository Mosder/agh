package server;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import io.grpc.Server;
import io.grpc.ServerBuilder;

public class SalesServer {
    private final int PORT = 50051;
    private Server server;

    private void start() throws IOException
    {
        SalesService service = new SalesService();
        service.startGenerator();
        server = ServerBuilder.forPort(PORT)
                .addService(service)
                .keepAliveTime(20, TimeUnit.SECONDS)
                .keepAliveTimeout(10, TimeUnit.SECONDS)
                .permitKeepAliveWithoutCalls(true)
                .build()
                .start();

        System.out.println("server.Server started on port " + PORT);

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            service.shutdown();
            server.shutdown();
        }));
    }

    private void awaitTermination() throws InterruptedException {
        if (server != null) {
            server.awaitTermination();
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        final SalesServer salesServer = new SalesServer();
        salesServer.start();
        salesServer.awaitTermination();
    }
}