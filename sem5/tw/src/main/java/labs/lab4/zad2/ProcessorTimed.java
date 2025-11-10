package labs.lab4.zad2;

import java.io.FileWriter;
import java.io.IOException;

public class ProcessorTimed extends Thread {
    private final Type type;
    private final int id;
    private final Buffer buffer;
    private final int maxRequest;
    private final int testTime;
    private final FileWriter fw;

    public ProcessorTimed(Type type, int id, Buffer buffer, int maxRequest, int testTime, FileWriter fw) {
        this.type = type;
        this.id = id;
        this.buffer = buffer;
        this.maxRequest = maxRequest;
        this.testTime = testTime;
        this.fw = fw;
    }

    public void run() {
        long startTime = System.nanoTime();
        while ((System.nanoTime() - startTime)*1e-9 < testTime) {
            try {
                int amount = (int) (Math.random() * maxRequest) + 1;
                long t0 = System.nanoTime();
                boolean success = switch (type) {
                    case PRODUCER -> buffer.produce(id, amount);
                    case CONSUMER -> buffer.consume(id, amount);
                };
                if (success) {
                    long t1 = System.nanoTime();
                    long duration = t1 - t0;
                    String toSave = String.format("%s,%d,%d\n", type, amount, duration);
                    synchronized (fw) {
                        fw.write(toSave);
                    }
                }
            } catch (InterruptedException | IOException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
