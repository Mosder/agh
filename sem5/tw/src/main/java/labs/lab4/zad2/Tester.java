package labs.lab4.zad2;

import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalTime;

public class Tester {
    private final static int[] MS = {1000, 10_000, 100_000};
    private final static int[] PROCESSORS = {10, 100, 1000};
    private final static int TEST_TIME = 30;
    private final static String[] BUFFER_NAMES = {"unfair", "fair"};

    public static void main(String[] args) throws InterruptedException {
        for (int i = 0; i < MS.length; i++) {
            int m =  MS[i];
            int procCount = PROCESSORS[i];
            Buffer[] buffers = {
                    new UnfairBuffer(m, false),
                    new FairBuffer(m, procCount, procCount, false)
            };
            for (int b = 0; b < buffers.length; b++) {
                Buffer buffer = buffers[b];
                String testString = String.format("m%dk_pk%d_%s", m / 1000, procCount, BUFFER_NAMES[b]);
                String filePath = String.format("./lab4/%s.csv", testString);
                System.out.println(LocalTime.now() + " - testing " + testString);
                try (FileWriter fw = new FileWriter(filePath)) {
                    ProcessorTimed[] processors = new ProcessorTimed[2 * procCount];
                    for (int j = 0; j < procCount; j++) {
                        processors[2 * j] = new ProcessorTimed(Type.PRODUCER, j, buffer, m, TEST_TIME, fw);
                        processors[2 * j + 1] = new ProcessorTimed(Type.CONSUMER, j, buffer, m, TEST_TIME, fw);
                    }
                    for (ProcessorTimed processor : processors) {
                        processor.start();
                    }
                    for (ProcessorTimed processor : processors) {
                        processor.join();
                    }
                } catch (IOException e) {
                    System.err.println("Error: " + e.getMessage());
                }
            }
        }
    }
}
