package labs.lab4.zad1;

public class Main {
    private final static int BUFFER_SIZE = 100;
    private final static int PROCESSOR_COUNT = 5;

    public static void main(String[] args) {
        Buffer buffer = new Buffer(BUFFER_SIZE,  PROCESSOR_COUNT+2);
        Processor producent = new Processor(buffer, -1, 0, 100);
        Processor[]  processors = new Processor[PROCESSOR_COUNT];
        for (int i = 0; i < PROCESSOR_COUNT; i++) {
            processors[i] = new Processor(buffer, i, i+1, 100*(i+2));
        }
        Processor consumer = new Processor(buffer, PROCESSOR_COUNT, -1, 700);

        producent.start();
        for (int i = 0; i < PROCESSOR_COUNT; i++) {
            processors[i].start();
        }
        consumer.start();
    }
}
