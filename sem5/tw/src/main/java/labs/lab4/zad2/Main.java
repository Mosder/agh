package labs.lab4.zad2;

public class Main {
    private final static int M = 50;
    private final static int PRODUCER_COUNT = 10;
    private final static int CONSUMER_COUNT = 10;
    private final static int MIN_WAIT = 0;
    private final static int MAX_WAIT = 1;
    private final static boolean FAIR = true;

    public static void main(String[] args) {
        Buffer buffer = FAIR ? new FairBuffer(M, PRODUCER_COUNT, CONSUMER_COUNT) : new UnfairBuffer(M);
        Processor[] producers = new Processor[PRODUCER_COUNT];
        Processor[] consumers = new Processor[CONSUMER_COUNT];
        for (int i = 0; i < PRODUCER_COUNT; i++) {
            producers[i] = new Processor(Type.PRODUCER, i, buffer, M, MIN_WAIT, MAX_WAIT);
        }
        for (int i = 0; i < CONSUMER_COUNT; i++) {
            consumers[i] = new Processor(Type.CONSUMER, i, buffer, M, MIN_WAIT, MAX_WAIT);
        }
        for (Processor producer : producers) {
            producer.start();
        }
        for (Processor consumer : consumers) {
            consumer.start();
        }
    }
}
