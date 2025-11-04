package labs.lab4.zad2;

public class Main {
    private final static int M = 50;
    private final static int PRODUCER_COUNT = 10;
    private final static int CONSUMER_COUNT = 10;

    public static void main(String[] args) {
        Buffer buffer = new Buffer(M);
        Producer[] producers = new Producer[PRODUCER_COUNT];
        Consumer[] consumers = new Consumer[CONSUMER_COUNT];
        for (int i = 0; i < PRODUCER_COUNT; i++) {
            producers[i] = new Producer(i+1, buffer, M);
        }
        for (int i = 0; i < CONSUMER_COUNT; i++) {
            consumers[i] = new Consumer(i+1, buffer, M);
        }
        for (int i = 0; i < PRODUCER_COUNT; i++) {
            producers[i].start();
        }
        for (int i = 0; i < CONSUMER_COUNT; i++) {
            consumers[i].start();
        }
    }
}
