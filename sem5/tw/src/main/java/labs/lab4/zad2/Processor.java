package labs.lab4.zad2;

public class Processor extends Thread {
    private final Type type;
    private final int id;
    private final Buffer buffer;
    private final int maxRequest;
    private final int minWait;
    private final int maxWait;

    public Processor(Type type, int id, Buffer buffer, int maxRequest, int minWait, int maxWait) {
        this.type = type;
        this.id = id;
        this.buffer = buffer;
        this.maxRequest = maxRequest;
        this.minWait = minWait;
        this.maxWait = maxWait;
    }

    public void run() {
        while (true) {
            try {
                int amount = (int) (Math.random() * maxRequest) + 1;
                switch (type) {
                    case PRODUCER -> buffer.produce(id, amount);
                    case CONSUMER -> buffer.consume(id, amount);
                }
                Thread.sleep((int)(minWait + (Math.random() * (maxWait - minWait + 1))));
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
