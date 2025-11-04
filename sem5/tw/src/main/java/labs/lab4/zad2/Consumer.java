package labs.lab4.zad2;

public class Consumer extends Thread {
    private final int id;
    private final Buffer buffer;
    private final int maxConsumption;

    public Consumer(int id, Buffer buffer, int maxConsumption) {
        this.id = id;
        this.buffer = buffer;
        this.maxConsumption = maxConsumption;
    }

    public void run() {
        while (true) {
            try {
                int consumption = (int) (Math.random() * maxConsumption) + 1;
                buffer.consume(id, consumption);
                Thread.sleep((int)(Math.random() * 1000));
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
