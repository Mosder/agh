package labs.lab4.zad2;

public class Producer extends Thread {
    private final int id;
    private final Buffer buffer;
    private final int maxProduce;

    public Producer(int id, Buffer buffer, int maxProduce) {
        this.id = id;
        this.buffer = buffer;
        this.maxProduce = maxProduce;
    }

    public void run() {
        while (true) {
            try {
                int produce = (int) (Math.random() * maxProduce) + 1;
                buffer.produce(id, produce);
                Thread.sleep((int)(Math.random() * 1000));
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
