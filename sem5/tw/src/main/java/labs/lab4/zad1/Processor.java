package labs.lab4.zad1;

// generalized class that can represent producer, processor and consument
public class Processor extends Thread {
    private final int startValue;
    private final int endValue;
    private final Buffer buffer;
    private final int delay;

    public Processor(Buffer buffer, int startValue, int endValue, int delay) {
        this.buffer = buffer;
        this.startValue = startValue;
        this.endValue = endValue;
        this.delay = delay;
    }

    public void run() {
        while (true) {
            for (int i = 0; i < buffer.getSize(); i++) {
                try {
                    buffer.set(i, startValue, endValue);
                    Thread.sleep(delay);
                } catch (InterruptedException e) {
                    System.out.println(e.getMessage());
                }
            }
        }
    }
}
