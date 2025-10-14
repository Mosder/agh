package labs.lab2;

public class SemaphoreGeneral {
    private int capacity;

    public SemaphoreGeneral(int capacity) {
        this.capacity = capacity;
    }

    public synchronized void acquire() {
        while (capacity <= 0) {
            try {
                wait();
            } catch (InterruptedException e) {
                System.out.println("Error: " + e.getMessage());
            }
        }
        capacity--;
    }

    public synchronized void release() {
        capacity++;
        notifyAll();
    }
}
