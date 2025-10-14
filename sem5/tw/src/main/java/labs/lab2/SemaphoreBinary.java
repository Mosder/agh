package labs.lab2;

public class SemaphoreBinary {
    private boolean isOpen;

    public SemaphoreBinary() {
        isOpen = true;
    }

    public synchronized void acquire() {
        while (!isOpen) {
            try {
                wait();
            } catch (InterruptedException e) {
                System.out.println("Error: " + e.getMessage());
            }
        }
        isOpen = false;
    }

    public synchronized void release() {
        isOpen = true;
        notifyAll();
    }
}
