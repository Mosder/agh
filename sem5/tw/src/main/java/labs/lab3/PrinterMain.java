package labs.lab3;

public class PrinterMain {
    private static final int PRINTER_COUNT = 10;
    private static final int WORKER_COUNT = 50;

    public static void main(String[] args) throws InterruptedException {
        PrinterMonitor monitor = new PrinterMonitor(PRINTER_COUNT);
        Worker[] workers = new Worker[WORKER_COUNT];
        for  (int i = 0; i < WORKER_COUNT; i++) {
            workers[i] = new Worker(monitor);
        }
        for (Worker worker : workers) {
            worker.start();
        }
    }
}
