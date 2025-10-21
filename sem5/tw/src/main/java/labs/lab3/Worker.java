package labs.lab3;

public class Worker extends Thread {
    PrinterMonitor printerMonitor;

    public Worker(PrinterMonitor printerMonitor) {
        this.printerMonitor = printerMonitor;
    }

    public void run() {
        while (true) {
            try {
                int printerId = printerMonitor.reservePrinter();
                Printer.print(printerId, "some message");
                printerMonitor.releasePrinter(printerId);
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }

        }
    }
}
