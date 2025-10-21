package labs.lab3;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class PrinterMonitor {
    private final Lock lock;
    private final Condition printerCondition;
    private final boolean[] isPrinterFree;
    private int freePrinters;

    public PrinterMonitor(int printerCount) {
        lock = new ReentrantLock();
        printerCondition = lock.newCondition();
        freePrinters = printerCount;
        isPrinterFree = new boolean[printerCount];
        for (int i = 0; i < printerCount; i++) {
            isPrinterFree[i] = true;
        }
    }

    private boolean isAnyPrinterFree() {
        return freePrinters > 0;
    }

    public int reservePrinter() throws InterruptedException {
        lock.lock();
        try {
            while (!isAnyPrinterFree()) {
                printerCondition.await();
            }
            int printerId = -1;
            for (int i = 0; i < isPrinterFree.length; i++) {
                if (isPrinterFree[i]) {
                    printerId = i;
                    isPrinterFree[i] = false;
                    freePrinters--;
                    break;
                }
            }
            return printerId;
        } finally {
            lock.unlock();
        }
    }

    public void releasePrinter(int printerNumber) throws InterruptedException {
        lock.lock();
        try {
            isPrinterFree[printerNumber] = true;
            freePrinters++;
            printerCondition.signal();
        } finally {
            lock.unlock();
        }
    }
}
