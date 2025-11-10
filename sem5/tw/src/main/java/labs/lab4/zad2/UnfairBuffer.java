package labs.lab4.zad2;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class UnfairBuffer implements Buffer {
    private final int size;
    private int filledSpace;
    private final Lock lock;
    private final Condition produced;
    private final Condition consumed;
    private final boolean print;

    public UnfairBuffer(int halfSize) {
        this(halfSize, true);
    }

    public UnfairBuffer(int halfSize, boolean print) {
        size = 2*halfSize;
        filledSpace = 0;
        lock = new ReentrantLock();
        produced = lock.newCondition();
        consumed = lock.newCondition();
        this.print = print;
    }

    public boolean produce(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > (size - filledSpace)) {
                if (!consumed.await(10, TimeUnit.SECONDS))
                    return true;
            }
            filledSpace += count;
            if (print)
                System.out.printf("Producer %d produced %d, buffer - %d/%d\n", id, count, filledSpace, size);
            produced.signalAll();
        } finally {
            lock.unlock();
        }
        return true;
    }

    public boolean consume(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > filledSpace) {
                if (!produced.await(10, TimeUnit.SECONDS))
                    return false;
            }
            filledSpace -= count;
            if (print)
                System.out.printf("Consumer %d consumed %d, buffer - %d/%d\n", id, count, filledSpace, size);
            consumed.signalAll();
        } finally {
            lock.unlock();
        }
        return true;
    }
}
