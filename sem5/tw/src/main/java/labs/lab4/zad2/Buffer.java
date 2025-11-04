package labs.lab4.zad2;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Buffer {
    private final int size;
    private int filledSpace;
    private final Lock lock;
    private final Condition produced;
    private final Condition consumed;

    public Buffer(int halfSize) {
        size = 2*halfSize;
        filledSpace = 0;
        lock = new ReentrantLock();
        produced = lock.newCondition();
        consumed = lock.newCondition();
    }

    public void produce(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > (size - filledSpace)) {
                consumed.await();
            }
            filledSpace += count;
            System.out.printf("Producer %d produced %d, buffer - %d/%d\n", id, count, filledSpace, size);
            produced.signalAll();
        } finally {
            lock.unlock();
        }
    }

    public void consume(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > filledSpace) {
                produced.await();
            }
            filledSpace -= count;
            System.out.printf("Consumer %d consumed %d, buffer - %d/%d\n", id, count, filledSpace, size);
            consumed.signalAll();
        } finally {
            lock.unlock();
        }
    }
}
