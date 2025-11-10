package labs.lab4.zad2;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class FairBuffer implements Buffer {
    private final int size;
    private int filledSpace;
    private final Lock lock;
    private final Condition[] produced;
    private final Condition[] consumed;
    private int nextProducer;
    private int nextConsumer;
    private final boolean print;

    public FairBuffer(int halfSize, int producerCount, int consumerCount) {
        this(halfSize, producerCount, consumerCount, true);
    }

    public FairBuffer(int halfSize, int producerCount, int consumerCount, boolean print) {
        size = 2*halfSize;
        filledSpace = 0;
        lock = new ReentrantLock();
        produced = new Condition[consumerCount];
        consumed = new Condition[producerCount];
        for (int i = 0; i < consumerCount; i++) {
            produced[i] = lock.newCondition();
        }
        for (int i = 0; i < producerCount; i++) {
            consumed[i] = lock.newCondition();
        }
        nextProducer = 0;
        nextConsumer = 0;
        this.print = print;
    }

    public boolean produce(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > (size - filledSpace)) {
                if (!consumed[id].await(10, TimeUnit.SECONDS))
                    return false;
            }
            filledSpace += count;
            if (++nextProducer == consumed.length)
                nextProducer = 0;
            if (print)
                System.out.printf("Producer %d produced %d, buffer - %d/%d\n", id, count, filledSpace, size);
            produced[nextConsumer].signal();
        } finally {
            lock.unlock();
        }
        return true;
    }

    public boolean consume(int id, int count) throws InterruptedException {
        lock.lock();
        try {
            while (count > filledSpace) {
                if (!produced[id].await(10, TimeUnit.SECONDS))
                    return false;
            }
            filledSpace -= count;
            if (++nextConsumer == produced.length)
                nextConsumer = 0;
            if (print)
                System.out.printf("Consumer %d consumed %d, buffer - %d/%d\n", id, count, filledSpace, size);
            consumed[nextProducer].signal();
        } finally {
            lock.unlock();
        }
        return true;
    }
}
