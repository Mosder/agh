package labs.lab4.zad1;

import java.util.Arrays;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Buffer {
    private final int size;
    private final int[] buffer;
    private final Lock[] locks;
    private final Condition[][] conditions;
    private final Lock printLock;

    public Buffer(int size, int possibleValues) {
        this.size = size;
        buffer = new int[size];
        Arrays.fill(buffer, -1);
        locks = new Lock[size];
        conditions = new Condition[size][possibleValues];
        for (int i = 0; i < size; i++) {
            locks[i] = new ReentrantLock();
            for  (int j = 0; j < possibleValues; j++) {
                conditions[i][j] = locks[i].newCondition();
            }
        }
        printLock = new ReentrantLock();
    }

    public int getSize() {
        return size;
    }

    public void set(int index, int valueNeeded, int valueNext) throws InterruptedException {
        locks[index].lock();
        try {
            while (buffer[index] != valueNeeded) {
                conditions[index][valueNeeded+1].await();
            }
            buffer[index] = valueNext;
            printBuffer();
            conditions[index][valueNext+1].signal();
        } finally {
            locks[index].unlock();
        }
    }

    private void printBuffer() {
        printLock.lock();
        try {
            for (int i = 0; i < buffer.length; i++) {
                System.out.printf("%c", buffer[i] == -1 ? '-' : buffer[i] + '0');
            }
            System.out.print("\n");
        } finally {
            printLock.unlock();
        }
    }
}
