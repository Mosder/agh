package labs.lab3;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class WaiterMonitor {
    private final Lock lock;
    private final Condition tableCondition;
    private final Condition[] pairConditions;
    private final int[] pairs;
    private int tableOccupied;

    public WaiterMonitor(int pairCount) {
        lock = new ReentrantLock();
        tableCondition = lock.newCondition();
        tableOccupied = 0;
        pairs = new int[pairCount];
        pairConditions = new Condition[pairCount];
        for (int i = 0; i < pairCount; i++) {
            pairs[i] = 0;
            pairConditions[i] = lock.newCondition();
        }
    }

    private boolean isTableFree() {
        return tableOccupied == 0;
    }

    public void reserve(int pairNumber) throws InterruptedException {
        lock.lock();
        try {
            pairs[pairNumber]++;
            if (pairs[pairNumber] == 1) {
                while (pairs[pairNumber] < 2) {
                    pairConditions[pairNumber].await();
                }
                pairs[pairNumber] = 0;
            }
            else {
                while (!isTableFree()) {
                    tableCondition.await();
                }
                pairConditions[pairNumber].signal();
            }
            tableOccupied = 2;
        } finally {
            lock.unlock();
        }
    }

    public void release() throws InterruptedException {
        lock.lock();
        try {
            tableOccupied--;
            if (isTableFree()) {
                tableCondition.signal();
            }
        } finally {
            lock.unlock();
        }
    }}
