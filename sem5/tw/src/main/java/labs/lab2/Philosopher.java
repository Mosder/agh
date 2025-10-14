package labs.lab2;

import java.time.Duration;

public class Philosopher extends Thread {
    private final int id;
    private final SemaphoreBinary leftFork;
    private final SemaphoreBinary rightFork;
    private final boolean leftFirst;
    private final SemaphoreGeneral waiter;

    public Philosopher(int id, SemaphoreBinary leftFork, SemaphoreBinary rightFork,
                       boolean leftFirst, SemaphoreGeneral waiter) {
        this.id = id;
        this.leftFork = leftFork;
        this.rightFork = rightFork;
        this.leftFirst = leftFirst;
        this.waiter = waiter;
    }

    private void takeLeft() {
        leftFork.acquire();
        System.out.println("Philosopher " + id + " took left fork");
    }

    private void takeRight() {
        rightFork.acquire();
        System.out.println("Philosopher " + id + " took right fork");
    }

    private void eat() {
        try {
            sleep(Duration.ofMillis(900 + (int)(Math.random() * 200)));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println("Philosopher " + id + " ate");
    }

    private void putDownLeft() {
        leftFork.release();
        System.out.println("Philosopher " + id + " put down left fork");
    }

    private void putDownRight() {
        rightFork.release();
        System.out.println("Philosopher " + id + " put down right fork");
    }

    public void run() {
        try {
            sleep(Duration.ofMillis(900 + (int)(Math.random() * 200)));
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        while (true) {
            if (waiter != null) {
                waiter.acquire();
            }
            if (this.leftFirst) {
                takeLeft();
                takeRight();
            }
            else {
                takeRight();
                takeLeft();
            }
            eat();
            if (this.leftFirst) {
                putDownLeft();
                putDownRight();
            }
            else  {
                putDownRight();
                putDownLeft();
            }
            if (waiter != null) {
                waiter.release();
            }
        }
    }
}
