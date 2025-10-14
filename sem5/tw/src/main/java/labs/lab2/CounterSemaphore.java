package labs.lab2;

public class CounterSemaphore {
    private static final int OPERATION_COUNT = (int) 1e8;

    private static class Counter {
        private int counter;

        public Counter() {
            counter = 0;
        }

        public void increment() {
            counter++;
        }

        public void decrement() {
            counter--;
        }

        public int getCounter() {
            return counter;
        }
    }

    private static class Incrementer extends Thread {
        private final Counter counter;
        private final SemaphoreBinary semaphore;

        public Incrementer(Counter counter, SemaphoreBinary sem) {
            this.counter = counter;
            this.semaphore = sem;
        }

        public void run() {
            for (int i = 0; i < CounterSemaphore.OPERATION_COUNT; i++) {
                semaphore.acquire();
                counter.increment();
                semaphore.release();
            }
        }
    }

    private static class Decrementer extends Thread {
        private final Counter counter;
        private final SemaphoreBinary semaphore;

        public Decrementer(Counter counter, SemaphoreBinary sem) {
            this.counter = counter;
            this.semaphore = sem;
        }

        public void run() {
            for (int i = 0; i < CounterSemaphore.OPERATION_COUNT; i++) {
                semaphore.acquire();
                counter.decrement();
                semaphore.release();
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter();
        SemaphoreBinary sem =  new SemaphoreBinary();
        Incrementer incrementer = new Incrementer(counter, sem);
        Decrementer decrementer = new Decrementer(counter, sem);
        incrementer.start();
        decrementer.start();
        incrementer.join();
        decrementer.join();
        System.out.println("Counter: " + counter.getCounter());
    }
}
