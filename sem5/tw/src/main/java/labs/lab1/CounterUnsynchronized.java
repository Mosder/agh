package labs.lab1;

public class CounterUnsynchronized {
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

        public Incrementer(Counter counter) {
            this.counter = counter;
        }

        public void run() {
            for (int i = 0; i < CounterUnsynchronized.OPERATION_COUNT; i++) {
                counter.increment();
            }
        }
    }

    private static class Decrementer extends Thread {
        private final Counter counter;

        public Decrementer(Counter counter) {
            this.counter = counter;
        }

        public void run() {
            for (int i = 0; i < CounterUnsynchronized.OPERATION_COUNT; i++) {
                counter.decrement();
            }
        }
    }

    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter();
        Incrementer incrementer = new Incrementer(counter);
        Decrementer decrementer = new Decrementer(counter);
        incrementer.start();
        decrementer.start();
        incrementer.join();
        decrementer.join();
        System.out.println("Counter: " + counter.getCounter());
    }
}
