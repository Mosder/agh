package labs.lab1;

public class ProducerConsumer {
    private static final int MESSAGE_COUNT = 5;

    private static class Producer implements Runnable {
        private final Buffer buffer;

        public Producer(Buffer buffer) {
            this.buffer = buffer;
        }

        public void run() {
            for(int i = 0; i < ProducerConsumer.MESSAGE_COUNT; i++) {
                buffer.put("message " + i);
            }
        }
    }

    private static class Consumer implements Runnable {
        private final Buffer buffer;

        public Consumer(Buffer buffer) {
            this.buffer = buffer;
        }

        public void run() {
            for(int i = 0;  i < ProducerConsumer.MESSAGE_COUNT;   i++) {
                String message = buffer.take();
                System.out.println(message);
            }
        }
    }

    private static class Buffer {
        private String message;

        public Buffer() {
            message = null;
        }

        private boolean isEmpty() {
            return message == null;
        }

        public synchronized void put(String mess) {
            while (!isEmpty()) {
                try {
                    wait();
                }
                catch (InterruptedException e) {
                    System.out.println("Error: " + e.getMessage());
                }
            }
            message = mess;
            notifyAll();
        }

        public synchronized String take() {
            while (isEmpty()) {
                try {
                    wait();
                }
                catch (InterruptedException e) {
                    System.out.println("Error: " + e.getMessage());
                }
            }
            String temp = message;
            message = null;
            notifyAll();
            return temp;
        }
    }

    public static void main(String[] args) {
        Buffer buffer = new Buffer();
        Producer producer1 = new Producer(buffer);
        Producer producer2 = new Producer(buffer);
        Consumer consumer1 = new Consumer(buffer);
        Consumer consumer2 = new Consumer(buffer);
        Thread thread1 = new Thread(producer1);
        Thread thread2 = new Thread(producer2);
        Thread thread3 = new Thread(consumer1);
        Thread thread4 = new Thread(consumer2);
        thread1.start();
        thread2.start();
        thread3.start();
        thread4.start();
    }
}
