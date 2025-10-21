package labs.lab3;

import java.time.Duration;

public class Person extends Thread {
    private final int pairNumber;
    private final WaiterMonitor waiterMonitor;

    public Person(int pairNumber, WaiterMonitor waiterMonitor) {
        this.pairNumber = pairNumber;
        this.waiterMonitor = waiterMonitor;
    }

    public void run() {
        while (true) {
            try {
                System.out.println("Person from pair " + pairNumber + " waiting for table");
                waiterMonitor.reserve(pairNumber);
                Thread.sleep(Duration.ofMillis((long)(Math.random() * 1000)));
                System.out.println("Person from pair " + pairNumber + " ate");
                waiterMonitor.release();
                Thread.sleep(Duration.ofMillis((long)(Math.random() * 3000)));
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }
}
