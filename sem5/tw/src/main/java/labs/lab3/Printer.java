package labs.lab3;

import java.time.Duration;

public class Printer {
    public static void print(int id, String message) throws InterruptedException {
        Thread.sleep(Duration.ofMillis((long)(Math.random() * 2500)));
        System.out.println(id + ": " + message);
    }
}
