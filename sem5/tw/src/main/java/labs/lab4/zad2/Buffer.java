package labs.lab4.zad2;

public interface Buffer {
    boolean produce(int id, int count) throws InterruptedException;
    boolean consume(int id, int count) throws InterruptedException;
}
