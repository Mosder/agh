package labs.lab2;

public class PhilosophersAsymmetric {
    public static void main(String[] args) throws InterruptedException {
        SemaphoreBinary fork1 = new SemaphoreBinary();
        SemaphoreBinary fork2 = new SemaphoreBinary();
        SemaphoreBinary fork3 = new SemaphoreBinary();
        SemaphoreBinary fork4 = new SemaphoreBinary();
        SemaphoreBinary fork5 = new SemaphoreBinary();
        Philosopher p1 = new Philosopher(1, fork1, fork2, true, null);
        Philosopher p2 = new Philosopher(2, fork2, fork3, false, null);
        Philosopher p3 = new Philosopher(3, fork3, fork4, true, null);
        Philosopher p4 = new Philosopher(4, fork4, fork5, false, null);
        Philosopher p5 = new Philosopher(5, fork5, fork1, true, null);
        p1.start();
        p2.start();
        p3.start();
        p4.start();
        p5.start();
    }
}
