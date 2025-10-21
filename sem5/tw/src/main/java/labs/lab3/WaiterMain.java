package labs.lab3;

public class WaiterMain {
    private static final int PAIR_COUNT = 10;

    public static void main(String[] args) throws InterruptedException {
        WaiterMonitor monitor = new WaiterMonitor(10);
        Person[] persons = new Person[PAIR_COUNT*2];
        for (int i = 0; i < PAIR_COUNT; i++) {
            persons[2*i] = new Person(i, monitor);
            persons[2*i+1] = new Person(i, monitor);
        }
        for (Person person : persons) {
            person.start();
        }
    }
}
