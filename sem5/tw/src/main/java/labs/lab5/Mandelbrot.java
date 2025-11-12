package labs.lab5;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.*;
import javax.swing.JFrame;

public class Mandelbrot extends JFrame {
    public final static int MONITOR_WIDTH = 1920;
    public final static int MONITOR_HEIGHT = 1080;
    public final static int WINDOW_WIDTH = 800;
    public final static int WINDOW_HEIGHT = 600;
    public final static int MAX_ITER = 570;
    public final static double ZOOM = 150;

    private final static int CPU_CORES = 6;
    private final static int THREAD_COUNT = 4*CPU_CORES;
    private final static TaskCount taskCount = TaskCount.EVERY_PIXEL;
    private static BufferedImage image;

    public Mandelbrot() throws ExecutionException, InterruptedException {
        super("Mandelbrot Set");
        setBounds((MONITOR_WIDTH-WINDOW_WIDTH)/2, (MONITOR_HEIGHT-WINDOW_HEIGHT)/2, WINDOW_WIDTH, WINDOW_HEIGHT);
        setResizable(false);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        image = new BufferedImage(WINDOW_WIDTH, WINDOW_HEIGHT, BufferedImage.TYPE_INT_RGB);

        ExecutorService pool = Executors.newFixedThreadPool(THREAD_COUNT);
        Set<Future<Set<Result>>> futureSet = new HashSet<>();

        int tasks = switch(taskCount) {
            case THREADS -> THREAD_COUNT;
            case THREADS_X10 -> THREAD_COUNT*10;
            case EVERY_PIXEL -> WINDOW_WIDTH*WINDOW_HEIGHT;
        };
        for (int i = 0; i < tasks; i++) {
            Callable<Set<Result>> assignTask = new AssignPixels(i, tasks);
            Future<Set<Result>> future = pool.submit(assignTask);
            futureSet.add(future);
        }

        for (Future<Set<Result>> future : futureSet) {
            Set<Result> resultSet = future.get();
            for (Result result : resultSet) {
                image.setRGB(result.x(), result.y(), result.iter() | (result.iter() << 8));
            }
        }
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(image, 0, 0, this);
    }

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        new Mandelbrot().setVisible(true);
    }
}
