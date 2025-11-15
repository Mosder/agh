package labs.lab5;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.*;
import javax.swing.JFrame;

public class Mandelbrot extends JFrame {
    public final static int MONITOR_WIDTH = 1920;
    public final static int MONITOR_HEIGHT = 1080;
    public final static int WINDOW_WIDTH = 800;
    public final static int WINDOW_HEIGHT = 600;
    public final static int MAX_ITER = 100_000;
    public final static double ZOOM = 150;
    private final static int CORE_COUNT = 6;
    private static BufferedImage image;
    private final static String FILE_PATH = "./lab5/results.csv";

    public Mandelbrot(int threadCount, TaskCount taskCount, boolean draw, FileWriter fw)
            throws ExecutionException, InterruptedException, IOException {
        super("Mandelbrot Set");
        if (draw) {
            setBounds((MONITOR_WIDTH - WINDOW_WIDTH) / 2, (MONITOR_HEIGHT - WINDOW_HEIGHT) / 2,
                    WINDOW_WIDTH, WINDOW_HEIGHT);
            setResizable(false);
            setDefaultCloseOperation(EXIT_ON_CLOSE);
            image = new BufferedImage(WINDOW_WIDTH, WINDOW_HEIGHT, BufferedImage.TYPE_INT_RGB);
        }

        ExecutorService pool = Executors.newFixedThreadPool(threadCount);
        Set<Future<Set<Result>>> futureSet = new HashSet<>();
        long t0 = -1;
        int tasks = switch(taskCount) {
            case THREADS -> threadCount;
            case THREADS_X10 -> threadCount*10;
            case EVERY_PIXEL -> WINDOW_WIDTH*WINDOW_HEIGHT;
        };

        if (fw != null) {
            System.out.printf("Timing: %d threads, %s tasks\n", threadCount, taskCount);
            t0 = System.nanoTime();
        }
        for (int i = 0; i < tasks; i++) {
            Callable<Set<Result>> assignTask = new AssignPixels(i, tasks);
            Future<Set<Result>> future = pool.submit(assignTask);
            futureSet.add(future);
        }
        if (fw != null) {
            pool.shutdown();
            if (pool.awaitTermination(1, TimeUnit.MINUTES)) {
                long elapsed = System.nanoTime() - t0;
                System.out.printf("%fs\n", elapsed*1e-9);
                String toSave = String.format("%d,%s,%d\n", threadCount, taskCount, elapsed);
                fw.write(toSave);
            }
        }

        if (draw) {
            for (Future<Set<Result>> future : futureSet) {
                Set<Result> resultSet = future.get();
                for (Result result : resultSet) {
                    image.setRGB(result.x(), result.y(), result.iter() | (result.iter() << 8));
                }
            }
        }
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(image, 0, 0, this);
    }

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        // first run takes longer for some reason, so run one additional
        int testCount = 11;
        int[] threadCounts = {1, CORE_COUNT, 4*CORE_COUNT};
        TaskCount[] taskCounts = {TaskCount.THREADS, TaskCount.THREADS_X10, TaskCount.EVERY_PIXEL};
        try (FileWriter fw = new FileWriter(FILE_PATH)) {
            for (int threadCount : threadCounts) {
                for (TaskCount taskCount : taskCounts) {
                    for (int i = 0; i < testCount; i++) {
                        new Mandelbrot(threadCount, taskCount, false, fw);
                    }
                }
            }
        }
        catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
}
