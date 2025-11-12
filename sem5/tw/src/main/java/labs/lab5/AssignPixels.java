package labs.lab5;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Callable;

public class AssignPixels implements Callable {
    private final int start;
    private final int jump;

    public AssignPixels(int start, int jump) {
        this.start = start;
        this.jump = jump;
    }

    private int[] linearTo2D(int linearCoordinate) {
        int y = linearCoordinate / Mandelbrot.WINDOW_WIDTH;
        int x = linearCoordinate % Mandelbrot.WINDOW_WIDTH;
        return new int[]{x, y};
    }

    @Override
    public Object call() {
        double zx, zy, cx, cy;
        Set<Result> results = new HashSet<>();

        for (int linear = start; linear < Mandelbrot.WINDOW_WIDTH*Mandelbrot.WINDOW_HEIGHT; linear += jump) {
            int[] xy = linearTo2D(linear);

            zx = zy = 0;
            cx = (xy[0] - (double)Mandelbrot.WINDOW_WIDTH/2) / Mandelbrot.ZOOM;
            cy = (xy[1] - (double)Mandelbrot.WINDOW_HEIGHT/2) / Mandelbrot.ZOOM;

            int iter = Mandelbrot.MAX_ITER;
            while (zx * zx + zy * zy < 4 && iter > 0) {
                double tmp = zx * zx - zy * zy + cx;
                zy = 2.0 * zx * zy + cy;
                zx = tmp;
                iter--;
            }

            results.add(new Result(xy[0], xy[1], iter));
        }

        return results;
    }
}
