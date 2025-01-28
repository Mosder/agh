import numpy as np
from scipy.integrate import quad as quad_integral
import matplotlib.pyplot as plt

class Fem:
    def __init__(self, n):
        self.n = n
        self.h = 2 / (n+1)
    
    def e(self, i, x):
        if x <= self.h * (i-1) or x >= self.h * (i+1):
            return 0
        if x <= self.h * i:
            return (x / self.h) - i + 1
        return i + 1 - (x / self.h)
    
    def de_dx(self, i, x):
        if x <= self.h * (i-1) or x >= self.h * (i+1):
            return 0
        if x <= self.h * i:
            return 1 / self.h
        return -1 / self.h
    
    def integral(self, function, i, j):
        # limit integration to parts where the values are not 0
        start = self.h * (max(i, j) - 1)
        end = self.h * (min(i, j) + 1)

        return quad_integral(function, start, end)[0]

    def B(self, i, j):
        result = self.e(i, 2) * self.e(j, 2)
        result += self.integral(lambda x: self.de_dx(i, x) * self.de_dx(j, x), i, j)
        result -= self.integral(lambda x: self.e(i, x) * self.e(j, x), i, j)
        return result
    
    def L_wave(self, j):
        result = self.integral(lambda x: self.e(j, x) * np.sin(x), j, j)
        result -= 2 * self.e(j, 2)
        result += self.integral(lambda x: 2 * self.e(j, x), j, j)
        return result

    def B_matrix(self):
        B = np.zeros((self.n, self.n))
        for row in range(self.n):
            for col in range(self.n):
                B[row, col] = self.B(col + 1, row + 1)
        return B
    
    def L_matrix(self):
        L = np.zeros(self.n)
        for row in range(self.n):
            L[row] = self.L_wave(row + 1)
        return L
    
    def get_w(self):
        w_array = np.linalg.solve(self.B_matrix(), self.L_matrix())
        
        def w(x, w_array):
            result = 0
            for i, wi in enumerate(w_array):
                result += wi * self.e(i+1, x)
            return result
        
        return lambda x: w(x, w_array)
    
    def get_u(self):
        w = self.get_w()
        return lambda x: 2 + w(x)
    
if __name__ == "__main__":
    n = int(input("Wprowadź n: "))

    fem = Fem(n)
    u = fem.get_u()

    xs = [fem.h * i for i in range(n+2)]
    ys = [u(x) for x in xs]

    plt.plot(xs, ys)
    plt.title(f"Wibracje akustyczne warstwy materiału dla n = {n}")
    plt.xlabel("x")
    plt.ylabel("u(x)")
    plt.show()
