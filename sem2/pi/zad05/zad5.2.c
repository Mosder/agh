#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define SIZE 40

#define TEST 0    // 1 - dla testow,  0 - dla oceny automatycznej

void read_vec(double x[], size_t n) {
  for(size_t i=0; i<n; ++i)  scanf("%lf",x++);
}

void print_vec(const double x[], size_t n) {
  for(size_t i=0; i<n; ++i) printf("%.4f ",x[i]);
  printf("\n");
}

void read_mat(double A[][SIZE], size_t m, size_t n) {
  for(size_t i=0; i<m; ++i) {
    for(size_t j=0; j<n; ++j)  scanf("%lf",&A[i][j]);
  }
}

void print_mat(const double A[][SIZE], size_t m, size_t n) {
  for(size_t i=0; i<m; ++i) {
    for(size_t j=0; j<n; ++j)  printf("%.4f ",A[i][j]);
    printf("\n");
  }
}

int countInversions(int arr[], size_t n) {
  int inv = 0;
  for (int i = 0; i < n-1; i++) {
    for (int j = i+1; j < n; j++) {
      if (arr[i] > arr[j]) inv++;
    }
  }
  return inv;
}

void mySort(double A[][SIZE], int rowIndices[], size_t n, int start) {
  for (int i = 0; i < n - start - 1; i++) {
    for (int j = start; j < n - i - 1; j++) {
      if (fabs(A[rowIndices[j]][start]) < fabs(A[rowIndices[j+1]][start])) {
        int tmp = rowIndices[j];
        rowIndices[j] = rowIndices[j+1];
        rowIndices[j+1] = tmp;
      }
    }
  }
}

// 5.2.1 Triangularyzacja, wyznacznik i rozwiazanie Ax=b dla  macierzy kwadratowej.
// Wersja rozszerzona: Wybor wiersza z maksymalna waroscia |elementu glownego|
// Przy wymianie wierszy nalezy zastosowac wetor permutacji indeksow wierszy.
// Jezeli maksymalna wartosc |elementu glownego| < eps, to wyznacznik = 0.
// Zwraca wyznacznik det. Jezeli =0,  to triangularyzacja moze byc niedokonczona.
// Jezeli wyznacznik != 0, i b,x != NULL,
// to w wektorze x umieszcza rozwiazanie ukladu rownan Ax=b.

// Usunąłem const z b, bo się zmienia w rozwiązywaniu równań metodą gaussa
double gauss(double A[][SIZE], double b[], double x[], size_t n, double eps) {
  int rowIndices[n];
  for (int i = 0; i < n; i++) rowIndices[i] = i;
  for (int i = 0; i < n; i++) {
    mySort(A, rowIndices, n, i);
    if (fabs(A[rowIndices[i]][i]) < eps) return 0;
    for (int row = i+1; row < n; row++) {
      double scalar = A[rowIndices[row]][i] / A[rowIndices[i]][i];
      if (b != NULL) b[rowIndices[row]] -= scalar * b[rowIndices[i]];
      for (int col = i; col < n; col++) A[rowIndices[row]][col] -= scalar * A[rowIndices[i]][col];
    }
  }
  double det = countInversions(rowIndices, n) % 2 == 0 ? 1 : -1;
  for (int i = 0; i < n; i++) det *= A[rowIndices[i]][i];
  if (b != NULL && x != NULL) {
    for (int i = n-1; i >= 0; i--) {
      double result = b[rowIndices[i]];
      for (int j = n-1; j > i; j--) result -= A[rowIndices[i]][j] * x[j];
      x[i] = result / A[rowIndices[i]][i];
    }
  }
  return det;
}

void makeIntoIdentity(double A[][SIZE], size_t n) {
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      A[i][j] = 0;
      if (i == j) A[i][j] = 1;
    }
  }
}

// 5.2.2
// Zwraca wyznacznik i w tablicy B macierz odwrotna (jezlei wyznacznik != 0)
// Jezeli maksymalna bezwzgledna wartosc elementu glownego < eps,
// to funkcja zwraca wartosc wyznacznika det = 0.
// Funkcja zmienia wartosci takze w tablicy A.

double matrix_inv(double A[][SIZE], double B[][SIZE], size_t n, double eps) {
  double tempB[SIZE][SIZE];
  makeIntoIdentity(tempB, n);
  int rowIndices[n];
  for (int i = 0; i < n; i++) rowIndices[i] = i;
  double det = 1;
  for (int i = 0; i < n; i++) {
    mySort(A, rowIndices, n, i);
    double scalar = A[rowIndices[i]][i];
    if (fabs(scalar) < eps) return 0;
    det *= scalar;
    for (int j = 0; j < n; j++) {
      A[rowIndices[i]][j] /= scalar;
      tempB[rowIndices[i]][j] /= scalar;
    }
    for (int row = i+1; row < n; row++) {
      scalar = A[rowIndices[row]][i];
      for (int col = 0; col < n; col++) {
        A[rowIndices[row]][col] -= scalar * A[rowIndices[i]][col];
        tempB[rowIndices[row]][col] -= scalar * tempB[rowIndices[i]][col];
      }
    }
  }
  for (int i = n-1; i > 0; i--) {
    for (int row = i-1; row >= 0; row--) {
      double scalar = A[rowIndices[row]][i];
      for (int col = 0; col < n; col++) {
        A[rowIndices[row]][col] -= scalar * A[rowIndices[i]][col];
        tempB[rowIndices[row]][col] -= scalar * tempB[rowIndices[i]][col];
      }
    }
  }
  for (int row = 0; row < n; row++)
    for (int col = 0; col < n; col++)
      B[row][col] = tempB[rowIndices[row]][col];
  if (countInversions(rowIndices, n) % 2 == 1) det *= -1;
  return det;
}

int main(void) {

    double A[SIZE][SIZE], B[SIZE][SIZE], C[SIZE][SIZE];
    double b[SIZE], x[SIZE], det, eps = 1.e-13;
	int to_do;
	size_t m,n,p,q;
	if(TEST) printf("Wpisz numer testu ");
	scanf ("%d", &to_do);
	switch (to_do) {
    case 4:
      if(TEST) printf("Wpisz liczbe wierszy i kolumn mac. kwadratowej: ");
      scanf("%d", &n);
      if(TEST) printf("Wpisz macierz A (wierszami): ");
      read_mat(A,n,n);
      if(TEST) printf("Wpisz wektor b: ");
      read_vec(b,n);
      det = gauss(A, b, x, n, eps);
      printf("%.4f\n",det);
      if(det) print_vec(x,n);
      break;
    case 5:
      if(TEST) printf("Wpisz rozmiar macierzy n = ");
      scanf("%d",&n);
      if(TEST) printf("Wpisz elementy macierzy (wierszami): ");
      read_mat(A,n,n);
      det = matrix_inv(A,B,n,eps);
      printf("%.4f\n",det);
      if(det) print_mat(B,n,n);
      break;
    default:
      printf("NOTHING TO DO FOR %d\n", to_do);
	}
	return 0;
}
