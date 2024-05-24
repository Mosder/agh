#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>

#define TAB_SIZE  1000
#define BUF_SIZE  1000

int get(int cols, int row, int col, const int *A) {
	return *(A + row*cols + col);
}

void set(int cols, int row, int col, int *A, int value) {
	*(A + row*cols + col) = value;
}

void prod_mat(int rowsA, int colsA, int colsB, int *A, int *B, int *AB) {
	for (int row = 0; row < rowsA; row++) {
		for (int col = 0; col < colsB; col++) {
			int val = 0;
			for (int i = 0; i < colsA; i++)
				val += get(colsA, row, i, A) * get(colsB, i, col, B);
			set(colsB, row, col, AB, val);
		}
	}
}

void read_mat(int rows, int cols, int *t) {
	for (int row = 0; row < rows; row++)
		for (int col = 0; col < cols; col++)
			scanf("%d", t + row*cols + col);
}

void print_mat(int rows, int cols, int *t) {
	for (int row = 0; row < rows; row++) {
		for (int col = 0; col < cols; col++)
			printf("%d ", *(t + row*cols + col));
		printf("\n");
	}
}

int read_char_lines(char *array[]) {
	char *buffer;
	buffer = malloc(sizeof(char) * BUF_SIZE);
	int linesAmount = 0;
	for (buffer = fgets(buffer, BUF_SIZE, stdin); buffer != NULL; buffer = fgets(buffer, BUF_SIZE, stdin)) {
		int lineLen = strlen(buffer);
		array[linesAmount] = malloc(sizeof(char) * lineLen);
		memcpy(array[linesAmount++], buffer, sizeof(char) * (lineLen+1));
	}
	array[linesAmount] = NULL;
	return linesAmount;
}

void write_char_line(char *array[], int n) {
	printf("%s", array[n]);
}

void delete_lines(char *array[]) {
	int i = 0;
	while (array[i] != NULL) free(array[i++]);
}

int* readLine(int *lineSize, int c) {
	int* line = malloc(0);
	int sign = 1;
	int number = 0;
	for (; c != '\n'; c = getc(stdin)) {
		if (c == '-') sign = -1;
		else if ('0' <= c && c <= '9') number = 10*number + (c-48);
		else if (c == ' ') {
			line = realloc(line, sizeof(int) * (*lineSize+1));
			line[(*lineSize)++] = sign * number;
			number = 0;
			sign = 1;
		}
	}
	line = realloc(line, sizeof(int) * (*lineSize+1));
	line[(*lineSize)++] = sign * number;
	return line;
}

int read_int_lines_cont(int *ptr_array[]) {
	int *matrix = malloc(0);
	int matrixSize = 0;
	int linesAmount = 0;
	for (int c = getc(stdin); c != EOF; c = getc(stdin)) {
		int lineSize = 0;
		int *line = readLine(&lineSize, c);
		matrix = realloc(matrix, sizeof(int) * (matrixSize+lineSize));
		memcpy(matrix+matrixSize, line, sizeof(int) * lineSize);
		free(line);
		ptr_array[linesAmount++] = matrix+matrixSize;
		matrixSize += lineSize;
	}
	return linesAmount;
}

void write_int_line_cont(int *ptr_array[], int n) {
	for (int *ptr = ptr_array[n]; ptr != ptr_array[n+1]; ptr++) printf("%d ", *ptr);
	printf("\n");
}

typedef struct {
	int *values;
	int len;
	double average;
} line_type;

int read_int_lines(line_type lines_array[]) {
	int linesAmount = 0;
	for (int c = getc(stdin); c != EOF; c = getc(stdin)) {
		int lineSize = 0;
		int *line = readLine(&lineSize, c);
		line_type newLine;
		newLine.len = lineSize;
		newLine.values = malloc(sizeof(int) * lineSize);
		memcpy(newLine.values, line, sizeof(int) * lineSize);
		int numSum = 0;
		for (int i = 0; i < lineSize; i++) numSum += *(line+i);
		newLine.average = numSum / lineSize;
		lines_array[linesAmount++] = newLine;
	}
	return linesAmount;
}

void write_int_line(line_type lines_array[], int n) {
	line_type line = lines_array[n];
	for (int i = 0; i < line.len; i++) printf("%d ", *(line.values + i));
	printf("\n%.2f\n", line.average);
}

void delete_int_lines(line_type array[], int line_count) {
	for (int i = 0; i < line_count; i++) free(array[i].values);
}

int cmp (const void *a, const void *b) {
	return ((line_type*)a)->average - ((line_type*)b)->average;
}

void sort_by_average(line_type lines_array[], int line_count) {
	qsort(lines_array, line_count, sizeof(line_type), cmp);
}

typedef struct {
	int r, c, v;
} triplet;

int read_sparse(triplet *triplet_array, int n_triplets) {
	for (int i = 0; i < n_triplets; i++)
		scanf("%d %d %d", &triplet_array[i].r, &triplet_array[i].c, &triplet_array[i].v);
	return n_triplets;
}

int cmp_triplets(const void *t1, const void *t2) {
	triplet *trip1 = (triplet*)t1;
	triplet *trip2 = (triplet*)t2;
	int result = trip1->r - trip2->r;
	if (result == 0) result = trip1->c - trip2->c;
	return result;
}

void make_CSR(triplet *triplet_array, int n_triplets, int rows, int *V, int *C, int *R) {
	qsort(triplet_array, n_triplets, sizeof(triplet), cmp_triplets);
	memset(R, 0, (rows+1)*sizeof(int));
	for (int i = 0; i < n_triplets; i++) {
		triplet trip = triplet_array[i];
		V[i] = trip.v;
		C[i] = trip.c;
		R[trip.r+1]++;
	}
	for (int i = 1; i < rows; i++) R[i+1] += R[i];
}

void multiply_by_vector(int rows, const int *V, const int *C, const int *R, const int *x, int *y) {
	for (int i = 0; i < rows; i++) {
		y[i] = 0;
		for (int j = R[i]; j <= R[i+1] - 1; j++) y[i] += V[j] * x[C[j]];
	}
}

void read_vector(int *v, int n) {
	for (int i = 0; i < n; i++) scanf("%d", v+i);
}

void write_vector(int *v, int n) {
	for (int i = 0; i < n; i++) printf("%d ", v[i]);
	printf("\n");
}

int read_int() {
	char c_buf[BUF_SIZE];
	fgets(c_buf, BUF_SIZE, stdin);
	return (int)strtol(c_buf, NULL, 10);
}

int main(void) {
	int to_do = read_int();

	int A[TAB_SIZE], B[TAB_SIZE], AB[TAB_SIZE];
	int n, lines_counter, rowsA, colsA, rowsB, colsB;
	int rows, cols, n_triplets;
	char *char_lines_array[TAB_SIZE] = { NULL };
	int continuous_array[TAB_SIZE];
	int *ptr_array[TAB_SIZE];
	triplet triplet_array[TAB_SIZE];
	int V[TAB_SIZE], C[TAB_SIZE], R[TAB_SIZE];
	int x[TAB_SIZE], y[TAB_SIZE];
	line_type int_lines_array[TAB_SIZE];

	switch (to_do) {
		case 1:
			scanf("%d %d", &rowsA, &colsA);
			read_mat(rowsA, colsA, A);
			scanf("%d %d", &rowsB, &colsB);
			read_mat(rowsB, colsB, B);
			prod_mat(rowsA, colsA, colsB, A, B, AB);
			print_mat(rowsA, colsB, AB);
			break;
		case 2:
			n = read_int() - 1; // we count from 1 :)
			ptr_array[0] = continuous_array;
			read_int_lines_cont(ptr_array);
			write_int_line_cont(ptr_array, n);
			break;
		case 3:
			n = read_int() - 1;
			read_char_lines(char_lines_array);
			write_char_line(char_lines_array, n);
			delete_lines(char_lines_array);
			break;
		case 4:
			n = read_int() - 1;
			lines_counter = read_int_lines(int_lines_array);
			sort_by_average(int_lines_array, lines_counter);
			write_int_line(int_lines_array, n);
			delete_int_lines(int_lines_array, lines_counter);
			break;
		case 5:
			scanf("%d %d %d", &rows, &cols, &n_triplets);
			n_triplets = read_sparse(triplet_array, n_triplets);
			read_vector(x, cols);
			make_CSR(triplet_array, n_triplets, rows, V, C, R);
			multiply_by_vector(rows, V, C, R, x, y);
			write_vector(V, n_triplets);
			write_vector(C, n_triplets);
			write_vector(R, rows + 1);
			write_vector(y, rows);
			break;
		default:
			printf("NOTHING TO DO FOR %d\n", to_do);
			break;
	}
	return 0;
}
