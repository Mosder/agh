#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_REL_SIZE 100
#define MAX_RANGE 100

typedef struct {
	int first;
	int second;
} pair;

// Add pair to existing relation if not already there
int add_relation (pair*, int, pair);

// Case 1:

// The relation R is reflexive if xRx for every x in X
int is_reflexive(pair*, int);

// The relation R on the set X is called irreflexive
// if xRx is false for every x in X
int is_irreflexive(pair*, int);

// A binary relation R over a set X is symmetric if:
// for all x, y in X xRy <=> yRx
int is_symmetric(pair*, int);

// A binary relation R over a set X is antisymmetric if:
// for all x,y in X if xRy and yRx then x=y
int is_antisymmetric(pair*, int);

// A binary relation R over a set X is asymmetric if:
// for all x,y in X if at least one of xRy and yRx is false
int is_asymmetric(pair*, int);

// A homogeneous relation R on the set X is a transitive relation if:
// for all x, y, z in X, if xRy and yRz, then xRz
int is_transitive(pair*, int);

// Case 2:

// A partial order relation is a homogeneous relation that is
// reflexive, transitive, and antisymmetric
int is_partial_order(pair*, int);

// A total order relation is a partial order relation that is connected
int is_total_order(pair*, int);

// Relation R is connected if for each x, y in X:
// xRy or yRx (or both)
int is_connected(pair*, int);

int find_max_elements(pair*, int, int*);
int find_min_elements(pair*, int, int*);
int get_domain(pair*, int, int*);

// Case 3:

int composition (pair*, int, pair*, int, pair*);

int cmp (const void *a, const void *b) {
	return *(int*)a - *(int*)b;
}

int insert_int (int *tab, int n, int new_element) {
	int pos = 0;
	while (pos < n && tab[pos] < new_element) pos++;
	while (pos < n) {
		int tmp = tab[pos];
		tab[pos] = new_element;
		new_element = tmp;
		pos++;
	}
	tab[pos] = new_element;
	return n + 1;
}

// Add pair to existing relation if not already there
int add_relation (pair *tab, int n, pair new_pair) {
	if (n == MAX_REL_SIZE) return n;
	for (int i = 0; i < n; i++) if (tab[i].first == new_pair.first && tab[i].second == new_pair.second) return n;
	tab[n] = new_pair;
	return n+1;
}

// Read number of pairs, n, and then n pairs of ints
int read_relation(pair *relation) {
	int n;
	scanf("%d", &n);
	for (int i = 0; i < n; i++) {
		pair p;
		scanf("%d %d", &p.first, &p.second);
		add_relation(relation, i, p);
	}
	return n;
}

void print_int_array(int *array, int n) {
	printf("%d\n", n);
	for (int i = 0; i < n; i++) printf("%d ", array[i]);
	printf("\n");
}

int main(void) {
	int to_do;
	pair relation[MAX_REL_SIZE];
	pair relation_2[MAX_REL_SIZE];
	pair comp_relation[MAX_REL_SIZE];
	int domain[MAX_REL_SIZE];
	int max_elements[MAX_REL_SIZE];
	int min_elements[MAX_REL_SIZE];

	scanf("%d",&to_do);
	int size = read_relation(relation);
	int ordered, size_2, n_domain;

	switch (to_do) {
		case 1:
			printf("%d ", is_reflexive(relation, size));
			printf("%d ", is_irreflexive(relation, size));
			printf("%d ", is_symmetric(relation, size));
			printf("%d ", is_antisymmetric(relation, size));
			printf("%d ", is_asymmetric(relation, size));
			printf("%d\n", is_transitive(relation, size));
			break;
		case 2:
			ordered = is_partial_order(relation, size);
			n_domain = get_domain(relation, size, domain);
			printf("%d %d\n", ordered, is_total_order(relation, size));
			print_int_array(domain, n_domain);
			if (!ordered) break;
			int no_max_elements = find_max_elements(relation, size, max_elements);
			int no_min_elements = find_min_elements(relation, size, min_elements);
			print_int_array(max_elements, no_max_elements);
			print_int_array(min_elements, no_min_elements);
			break;
		case 3:
			size_2 = read_relation(relation_2);
			printf("%d\n", composition(relation, size, relation_2, size_2, comp_relation));
			break;
		default:
			printf("NOTHING TO DO FOR %d\n", to_do);
			break;
	}
	return 0;
}

int is_reflexive(pair *relation, int n) {
	int domain[MAX_REL_SIZE * 2];
	int domainSize = get_domain(relation, n, domain);
	int counter = 0;
	for (int i = 0; i < n; i++) if (relation[i].first == relation[i].second) counter++;
	return counter == domainSize ? 1 : 0;
}

int is_irreflexive(pair *relation, int n) {
	for (int i = 0; i < n; i++) if (relation[i].first == relation[i].second) return 0;
	return 1;
}

int is_symmetric(pair *relation, int n) {
	for (int i = 0; i < n; i++) {
		int flag = 0;
		for (int j = 0; j < n; j++) if (relation[i].first == relation[j].second && relation[j].first == relation[i].second) flag = 1;
		if (!flag) return 0;
	}
	return 1;
}

int is_antisymmetric(pair *relation, int n) {
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			if (relation[i].first == relation[j].second && relation[j].first == relation[i].second && relation[i].first != relation[i].second) return 0;
	return 1;
}

int is_asymmetric(pair *relation, int n) {
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			if (relation[i].first == relation[j].second && relation[j].first == relation[i].second) return 0;
	return 1;
}

int is_transitive(pair *relation, int n) {
	pair needed[MAX_REL_SIZE];
	int neededSize = 0;
	for (int i = 0; i < n; i++)
		for (int j = 0; j < n; j++)
			if (relation[i].second == relation[j].first) neededSize = add_relation(needed, neededSize, (pair) {relation[i].first, relation[j].second});
	for (int i = 0; i < neededSize; i++) {
		int flag = 0;
		for (int j = 0; j < n; j++) if (needed[i].first == relation[j].first && needed[i].second == relation[j].second) flag = 1;
		if (!flag) return 0;
	}
	return 1;
}

int is_partial_order(pair *relation, int n) {
	if (is_reflexive(relation, n) && is_antisymmetric(relation, n) && is_transitive(relation, n)) return 1;
	return 0;
}

int is_total_order(pair *relation, int n) {
	if (is_partial_order(relation, n) && is_connected(relation, n)) return 1;
	return 0;
}

int is_connected(pair *relation, int n) {
	int domain[MAX_REL_SIZE * 2];
	int domainSize = get_domain(relation, n, domain);
	for (int i = 0; i < domainSize - 1; i++) {
		for (int j = i + 1; j < domainSize; j++) {
			int x = domain[i];
			int y = domain[j];
			int flag = 0;
			for (int k = 0; k < n; k++)
				if ((relation[k].first == x && relation[k].second == y) || (relation[k].first == y && relation[k].second == x)) flag = 1;
			if (!flag) return 0;
		}
	}
	return 1;
}

int find_max_elements(pair *relation, int n, int *maxElements) {
	int amount = 0;
	int domain[MAX_REL_SIZE * 2];
	int domainSize = get_domain(relation, n, domain);
	for (int i = 0; i < domainSize; i++) {
		int x = domain[i];
		int flag = 1;
		for (int j = 0; j < n; j++) if (relation[j].second != x && relation[j].first == x) flag = 0;
		if (flag) maxElements[amount++] = x;
	}
	return amount;
}

int find_min_elements(pair *relation, int n, int *minElements) {
	int amount = 0;
	int domain[MAX_REL_SIZE * 2];
	int domainSize = get_domain(relation, n, domain);
	for (int i = 0; i < domainSize; i++) {
		int x = domain[i];
		int flag = 1;
		for (int j = 0; j < n; j++) if (relation[j].first != x && relation[j].second == x) flag = 0;
		if (flag) minElements[amount++] = x;
	}
	return amount;
}

int get_domain(pair *relation, int n, int *domain) {
	int size = 0;
	for (int i = 0; i < n; i++) {
		if (bsearch(&relation[i].first, domain, size, sizeof(int), cmp) == NULL) size = insert_int(domain, size, relation[i].first);
		if (bsearch(&relation[i].second, domain, size, sizeof(int), cmp) == NULL) size = insert_int(domain, size, relation[i].second);
	}
	return size;
}

int composition(pair *rel1, int n1, pair *rel2, int n2, pair *relResult) {
	int resultSize = 0;
	for (int i = 0; i < n1; i++)
		for (int j = 0; j < n2; j++)
			if (rel1[i].second == rel2[j].first) resultSize = add_relation(relResult, resultSize, (pair) {rel1[i].first, rel2[j].second});
	return resultSize;
}