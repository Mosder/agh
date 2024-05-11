#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define TEST 0

typedef struct {
    int day; 
    int month; 
    int year;
} Date;

/////////////////////////////////////////////////////////////////
// 8.1 funkcja bsearch2

#define FOOD_MAX  30   // max. liczba artykułów
#define ART_MAX   15   // max. długość nazwy artykułu +1
#define RECORD_MAX 40  // max. długość rekordu (linii) w pliku

typedef struct {
    char art[ART_MAX];
    float price;
    float amount;
    Date valid_date;
} Food;

typedef int (*ComparFp)(const void *, const void *);

int sgn(const float x) { return (x > 0) - (x < 0); }

int compareDates(const Date d1, const Date d2) {
    int result = d1.year - d2.year;
    if (result == 0) result = d1.month - d2.month;
    if (result == 0) result = d1.day - d2.day;
    return result;
}

int compareFoodByDate(const void *p1, const void *p2) {
    return compareDates(((Food*)p1)->valid_date, ((Food*)p2)->valid_date);
}

int compareFood(const void *p1, const void *p2) {
    Food* food1 = (Food*)p1;
    Food* food2 = (Food*)p2;
    int result = strcmp(food1->art, food2->art);
    if (result == 0) {
        result = sgn(food1->price - food2->price);
        if (result == 0) {
            Date d1 = food1->valid_date;
            Date d2 = food2->valid_date;
            result = compareDates(d1, d2);
        }
    }
    return result;
}

void *bsearch2(const void *key, void *base, size_t nitems, size_t size, ComparFp compar, char *result) {
    if (nitems == 0) {
        *result = 0;
        return base;
    }
    int mid = (int) (nitems/2);
    void* pointer = base + mid*size;
    int compareResult = compar(key, pointer);
    if (compareResult == 0) {
        *result = 1;
        return pointer;
    }
    else if (compareResult < 0) return bsearch2(key, base, mid, size, compar, result);
    else return bsearch2(key, pointer+size, nitems-mid-1, size, compar, result);
}

void print_art(Food *p, size_t n, char *art) {
    for (int i = 0; i < n; i++)
        if (strcmp(art, (p+i)->art) == 0)
            printf("%.2f %.2f %d %d %d\n", (p+i)->price, (p+i)->amount,
                (p+i)->valid_date.day, (p+i)->valid_date.month, (p+i)->valid_date.year);
}

Food* add_record(Food *tab, size_t tab_size, int *np, ComparFp compar, Food *new) {
    char result;
    Food *ptr = (Food*)bsearch2(new, tab, *np, sizeof(Food), compar, &result);
    if (result == 0) {
        if (*np == tab_size) return NULL;
        int insertIndex = (int)(ptr - tab);
        for (int i = ++(*np); i > insertIndex; i--) tab[i] = tab[i-1];
        tab[insertIndex] = *new;
    }
    else ptr->amount += new->amount;
    return ptr;
}
  
int read_stream(Food *tab, size_t size, int no, FILE *stream) {
    int np = 0;
    while (no--) {
        Food food;
        memset(food.art, 0, ART_MAX);
        fscanf(stream, "%s %f %f %d %d %d", food.art, &food.price, &food.amount,
            &food.valid_date.day, &food.valid_date.month, &food.valid_date.year);
        add_record(tab, size, &np, compareFood, &food);
    }
    return np;
}

int read_stream0(Food *tab, size_t size, int no, FILE *stream) {
    return read_stream(tab, size, no, stream);
}

float value(Food *food_tab, size_t n, Date curr_date, int anticip) {
    qsort(food_tab, n, sizeof(Food), compareFoodByDate);

    time_t timeNow;
    time(&timeNow);
    struct tm *soughtDate = localtime(&timeNow);
    soughtDate->tm_mday = curr_date.day + anticip;
    soughtDate->tm_mon = curr_date.month - 1;
    soughtDate->tm_year = curr_date.year - 1900;
    mktime(soughtDate);
    Date dateToFind;
    dateToFind.day = soughtDate->tm_mday;
    dateToFind.month = soughtDate->tm_mon + 1;
    dateToFind.year = soughtDate->tm_year + 1900;

    Food expDateFood;
    expDateFood.valid_date = dateToFind;
    char result;
    Food *ptr = (Food*) bsearch2(&expDateFood, food_tab, n, sizeof(Food), compareFoodByDate, &result);
    if (result == 0) return 0;

    float val = ptr->amount * ptr->price;
    int index = (int) (ptr-food_tab);
    for (int i = index-1; i >= 0 && compareFoodByDate(&expDateFood, &food_tab[i]) == 0; i--)
        val += food_tab[i].amount * food_tab[i].price;
    for (int i = index+1; i < n && compareFoodByDate(&expDateFood, &food_tab[i]) == 0; i++)
        val += food_tab[i].amount * food_tab[i].price;
    return val;
}

/////////////////////////////////////////////////////////////////
// 8.3 Sukcesja

#define CHILD_MAX  20   // maksymalna liczba przesuwanej w tablicy grupy osób (elementów tablicy)

enum Sex {F, M};
enum BOOL {no, yes};

struct Bit_data {
    enum Sex sex:1;
    enum BOOL pretendent:1;   // =no - nie pretenduje (panuje albo nie żyje) ale jest w zbiorze dla spójności relacji.
};

typedef struct {
    char *name;
    struct Bit_data bits;
    Date born;
    char *parent;
} Person;

typedef struct {
       char *par_name;
       int index;
} Parent;    // strukturę można rozbudować o pole liczby dzieci

const Date primo_date = { 28, 10, 2011 }; // Data zmiany kolejności sukcesji

int fill_indices_tab(Parent *idx_tab, Person *pers_tab, int size) {
}

void persons_shiftings(Person *person_tab, int size, Parent *idx_tab, int no_parents) {
}

int cleaning(Person *person_tab,int n) {
}

void print_person(const Person *p) {
    printf("%s\n", p->name);
}

void print_persons(const Person *person_tab, int n) {
    for(int i=1; i<=n; ++i, ++person_tab) printf("%2d %12s %s\n", i, person_tab->name, person_tab->parent);
    return;
}

int create_list(Person *person_tab, int n) {
}

////////////////////////////////////////////////////////////////

int main(void)
{
    // Wszyscy potomkowie Jerzego VI (w kolejności przypadkowej):
    Person person_tab[34]={
    {"Charles III",M,no,14,11,1948,"Elizabeth II"},
    {"Anne",F,yes,15,8,1950,"Elizabeth II"},
    {"Andrew",M,yes,19,2,1960,"Elizabeth II"},
    {"Edward",M,yes,10,3,1964,"Elizabeth II"},
    {"David",M,yes,3,11,1961,"Margaret"},
    {"Sarah",F,yes,1,5,1964,"Margaret"},
    {"William",M,yes,21,6,1982,"Charles III"},
    {"Henry",M,yes,15,9,1984,"Charles III"},
    {"Peter",M,yes,15,11,1977,"Anne"},
    {"Zara",F,yes,15,5,1981,"Anne"},
    {"Beatrice",F,yes,8,8,1988,"Andrew"},
    {"Eugenie",F,yes,23,3,1990,"Andrew"},
    {"James",M,yes,17,12,2007,"Edward"},
    {"Louise",F,yes,8,11,2003,"Edward"},
    {"Charles",M,yes,1,7,1999,"David"},
    {"Margarita",F,yes,14,5,2002,"David"},
    {"Samuel",M,yes,28,7,1996,"Sarah"},
    {"Arthur",M,yes,6,5,2019,"Sarah"},
    {"Georg",M,yes,22,7,2013,"William"},
    {"George VI",M,no,14,12,1895,NULL},
    {"Charlotte",F,yes,22,5,2015,"William"},
    {"Louis",M,yes,23,4,2018,"William"},
    {"Archie",M,yes,6,5,2019,"Henry"},
    {"Lilibet",F,yes,4,6,2021,"Henry"},
    {"Savannah",F,yes,29,12,2010,"Peter"},
    {"Isla",F,yes,29,3,2012,"Peter"},
    {"Mia",F,yes,17,1,2014,"Zara"},
    {"Lena",F,yes,18,6,2018,"Zara"},
    {"Elizabeth II",F,no,21,4,1925,"George VI"},
    {"Margaret",F,no,21,8,1930,"George VI"},
    {"Lucas",M,yes,21,3,2021,"Zara"},
    {"Sienna",F,yes,18,9,2021,"Beatrice"},
    {"August",M,yes,9,2,2021,"Eugenie"},
    {"Ernest",M,yes,30,5,2023,"Eugenie"}
    };
    
    int to_do, no;
    size_t size, n;
        Food food_tab[FOOD_MAX];
        char buff[ART_MAX];
        FILE *file;
    if(TEST) printf("Wpisz nr zadania (1 - 3) ");
    scanf("%d", &to_do);

    switch (to_do) {
    case 1:  // bsearch2
        if (TEST) printf("Wpisz liczbe linii danych: ");
        scanf("%d",&no); getchar();
        if(TEST) file = stdin;
        else {
            scanf("%s",buff);
            file = fopen(buff,"r");
//            file = fopen("foods0.txt","r");
            if(file==NULL) { printf("Error 1\n"); break; }
        }
        if (TEST) printf("W %d liniach wpisuj dane: nazwa cena ilosc dd mm yyyy: \n",no);
        n = read_stream(food_tab,FOOD_MAX,no,file);
        if (TEST) printf("Wpisz nazwe artykulu: ");
        scanf("%s",buff);
        print_art(food_tab,n,buff);
        break;
    case 2: // proste sortowanie struktur
        if (TEST) printf("Wpisz liczbe linii danych: ");
        scanf("%d",&no); getchar();
        if(TEST) file = stdin;
        else {
            scanf("%s",buff);
            file = fopen(buff,"r");
//            file = fopen("foods0.txt","r");
            if(file==NULL) { printf("Error 1\n"); break; }
        }
        if (TEST) printf("W %d liniach wpisuj dane: nazwa cena ilosc dd mm yyyy: \n",no);
        n = read_stream0(food_tab,FOOD_MAX,no,file);
        Date curr_date;
        int anticip;
        if (TEST) printf("Wpisz date odniesienia dd mm yyyy: ");
        scanf("%d %d %d",&curr_date.day,&curr_date.month,&curr_date.year);
        if (TEST) printf("Wpisz roznice dni: ");
        scanf("%d",&anticip);
        printf("%.2f\n",value(food_tab,n,curr_date,anticip));
        break;
    case 3: // sukcesja
        if(TEST==1) printf("Wpisz pozycję w kolejce (liczona od 1): ");
        scanf("%d",&no);
    	int no_persons = sizeof(person_tab) / sizeof(Person);
        no_persons = create_list(person_tab,no_persons);
        if(TEST) print_persons(person_tab,no_persons);
        else print_person(person_tab+no-1);
        break;
    default:
        printf("NOTHING TO DO FOR %d\n", to_do);
    }
    return 0;
}

