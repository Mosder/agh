#include <stdio.h>
#include <math.h>

#define TEST 0   // 1 - dla testowania, 0 - dla sprawdzarki

#define RECURS_LEVEL_MAX  10
#define N_MAX             10

/////////////////////////////////////////////////////////////////////
// 7.1 Calki jednokrotne

// Przyklady podcalkowych funkcji jednej zmiennej

typedef double (*Func1vFp) (double);  // Definiowanie nazwy dla typu wskaznika do funkcji jedej zmiennej 

double f_poly(double x) {  //    polynomial  a[0]+a[1]x+ ... + a[n]x^n
    return 2*pow(x,5) - 4*pow(x,4) + 3.5*pow(x,2) + 1.35*x - 6.25;
}

double f_rat(double x) {  // Przyklad funkcji podcalkowej Proba z funkcja f(x) = 1/((x-0.5)*(x-0.5)+0.01) w przedziale od 0 do 3.
    return 1 / (pow(x-0.5,2) + 0.01);
}

double f_exp(double x) {  // Przyklad funkcji podcalkowej
    return 2*x*exp(-1.5*x) - 1;
}

double f_trig(double x) {  // Przyklad funkcji podcalkowej
    return x * tan(x);
}

// Obliczanie kwadratur złożonych dla funkcji jednej zmiennej

double quad_rect_left(Func1vFp f1, double a, double b, int n) {  // Prostokatow leftpoint 
    double result = 0;
    double width = (b-a)/n;
    for (int i = 0; i < n; i++) result += width * f1(a + i*width);
    return result;
}

double quad_rect_right(Func1vFp f1, double a, double b, int n) {  // Prostokatow rightpoint 
    double result = 0;
    double width = (b-a)/n;
    for (int i = 0; i < n; i++) result += width * f1(a + (i+1)*width);
    return result;
}

double quad_rect_mid(Func1vFp f1, double a, double b, int n) {  // Prostokatow midpoint 
    double result = 0;
    double width = (b-a)/n;
    for (int i = 0; i < n; i++) result += width * f1(a + (i+0.5)*width);
    return result;
}

double quad_trap(Func1vFp func, double a, double b, int n) {  // Trapezow
    double result = 0;
    double width = (b-a)/n;
    double prevVal = func(a);
    for (int i = 1; i <= n; i++) {
        double val = func(a + i*width);
        result += 0.5 * width * (prevVal + val);
        prevVal = val;
    }
    return result;
}

double quad_simpson(Func1vFp f, double a, double b, int n) {  // Simpsona
    double result = 0;
    double width = (b-a)/n;
    double prevVal = f(a);
    for (int i = 1; i <= n; i++) {
        double val = f(a + i*width);
        result += width / 6 * (prevVal + 4*f(a + (i-0.5)*width) + val);
        prevVal = val;
    }
    return result;
}

// Definiowanie nazwy dla typu wskaznika do funkcji obliczającej kwadraturę funkcji jednej zmiennej  
typedef double (*QuadratureFp) (Func1vFp, double, double, int); 

// Definicja i inicjowanie tablicy wskaznikow do funkcji jednej zmiennej, w kolejności: f_poly, f_rat, f_trig, f_exp.
Func1vFp func_tab[4]={f_poly, f_rat, f_trig, f_exp};
// Definicja i inicjowanie tablicy wskaznikow do kwadratur dla funkcji jednej zmiennej, w kolejności:
// quad_rect_left, quad_rect_right, quad_rect_mid, quad_trap, quad_simpson.
QuadratureFp quad_tab[5]={quad_rect_left, quad_rect_right, quad_rect_mid, quad_trap, quad_simpson};

// Oblicza i zwraca wartosc wskazanej indeksem quad_no kwadratury dla wskazanej indeksem fun_no funkcji
// w przedziale [a,b] z podzialem na n podprzedzialow. 
double quad_select(int fun_no, int quad_no, double a, double b, int n) {
    return quad_tab[quad_no](func_tab[fun_no], a, b, n);
}

// Algorytm adaptacyjny obliczania kwadratury,
double recurs(Func1vFp f, double a, double b, double S, double delta, QuadratureFp quad, int level) {
    double mid = (a+b)/2;
    double S1 = quad(f, a, mid, 1);
    double S2 = quad(f, mid, b, 1);
    if (fabs(S1 + S2 - S) <= delta) return S1 + S2;
    if (level >= RECURS_LEVEL_MAX) return NAN;
    double r1 = recurs(f, a, mid, S1, delta/2, quad, level+1);
    double r2 = recurs(f, mid, b, S2, delta/2, quad, level+1);
    return r1 + r2;
}

// Funkcja inicjująca rekurencję
double init_recurs(Func1vFp f, double a, double b, double delta, QuadratureFp quad) {
    return recurs(f, a, b, quad(f, a, b, 1), delta, quad, 0);
}

///////////////////////////////////////////////////////////////
// 7.2 Calki dwukrotne:

typedef double (*Func2vFp) (double, double);  // Definiowanie nazwy dla typu wskaznika do funkcji dwoch zmiennych

// Przykladowe funkcje dwoch zmiennych:
double func2v_1(double x, double y) {  //fxy1
    return sin(x/y);
}
double func2v_2(double x, double y) {
    return 2-x*x-y*y*y;
}

// Przykladowe funkcje brzegu obszaru calkowania (do calki podwojnej nad obszarem normalnym) 
double lower_bound1(double x) {
    return sin(x);
}
double upper_bound1(double x) {
    return x*x+1;
}
double lower_bound2(double x) {
    return 0.7*exp(-2*x*x);
}
double upper_bound2(double x) {
    return sin(10*x);
}

// Metoda prostokatow (leftpoint) oblicza przyblizenie calki podwojnej nad obszarem prostokatnym
double dbl_integr(Func2vFp f, double x1, double x2, int nx, double y1, double y2, int ny)  {
    double xWidth = (x2-x1)/nx;
    double yWidth = (y2-y1)/ny;
    double result = 0;
    for (int i = 0; i < ny; i++) {
        for (int j = 0; j < nx; j++) {
            result += f(x1 + j*xWidth, y1 + i*yWidth);
        }
    }
    return xWidth * yWidth * result;
}

// Oblicza kwadrature prostokatow midpoint dla calki podwojnej nad obszarem normalnym wzgledem osi 0x
double dbl_integr_normal_1(Func2vFp f, double x1, double x2, int nx, double hy, Func1vFp fg, Func1vFp fh)  {
    double xWidth = (x2-x1)/nx;
    double result = 0;
    for (int i = 0; i < nx; i++) {
        double currentX = x1 + (i+0.5)*xWidth;
        double currentG = fg(currentX);
        double currentH = fh(currentX);
        int ny = (int) ceil((currentH - currentG)/hy);
        double yWidth = (currentH - currentG)/ny;
        for (int j = 0; j < ny; j++) {
            result += xWidth * yWidth * f(currentX, currentG + (j+0.5)*yWidth);
        }
    }
    return result;
}

// Oblicza kwadrature prostokatow leftpoint dla calki podwojnej nad obszarami normalnymi wzgledem osi 0x 
double dbl_integr_normal_n(Func2vFp f, double x1, double x2, int nx, double y1, double y2, int ny, Func1vFp fg, Func1vFp fh)  {
    double xWidth = (x2-x1)/nx;
    double yWidth = (y2-y1)/ny;
    double result = 0;
    for (int i = 0; i < nx; i++) {
        double currentX = x1 + i*xWidth;
        double currentG = fg(currentX);
        double currentH = fh(currentX);
        double lowerBound = fmax(y1, currentG);
        double upperBound = fmin(y2, currentH);
        int ny2 = (int) ceil((upperBound - lowerBound)/yWidth);
        double yWidth2 = (upperBound-lowerBound)/ny2;
        for (int j = 0; j < ny2; j++) {
            double currentY = lowerBound + j*yWidth2;
            if (currentG <= currentY && currentY <= currentH)
                result += xWidth * yWidth2 * f(currentX, currentY);
        }
    }
    return result;
}

///////////////////////////////////////////////////////////////
// 7.3 Calki wielokrotne:

typedef double (*FuncNvFp)(const double*, int);
typedef int (*BoundNvFp)(const double*, int);

// Przykladowa funkcja trzech zmiennych:
double func3v(const double v[], int n) {
    return v[0]-v[1]+2*v[2];
}

// Przykładowy predykat w przestrzeni 3-wymiarowej
int bound3v(const double v[], int n) {
    if(v[0]>0 && v[0]<0.5 && v[1]*v[1]+(v[2]-1)*(v[2]-1)<1) return 1;   // walec
    return 0;
}

// Przykladowa funkcja n zmiennych:
double funcNv(const double v[], int n) {
    double fv=1.;
    for(int i=1; i<n; ++i) fv += sin(i*v[i]);   
    return fv;
}
// Przykładowy predykat w przestrzeni n-wymiarowej
int boundNv(const double v[], int n) {
    double r=0.0;
    for(int i=0; i<n; ++i) r += (v[i]-1)*(v[i]-1);  // hiperkula n-wymiarowa
    if(r > 1.) return 0;
    return 1;
}

// Obliczanie calek wielokrotnych

// Oblicza calke potrojna "nad" prostopadloscianem z predykatem wykluczajacym jego czesci (jezeli boundary != NULL).
// Metoda prostokatow wstecz (rightpoint) wzdluz kazdej zmiennej.
double trpl_quad_rect(FuncNvFp f, const double variable_lim[][2], const int tn[], BoundNvFp boundary) {
    double xWidth = (variable_lim[0][1] - variable_lim[0][0])/tn[0];
    double yWidth = (variable_lim[1][1] - variable_lim[1][0])/tn[1];
    double zWidth = (variable_lim[2][1] - variable_lim[2][0])/tn[2];
    double result = 0;
    for (int i = 0; i < tn[0]; i++) {
        for (int j = 0; j < tn[1]; j++) {
            for (int k = 0; k < tn[2]; k++) {
                double currentX = variable_lim[0][0] + (i+1)*xWidth;
                double currentY = variable_lim[1][0] + (j+1)*xWidth;
                double currentZ = variable_lim[2][0] + (k+1)*xWidth;
                double variables[3] = {currentX, currentY, currentZ};
                if (boundary == NULL || boundary(variables, 3))
                    result += xWidth * yWidth * zWidth * f(variables, 3);
            }
        }
    }
    return result;
}

// Oblicza calke wielokrotna (funkcji n zmiennych) "nad" n wymiarowym hiperprostopadloscianem z predykatem wykluczajacym jego czesci (jezeli boundary != NULL).
// Metoda prostokatow midpoint wzdluz kazdej zmiennej.
void recur_quad_rect_mid(double *psum, FuncNvFp f, int variable_no, double tvariable[], const double variable_lim[][2], const int tn[], int level, BoundNvFp boundary) {
    if (level >= variable_no) {
        if (boundary == NULL || boundary(tvariable, variable_no)) {
            double result = f(tvariable, variable_no);
            for (int i = 0; i < variable_no; i++) result *= (variable_lim[i][1] - variable_lim[i][0])/tn[i];
            *psum += result;
        }
        return;
    }
    double width = (variable_lim[level][1] - variable_lim[level][0])/tn[level];
    for (int i = 0; i < tn[level]; i++) {
        tvariable[level] = variable_lim[level][0] + (i+0.5)*width;
        recur_quad_rect_mid(psum, f, variable_no, tvariable, variable_lim, tn, level+1, boundary);
    }
}

int main(void)
{
    int to_do, n, nx, ny, integrand_fun_no, method_no, flag;
    double a,b,x1,x2,y1,y2,hy,sum,delta;
    double tvariable[N_MAX], variable_lim[N_MAX][2];
    int tn[N_MAX];

    if (TEST) printf("Wpisz numer testu [1, 7]: ");
    scanf("%d", &to_do);
    switch (to_do) {
    case 1: // 7.1.1 wybor funkcji i metody
        if (TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow: ");
        scanf("%lf %lf %d", &a, &b, &n);
        for(int q=0; q<5; ++q) {
            for(int f=0; f<4; ++f) {
                printf("%.5f ",quad_select(f, q, a, b, n));
            }
            printf("\n");
        }
        break;
    case 2: // 7.1.2 rekurencyjny algorytm adaptacyjny
        if(TEST) printf("Nr funkcji (0-3) i metody (0-4): ");
        scanf("%d %d",&integrand_fun_no,&method_no);
        if (TEST) printf("Wpisz przedzial calkowania i tolerancje bledu: ");
        scanf("%lf %lf %lf", &a, &b, &delta);
        printf("%.5f\n",init_recurs(func_tab[integrand_fun_no],a,b,delta,quad_tab[method_no]));
        break;
    case 3: // 7.2.1 calka podwojna nad prostokatem
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow wzdluz x: ");
        scanf("%lf %lf %d",&x1,&x2,&nx);
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow wzdluz y: ");
        scanf("%lf %lf %d",&y1,&y2,&ny);
        printf("%.5f\n",dbl_integr(func2v_2, x1, x2, nx, y1, y2, ny));
        break;
    case 4: // 7.2.2 calka podwojna nad obszarem normalnym
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow zmiennej x: ");
        scanf("%lf %lf %d",&x1,&x2,&nx);
        if(TEST) printf("Wpisz dlugosc podprzedzialu wzdluz y: ");
        scanf("%lf",&hy);
        printf("%.5f\n",dbl_integr_normal_1(func2v_2, x1, x2, nx, hy, lower_bound2, upper_bound2));
        break;
    case 5: // 7.2.3 calka podwojna nad wieloma obszarami normalnymi
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow wzdluz x: ");
        scanf("%lf %lf %d",&x1,&x2,&nx);
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow wzdluz y: ");
        scanf("%lf %lf %d",&y1,&y2,&ny);
        printf("%.5f\n",dbl_integr_normal_n(func2v_2, x1, x2, nx, y1, y2, ny, lower_bound2, upper_bound2));
        break;
    case 6: // 7.3.1 calka potrojna po prostopadloscianie
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow 1. zmiennej: ");
        scanf("%lf %lf %d",&variable_lim[0][0],&variable_lim[0][1],tn);
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow 2. zmiennej: ");
        scanf("%lf %lf %d",&variable_lim[1][0],&variable_lim[1][1],tn+1);
        if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow 3. zmiennej: ");
        scanf("%lf %lf %d",&variable_lim[2][0],&variable_lim[2][1],tn+2);
        if(TEST) printf("Wpisz 1 gdy ograniczenie obszaru całkowania ma byc aktywne; 0 - w przeciwnym przypadku: ");
        scanf("%d",&flag);
        printf("%.5f\n",trpl_quad_rect(func3v, variable_lim, tn, flag?bound3v:NULL));
        break;
    case 7: // 7.3.3 calka n-wymiarowa na hiperprostopadloscianie
        if(TEST) printf("Wpisz liczbe zmiennych <= %d: ", N_MAX);
        scanf("%d",&n);
        if(n>N_MAX) break;
        for(int i=0; i<n; ++i) {
            if(TEST) printf("Wpisz przedzial calkowania i liczbe podprzedzialow %d. zmiennej: ",i+1);
            scanf("%lf %lf %d",&variable_lim[i][0],&variable_lim[i][1],tn+i);
        }
        if(TEST) printf("Wpisz 1 gdy ograniczenie obszaru całkowania ma byc aktywne; 0 - w przeciwnym przypadku: ");
        scanf("%d",&flag);
        sum=0.;
        recur_quad_rect_mid(&sum, funcNv, n, tvariable, variable_lim, tn, 0, flag?boundNv:NULL);//        recur_quad_rect_mid(&sum, func3v_1, n, tvariable, variable_lim, tn, 0, flag?bound3v_2:NULL);
        printf("%.5f\n",sum);
        break;
    default:
        printf("Numer testu spoza zakresu [1, 7] %d", to_do);
    }
    return 0;
}

