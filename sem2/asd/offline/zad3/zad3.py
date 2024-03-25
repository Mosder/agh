# Maciej Borowiec
#
# 1. Tworzę 2 tabele (dla x-ów - xAmount i dla y-ów - yAmount) o długości len(P)+1,
#   które dla indeksu i będą posiadać ile jest punktów których x (lub y) jest mniejszy lub równy i
#   Zatem dla każdego punktu (xi,yi):
#     - xAmount[xi-1] zawiera liczbę punktów o x < xi
#     - yAmount[yi-1] zawiera liczbę punktów o y < yi
# 2. Dla każdego punktu (xi,yi) szukamy wartości: xAmount[xi-1] + yAmount[yi-1] - (len(P) - 1)
#   Maksimum tej wartości jest szukanym wynikiem
#   Dowód:
#     Podana wartość dla dowolnego danego punktu (xi,yi) jest równa temu jakbyśmy porównywali go z każdym innym punktem (xj,yj) (i != j) i wtedy:
#       - Dodawali 1, gdy xj < xi
#       - Dodawali 1, gdy yj < yi
#       - Odejmowali 1 zawsze
#     Żeby udowodnić poprawność algorytmu załóżmy, że szukamy wartości dla punktu (xi, yi)
#     Rozpatrzmy teraz porównanie z losowo wybranym punktem (xj,yj) - istnieją 4 różne przypadki:
#       I. xj < xi oraz yj < yi:
#         Dodajemy 2 (bo xj < xi oraz yj < yi) oraz odejmujemy 1
#         Zatem dodaliśmy 1 - zgadza się, bo (xi,yi) dominuje (xj,yj)
#       II. xj >= xi oraz yj < yi:
#         Dodajemy 1 (bo yj < yi) oraz odejmujemy 1
#         Zatem dodaliśmy 0 - zgadza się, bo (xi,yi) nie dominuje (xj,yj)
#       III. xj < xi oraz yj >= yi:
#         Dodajemy 1 (bo xj < xi) oraz odejmujemy 1
#         Zatem dodaliśmy 0 - zgadza się, bo (xi,yi) nie dominuje (xj,yj)
#       IV. xj >= xi oraz yj >= yi (ważne jest to, że tylko jedna współrzędna może być równa, bo rozpatrujemy różne punkty):
#         Dodajemy 0 oraz odejmujemy 1
#         Zatem odjęliśmy 1 - jedyny przypadek, dla którego się nie zgadza, bo sztucznie obniżamy wartości
#         Jednak nie należy się tym przejmować, bo albo:
#           a) xj > xi oraz yj > yi - wtedy (xi,yi) jest dominowany przez (xj,yj) zatem wartość dla (xj,yj) i tak musi być większa, a my szukamy tylko maksimum
#           b) xi == xj albo yi == yj - podobnie do a), ale tym razem (xi,yi) nie jest dominowany, więc wartość dla (xj,yj) będzie większa bądź równa
#
# Złożoność czasowa i pamięciowa: O(n)

from zad3testy import runtests

def dominance(P):
  n = len(P)
  xAmount = [0 for _ in range(n+1)]
  yAmount = [0 for _ in range(n+1)]
  for i in range(n):
    xAmount[P[i][0]] += 1
    yAmount[P[i][1]] += 1
  for i in range(1, n):
    xAmount[i+1] += xAmount[i]
    yAmount[i+1] += yAmount[i]
  maxDom = 0
  for i in range(n):
    maxDom = max(maxDom, xAmount[P[i][0]-1] + yAmount[P[i][1]-1])
  return maxDom - n + 1

# zmien all_tests na True zeby uruchomic wszystkie testy
runtests( dominance, all_tests = True )
