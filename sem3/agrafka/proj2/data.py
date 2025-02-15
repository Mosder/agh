from testy import *


problems = [
# Przypadki, które można w sensownym czasie przeanalizować "ręcznie"

# Test 0
{"arg": [1, 3,
  [],
  [("k", 1, 2)],
  ],
  "hint": True},

# Test 1
{"arg": [1, 3,
  [],
  [("k", 1, 1)],
  ],
  "hint": False},

# Test 2
{"arg": [2, 2,
  [],
  [("b", 1, 1), ("b", 2, 2)],
  ],
  "hint": False},

# Test 3
{"arg": [2, 2,
  [],
  [("b", 1, 2)],
  ],
  "hint": True},

# Test 4
{"arg": [2, 2,
  [(1, 1), (2, 2)],
  [("r", 1, 2)],
  ],
  "hint": False},

# Test 5
{"arg": [2, 2,
  [],
  [("q", 2, 1)],
  ],
  "hint": True},

# Test 6
{"arg": [3, 3,
  [],
  [("n", 3, 2)],
  ],
  "hint": True},

# Test 7
{"arg": [3, 3,
  [(2, 3)],
  [("n", 3, 1)],
  ],
  "hint": False},

# Test 8
{"arg": [3, 3,
  [(2, 1), (2, 2), (2, 3)],
  [("k", 1, 2), ("q", 3, 3)],
  ],
  "hint": False},

# Test 9
{"arg": [3, 3,
  [(2, 1), (2, 2), (2, 3)],
  [("k", 1, 2), ("q", 3, 3)],
  ],
  "hint": False},

# Test 10
{"arg": [4, 3,
  [(1, 2), (2, 2), (3, 2), (1, 1)],
  [("k", 1, 3)],
  ],
  "hint": True},

# Test 11
{"arg": [1, 5,
  [],
  [("r", 1, 2)],
  ],
  "hint": False},

# Test 12
{"arg": [2, 5,
  [(2, 1), (2, 3), (2, 4), (2, 5)],
  [("k", 2, 2)],
  ],
  "hint": True},

# Test 13
{"arg": [3, 2,
  [],
  [("k", 3, 2), ("n", 1, 2)],
  ],
  "hint": True},

# Test 14
{"arg": [2, 2,
  [(1, 2)],
  [("k", 1, 1), ("k", 2, 2)],
  ],
  "hint": False},

# Test 15
{"arg": [3, 3,
  [(2, 1), (2, 2), (2, 3)],
  [("k", 3, 3), ("n", 3, 1)],
  ],
  "hint": True},

# Trochę większe testy (bruteforce powinien miec szansę)
# Test 16
{"arg": [4, 5,
  [],
  [("k", 1, 2)],
  ],
  "hint": True},

# Test 17
{"arg": [3, 6,
  [(2, 2), (3, 3), (2, 5)],
  [("q", 1, 1)],
  ],
  "hint": False},

# Test 18
{"arg": [4, 4,
  [(2, 2), (3, 3)],
  [("k", 1, 1), ("b", 2, 4)],
  ],
  "hint": True},

# Test 19
{"arg": [4, 4,
  [(2, 2), (3, 3), (3, 2), (3, 4)],
  [("n", 1, 1), ("n", 2, 4)],
  ],
  "hint": True},

# Test 20
{"arg": [4, 5,
  [(2, 2), (3, 3), (4, 2)],
  [("q", 2, 4), ("b", 3, 1)],
  ],
  "hint": True},

# Testy za duże na najprostszego bruteforce'a
# Test 21
{"arg": [3, 7,
  [(1,0), (1,1), (1,2), (2, 3)],
  [("r", 2, 2), ("k", 0, 0), ("n", 2, 5)],
  ],
  "hint": False},

# Test 22
{"arg": [8, 8,
  [],
  [("n", 4, 4)],
  ],
  "hint": True},

# Test 23
{"arg": [5, 5,
  [],
  [("k", 3, 4), ("b", 1, 2)],
  ],
  "hint": True},

# Test 24
{"arg": [5, 5,
  [],
  [("k", 3, 4), ("n", 1, 2)],
  ],
  "hint": True},

# Test 25
{"arg": [5, 6,
  [(2, 1), (4, 4), (4, 3), (2, 5)],
  [("b", 3, 4), ("q", 1, 2)],
  ],
  "hint": False},

# Test 26
{"arg": [5, 4,
  [(2, 1), (4, 4), (4, 3), (2, 4)],
  [("k", 3, 4), ("n", 5, 3), ("b", 1, 2)],
  ],
  "hint": True},

# Test 27
{"arg": [5, 4,
  [],
  [("n", 1, 1), ("n", 1, 2), ("n", 1, 3)],
  ],
  "hint": True},

# Test 28
{"arg": [5, 5,
  [(3, 3), (1, 4), (3, 2), (5, 5)],
  [("n", 1, 1), ("n", 1, 2), ("n", 1, 3)],
  ],
  "hint": False},

# Test 29
{"arg": [10, 2,
  [],
  [("n", 1, 1), ("n", 1, 2), ("n", 2, 1), ("b", 2, 2)],
  ],
  "hint": False},

# Test 30
{"arg": [6, 7,
  [(2, 1), (4, 4), (5, 3), (2, 4)],
  [("k", 3, 4), ("n", 5, 3)] ,
  ],
  "hint": True},

# Test 31
{"arg": [7, 6,
  [],
  [("k", 1, 1), ("r", 1, 2)] ,
  ],
  "hint": True},

# Test 32
{"arg": [3, 5,
  [],
  [("n", 1, 5), ("r", 1, 2), ("b", 3, 5)] ,
  ],
  "hint": True},

# Test 33
{"arg": [4, 7,
  [(1,3), (3, 7), (3, 6), (4, 6), (2, 4)],
  [("k", 1, 5), ("b", 1, 2), ("b", 3, 5)] ,
  ],
  "hint": True},

# Test 34
{"arg": [10, 2,
  [(5, 1), (7, 2)],
  [("b", 1, 1), ("r", 1, 2), ("n", 10, 1), ("b", 10, 2)],
  ],
  "hint": False},

# Test 35
{"arg": [6, 9,
  [(1, 3), (1, 5), (1, 10),
    (2, 3), (2, 4), (2, 5), (2, 7), (2, 9),
    (3, 2), (3, 4), (3, 7), (3, 8), (3, 9),
    (4, 1), (4, 2), (4, 5), (4, 6), (4, 9),
    (5, 2), (5, 3), (5, 6), (5, 7), (5, 9),
    (6, 3), (6, 4), (6, 6), (6, 9), (6, 10),],
  [("b", 1, 1), ("r", 1, 2), ("n", 1, 8)],
  ],
  "hint": False},
]

# problems = [problems[-1]]


def printarg(N, M, holes, pieces):
    print(f"{N} x {M}, {len(holes)} dziur, {len(pieces)} figur")

def printhint(hint):
    print("Wynik: {}".format(hint))

def printsol(sol):
    print("Uzyskany wynik: {}".format(sol))

def check(N, M, holes, pieces, hint, sol):
    if hint == sol:
        print("Test zaliczony")
        return True
    else:
        print("NIEZALICZONY!")
        print(f"got {sol}, wanted {hint}")
        return False

def runtests(f):
    internal_runtests(printarg, printhint, printsol, check, problems, f)
