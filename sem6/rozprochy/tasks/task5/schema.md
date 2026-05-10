flowchart LR
   %% Użytkownicy
   A[Agencja: A1]
   C[Przewoźnik: C1<br/>(obsługuje: human + cargo)]
   D[Administrator]

   %% RabbitMQ
   X{{Topic Exchange<br/>exchange}}

   QH[(service.human)]
   QC[(service.cargo)]
   QS[(service.satelite)]
   QCONF[(confirmations.A1)]
   QA[(admin.agencies.A1)]
   QCAR[(admin.carriers.C1)]
   QMON[(admin.monitor)]

   %% Bindings (kolejki -> exchange)
   X -- "orders.human" --> QH
   X -- "orders.cargo" --> QC
   X -- "orders.satelite" --> QS
   X -- "confirm.A1" --> QCONF
   X -- "admin.agencies" --> QA
   X -- "admin.all" --> QA
   X -- "admin.carriers" --> QCAR
   X -- "admin.all" --> QCAR
   X -- "#" --> QMON

   %% Publikacja zleceń przez agencję
   A -- "publish: orders.human / orders.cargo / orders.satelite" --> X

   %% Konsumpcja zleceń przez przewoźnika (2 z 3 usług)
   QH -- "consume" --> C
   QC -- "consume" --> C

   %% Potwierdzenie wykonania
   C -- "publish: confirm.A1" --> X
   QCONF -- "consume" --> A

   %% Moduł admina
   D -- "publish: admin.agencies / admin.carriers / admin.all" --> X
   QA -- "consume" --> A
   QCAR -- "consume" --> C

   %% Monitoring premium (kopia wszystkich wiadomości)
   QMON -- "consume" --> D
