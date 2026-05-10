# Schemat działania systemu RabbitMQ

```mermaid
flowchart LR
  A1[Agencja 1]
  A2[Agencja 2]
  C1[Przewoznik 1]
  C2[Przewoznik 2]
  AD[Administrator]

  EX((topic exchange: exchange))

  QS_H[service.human]
  QS_C[service.cargo]
  QS_S[service.satelite]
  QA1[confirmations.agency1]
  QA2[confirmations.agency2]
  QAC1[admin.carriers.carrier1]
  QAC2[admin.carriers.carrier2]
  QAA1[admin.agencies.agency1]
  QAA2[admin.agencies.agency2]
  QAM[admin.monitor]

  A1 -- orders.human / orders.cargo / orders.satelite --> EX
  A2 -- orders.human / orders.cargo / orders.satelite --> EX

  EX -- orders.human --> QS_H
  EX -- orders.cargo --> QS_C
  EX -- orders.satelite --> QS_S

  QS_H --> C1
  QS_C --> C1
  QS_C --> C2
  QS_S --> C2

  C1 -- confirm.agency1 / confirm.agency2 --> EX
  C2 -- confirm.agency1 / confirm.agency2 --> EX

  EX -- confirm.agency1 --> QA1
  EX -- confirm.agency2 --> QA2
  QA1 --> A1
  QA2 --> A2

  AD -- admin.agencies / admin.carriers / admin.all --> EX

  EX -- admin.carriers --> QAC1
  EX -- admin.carriers --> QAC2
  EX -- admin.all --> QAC1
  EX -- admin.all --> QAC2
  QAC1 --> C1
  QAC2 --> C2

  EX -- admin.agencies --> QAA1
  EX -- admin.agencies --> QAA2
  EX -- admin.all --> QAA1
  EX -- admin.all --> QAA2
  QAA1 --> A1
  QAA2 --> A2

  EX -- "#" --> QAM
  QAM --> AD
```

## Uzytkownicy (procesy)

- Agencja 1, Agencja 2
- Przewoznik 1 (human + cargo), Przewoznik 2 (cargo + satelite)
- Administrator

## Exchange

- `exchange` (typ: `topic`)

## Kolejki i klucze wiazania (binding keys)

| Kolejka | Binding key |
|---|---|
| `service.human` | `orders.human` |
| `service.cargo` | `orders.cargo` |
| `service.satelite` | `orders.satelite` |
| `confirmations.<agency>` | `confirm.<agency>` |
| `admin.agencies.<agency>` | `admin.agencies`, `admin.all` |
| `admin.carriers.<carrier>` | `admin.carriers`, `admin.all` |
| `admin.monitor` | `#` |

