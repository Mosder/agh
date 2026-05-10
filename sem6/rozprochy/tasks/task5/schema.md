# Schemat działania aplikacji

System składa się z 3 ról komunikujących się przez RabbitMQ (`topic exchange` o nazwie `exchange`):
- **Agency** – składa zamówienia na usługi.
- **Carrier** – realizuje zamówienia i odsyła potwierdzenia.
- **Admin** – wysyła komunikaty administracyjne oraz monitoruje ruch.

## Kolejki i routing key

| Element | Kolejka | Routing key |
|---|---|---|
| Zamówienia usług | `service.human`, `service.cargo`, `service.satelite` | `orders.<service>` |
| Potwierdzenia dla agencji | `confirmations.<agency>` | `confirm.<agency>` |
| Komunikaty admin do agencji | `admin.agencies.<agency>` | `admin.agencies`, `admin.all` |
| Komunikaty admin do carrierów | `admin.carriers.<carrier>` | `admin.carriers`, `admin.all` |
| Monitoring admina | `admin.monitor` | `#` (wszystko) |

## Przepływ działania

1. **Agency** uruchamia wątek nasłuchu:
   - odbiera potwierdzenia (`confirm.<agency>`),
   - odbiera komunikaty admina (`admin.agencies` i `admin.all`).
2. W głównej pętli **Agency** wybiera usługę (`human/cargo/satelite`) i publikuje zamówienie:
   - `routing_key = orders.<service>`
   - payload: `agency`, `order_id`, `service`.
3. **Carrier** obsługuje dokładnie 2 różne usługi i subskrybuje odpowiednie kolejki `service.<service>`.
4. Po odebraniu zamówienia **Carrier**:
   - przetwarza je,
   - wysyła potwierdzenie do agencji:
     - `routing_key = confirm.<agency>`
     - payload: `order_id`, `service`, `carrier`.
5. **Admin**:
   - wysyła komunikaty do `admin.agencies`, `admin.carriers` albo `admin.all`,
   - równolegle nasłuchuje `admin.monitor` (związanej z `#`) i widzi wszystkie wiadomości przechodzące przez exchange.

## Diagram (Mermaid)

```mermaid
flowchart LR
    A[Agency] -- orders.<service> --> X[(RabbitMQ topic exchange: exchange)]
    X --> SH[service.human]
    X --> SC[service.cargo]
    X --> SS[service.satelite]

    SH --> C[Carrier]
    SC --> C
    SS --> C

    C -- confirm.<agency> --> X
    X --> QCONF[confirmations.<agency>]
    QCONF --> A

    AD[Admin] -- admin.agencies / admin.carriers / admin.all --> X
    X --> QA[admin.agencies.<agency>]
    X --> QC[admin.carriers.<carrier>]
    QA --> A
    QC --> C

    X --> MON[admin.monitor (#)]
    MON --> AD
```
