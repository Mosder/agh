```mermaid
flowchart LR
    subgraph ADMIN
        direction TB
        AD[Admin]
        QAD(admin.monitor)
    end

    AD -- admin.&lt;target&gt; --> X
    X -- "#" --> QAD --> AD

    A[&lt;agency&gt;] -- orders.&lt;service&gt; --> X[("Exchange (topic)")]
    X -- confirm.&lt;agency&gt; --> QA(confirmations.&lt;agency&gt;) --> A
    X -- admin.all / admin.agencies --> QAAD(admin.agencies.&lt;agency&gt;) --> A

    C[&lt;carrier&gt;] -- confirm.&lt;agency&gt; --> X
    X -- orders.&lt;service&gt; --> QS(service.&lt;service&gt;) --> C
    X -- admin.all / admin.carriers --> QC(admin.carriers.&lt;carrier&gt;) --> C
```
