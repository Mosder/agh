-- Porównywanie napisów
-- 2.
select title from employees where lastname like '[B-L]%'

-- Zakes wartości
-- 2.
select * from orders where YEAR(orderdate) = 1997

select orderid, orderdate, customerid, shippeddate
from orders
where ShipCountry = 'Argentina' and (shippeddate is null or shippeddate > getdate())

-- order by
select *
from Orders
where year(OrderDate) = 1997
order by month(OrderDate) desc, Freight

select * from Orders where OrderID = 10250
select concat_ws(', ', Phone, Fax)