-- Wyświetl zamówienia dla których liczba pozycji zamówienia jest większa niż 5
select OrderID, count(*) as [occurences]
from [Order Details]
group by OrderID
having count(*) > 5

-- Wyświetl  klientów dla których w 1998 roku zrealizowano więcej niż 8 zamówień
-- (wyniki posortuj malejąco wg  łącznej kwoty za dostarczenie zamówień dla każdego z klientów)
select Orders.CustomerID, Customers.ContactName, count(*) as [order amount], sum(Orders.Freight) as [total price]
from Orders
inner join Customers
on Orders.CustomerID = Customers.CustomerID
where year(ShippedDate) = 1998
group by Orders.CustomerID, Customers.ContactName
having count(*) > 8
order by sum(Orders.Freight) desc


