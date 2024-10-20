-- Podaj maksymalną cenę produktu dla produktów o cenach poniżej 20
select max(UnitPrice)
from Products
where UnitPrice < 20

-- Wypisz informację o wszystkich produktach o cenie powyżej średniej
select *
from Products
where (select avg(UnitPrice) from Products) < UnitPrice

-- Podaj sumę/wartość zamówienia o numerze 10250
select round(sum(Quantity * UnitPrice * (1 - Discount)), 2) as [Order 10250 price]
from [Order Details]
where OrderID = 10250
select cast(sum(Quantity * UnitPrice * (1 - Discount)) as money) as [Order 10250 price]
from [Order Details]
where OrderID = 10250

-- Posortuj zamówienia wg maksymalnej ceny produktu
select orderid, max(UnitPrice) as maxPrice
from [Order Details]
group by orderid
order by maxPrice

-- Podaj liczbę zamówień dostarczanych przez poszczególnych spedytorów (przewoźników)
select ShipVia, count(*) as [shipping count]
from Orders
group by ShipVia

-- Który ze spedytorów był najaktywniejszy w 1997 roku
select top 1 Shippers.ShipperID, Shippers.CompanyName, count(*) as [Orders shipped in 1997]
from Orders
inner join Shippers
on Shippers.ShipperID = Orders.ShipVia
where year(ShippedDate) = 1997
group by Shippers.ShipperID, Shippers.CompanyName
order by count(*) desc

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
where year(OrderDate) = 1998
group by Orders.CustomerID, Customers.ContactName
having count(*) > 8
order by sum(Orders.Freight) desc


