-- Wybierz nazwy i ceny produktów (baza northwind) o cenie jednostkowej pomiędzy 20.00 a 30.00,
-- dla każdego produktu podaj dane adresowe dostawcy
select ProductName, UnitPrice, Address
from Products as p
inner join Suppliers as s
on p.SupplierID = s.SupplierID
where UnitPrice BETWEEN 20 AND 30
-- Wybierz nazwy produktów oraz inf. o stanie magazynu dla produktów dostarczanych przez firmę ‘Tokyo Traders’
select ProductName, UnitsInStock
from Products as p
inner join Suppliers as s
on p.SupplierID = s.SupplierID
where CompanyName = 'Tokyo Traders'
-- Czy są jacyś klienci którzy nie złożyli żadnego zamówienia w 1997 roku, jeśli tak to pokaż ich dane adresowe
-- Sposób 1.
select CustomerID, ContactName, Address, City, Region, PostalCode, Country
from Customers
where CustomerID not in (
    select CustomerID
    from Orders
    where year(OrderDate) = 1997
)
-- Sposób 2.
select c.CustomerID, ContactName, Address, City, Region, PostalCode, Country
from Customers as c
left join (
    select OrderID, CustomerID, OrderDate
    from Orders
    where year(OrderDate) = 1997
) as o
on c.CustomerID = o.CustomerID
where OrderID is null
-- Sposób 3. (ten dobry, czyli ten, którego ja nie wymyśliłem xd)
select c.CustomerID, ContactName, Address, City, Region, PostalCode, Country
from Customers as c
left join Orders as o
on c.CustomerID = o.CustomerID and year(OrderDate) = 1997
where OrderID is null