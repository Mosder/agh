-- 1.
use library;
with reserved as (
    select m.member_no, firstname, lastname, street, city, state, zip, title
    from member m
    inner join adult a
    on m.member_no = a.member_no
    inner join loan l
    on m.member_no = l.member_no
    inner join title t
    on l.title_no = t.title_no
    where state = 'AZ'
)
select firstname, lastname, street, city, state, zip, title
from reserved
union
select firstname, lastname, street, city, state, zip, 'BRAK'
from member m
inner join adult a
on m.member_no = a.member_no
where state = 'AZ' and m.member_no not in (select member_no from reserved)

-- 2.
use Northwind;
with bought as (
    select ProductID
    from [Order Details] od
    inner join Orders o
    on od.OrderID = o.OrderID
    where OrderDate >= '1997.02.20' and OrderDate < '1997.02.26'
)
select ProductName, CompanyName, CategoryName
from Products p
inner join Suppliers s
on p.SupplierID = s.SupplierID
inner join Categories c
on p.CategoryID = c.CategoryID
where CategoryName = 'Beverages' and ProductID not in (select * from bought)

-- 3.
use Northwind;
with orderCount as (
    select c.CustomerID, CompanyName, count(OrderID) as orderCount
    from Customers c
    inner join Orders o
    on c.CustomerID = o.CustomerID
    where year(OrderDate) = 1997 and month(OrderDate) = 2
    group by c.CustomerID, CompanyName
),
orderPriceSum as (
    select CustomerID, sum(UnitPrice * Quantity * (1-Discount)) as ordersPriceSum
    from [Order Details] od
    inner join Orders o
    on od.OrderID = o.OrderID
    where year(OrderDate) = 1997 and month(OrderDate) = 2
    group by CustomerID
)
select CompanyName, orderCount, ordersPriceSum
from orderCount oc
inner join orderPriceSum ops
on oc.CustomerID = ops.CustomerID
union
select CompanyName, 0, 0
from Customers
where CustomerID not in (select CustomerID from orderCount)