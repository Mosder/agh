-- 1. było wałkowane milion razy. używaj szukaj. zamykam.

-- 2.
use Northwind;
with conf as (
    select OrderID
    from [Order Details] od
    inner join Products p
    on od.ProductID = p.ProductID
    inner join Categories c
    on p.CategoryID = c.CategoryID
    where CategoryName = 'confections'
)
select OrderID, OrderDate
from Orders o
inner join Shippers s
on o.ShipVia = s.ShipperID
where year(OrderDate) = 1997 and month(OrderDate) in (3,4,5) and OrderID not in (select * from conf)

-- 3.
use Northwind;
with seafoodProducts as (
    select ProductID
    from Products p
    inner join Categories c
    on p.CategoryID = c.CategoryID
    where CategoryName = 'Seafood'
),
orderCount as (
    select ShipperID, CompanyName, p.ProductID, ProductName, count(o.OrderID) as orderCount
    from Shippers s
    inner join Orders o
    on s.ShipperID = o.ShipVia
    inner join [Order Details] od
    on o.OrderID = od.OrderID
    inner join Products p
    on od.ProductID = p.ProductID
    where p.ProductID in (select * from seafoodProducts) and year(ShippedDate) = 1997 and month(ShippedDate) = 4
    group by ShipperID, CompanyName, p.ProductID, ProductName
),
maxOrders as (
    select ShipperID, max(orderCount) as maxOrders
    from orderCount
    group by ShipperID
)
select CompanyName, ProductName, orderCount
from orderCount oc
inner join maxOrders mo
on mo.ShipperID = oc.ShipperID and orderCount = maxOrders
order by oc.ShipperID