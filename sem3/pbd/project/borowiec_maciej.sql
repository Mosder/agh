-- Maciej Borowiec

-- 1.
use Northwind;
with confOrders as (
    select OrderID
    from [Order Details] od
    inner join Products p
    on od.ProductID = p.ProductID
    inner join Categories c
    on p.CategoryID = c.CategoryID
    where CategoryName = 'confections'
)
select OrderID, c.CompanyName
from Customers c
inner join Orders o
on c.CustomerID = o.CustomerID
inner join Shippers s
on o.ShipVia = s.ShipperID
where s.CompanyName = 'United Package' and year(OrderDate) = 1997
  and month(OrderDate) in (3,4,5) and OrderID not in (select * from confOrders)

-- 2.
-- W zadaniu było podane, że wystarczy podać jednego klienta w wypadku remisu.
-- Założyłem, że wystarczy != trzeba i podałem wszystkich.
use Northwind;
with orderCount as (
    select e.EmployeeID, FirstName, LastName, c.CustomerID, CompanyName, count(OrderID) as orderCount
    from Employees e
    inner join Orders o
    on e.EmployeeID = o.EmployeeID
    inner join Customers c
    on o.CustomerID = c.CustomerID
    where year(OrderDate) = 1997
    group by e.EmployeeID, FirstName, LastName, c.CustomerID, CompanyName
),
maxOrders as (
    select EmployeeID, max(orderCount) as maxOrders
    from orderCount
    group by EmployeeID
)
select FirstName, LastName, CompanyName, orderCount
from orderCount oc
inner join maxOrders mo
on mo.EmployeeID = oc.EmployeeID and orderCount = maxOrders
order by mo.EmployeeID

-- 3.
use library;
with borrowed as (
    select m.member_no, firstname, lastname, street, city, state, zip, title
    from member m
    inner join juvenile j
    on m.member_no = j.member_no
    inner join adult a
    on j.adult_member_no = a.member_no
    inner join loan l
    on m.member_no = l.member_no
    inner join title t
    on l.title_no = t.title_no
    where state = 'AZ'
)
select firstname, lastname, street, city, state, zip, title
from borrowed
union all
select firstname, lastname, street, city, state, zip, 'BRAK'
from member m
inner join juvenile j
on m.member_no = j.member_no
inner join adult a
on j.adult_member_no = a.member_no
where state = 'AZ' and m.member_no not in (select member_no from borrowed)
