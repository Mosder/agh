-- 1.
use Northwind;
with o1997 as (
    select c.CustomerID, c.CompanyName, e.EmployeeID, e.FirstName, e.LastName, count(o.OrderID) as orderCount
    from Customers c
    inner join Orders o
    on c.CustomerID = o.CustomerID
    inner join Employees e
    on o.EmployeeID = e.EmployeeID
    where year(o.OrderDate) = 1997
    group by c.CustomerID, c.CompanyName, e.EmployeeID, e.FirstName, e.LastName
),
maxOrders as (
    select CustomerID, max(orderCount) as maxOrders
    from o1997
    group by CustomerID
)
select CompanyName, FirstName, LastName, maxOrders
from o1997 as o
inner join maxOrders as m
on o.CustomerID = m.CustomerID and orderCount = maxOrders
order by o.CustomerID

-- 2.
use Northwind;
with oCount as (
    select e.EmployeeID, e.FirstName, e.LastName, count(o.OrderID) as orderCount
    from Employees e
    inner join Orders o
    on e.EmployeeID = o.EmployeeID
    where year(o.OrderDate) = 1997 and month(o.OrderDate) = 2
    group by e.EmployeeID, e.FirstName, e.LastName
),
oPrice as (
    select EmployeeID, sum(UnitPrice * Quantity * (1 - Discount) + Freight) as orderPriceSum
    from Orders o
    inner join [Order Details] od
    on o.OrderID = od.OrderID
    where year(o.OrderDate) = 1997 and month(o.OrderDate) = 2
    group by EmployeeID
)
select FirstName, LastName, orderCount, orderPriceSum
from oCount oc
inner join oPrice op
on oc.EmployeeID = op.EmployeeID
union
select FirstName, LastName, 0, 0
from Employees
where EmployeeID not in (select EmployeeID from oCount)

-- 3.
use library;
select firstname, lastname, street, city, state, zip
from member m
inner join juvenile j
on m.member_no = j.member_no
inner join adult a
on j.adult_member_no = a.member_no
inner join loanhist lh
on m.member_no = lh.member_no
inner join title t
on lh.title_no = t.title_no
where title = 'Walking' and in_date >= '2001-12-14' and in_date < '2001-12-15'
group by m.member_no, firstname, lastname, street, city, state, zip