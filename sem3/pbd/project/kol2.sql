-- 1.
use Northwind;
with oCounts as (
    select e.EmployeeID, FirstName, LastName, count(OrderID) as orderCount, max(OrderDate) as latestOrder
    from Employees e
    inner join Orders o
    on e.EmployeeID = o.EmployeeID
    where year(OrderDate) = 1997 and month(OrderDate) = 3
    group by e.EmployeeID, FirstName, LastName
),
oPriceSums as (
    select EmployeeID, sum(UnitPrice * Quantity * (1-Discount)) as ordersPriceSum
    from Orders o
    inner join [Order Details] od
    on o.OrderID = od.OrderID
    where year(OrderDate) = 1997 and month(OrderDate) = 3
    group by EmployeeID
)
select FirstName, LastName, orderCount, ordersPriceSum, latestOrder
from oCounts oc
inner join oPriceSums ops
on oc.EmployeeID = ops.EmployeeID
union
select FirstName, LastName, 0, 0, null
from Employees
where EmployeeID not in (select EmployeeID from oCounts)

-- 2.
use library;
select firstname, lastname, street, city, state, zip
from member m
inner join juvenile j
on m.member_no = j.member_no
inner join adult a
on j.adult_member_no = a.member_no
where m.member_no not in
(
    select member_no
    from loanhist lh
    inner join title t
    on lh.title_no = t.title_no
    where in_date >= '2001-12-14' and in_date < '2001-12-15' and title = 'Walking'
)

-- 3.
use Northwind;
with orders1997 as (
    select c.CustomerID, CompanyName, e.EmployeeID, FirstName, LastName, count(OrderID) as orderCount
    from Customers c
    inner join Orders o
    on c.CustomerID = o.CustomerID
    inner join Employees e
    on o.EmployeeID = e.EmployeeID
    where year(OrderDate) = 1997
    group by c.CustomerID, CompanyName, e.EmployeeID, FirstName, LastName
),
maxOrders as (
    select CustomerID, max(orderCount) as maxOrders
    from orders1997
    group by CustomerID
)
select CompanyName, FirstName, LastName, orderCount
from orders1997 o
inner join maxOrders m
on m.CustomerID = o.CustomerID and maxOrders = orderCount
order by m.CustomerID