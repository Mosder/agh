select CompanyName, Address, City
from Customers
left join Orders on Customers.CustomerID = Orders.CustomerID and year(OrderDate) = 1997
where OrderID is null

select firstname, lastname, street, city
from juvenile
inner join member
on juvenile.member_no = member.member_no
inner join adult
on juvenile.adult_member_no = adult.member_no

select e.FirstName, e.LastName, b.FirstName as BossFirstName, b.LastName as BossLastName
from Employees as e
inner join Employees as b
on e.ReportsTo = b.EmployeeID