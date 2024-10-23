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

-- Ćwiczenie 1.
-- Dla każdego zamówienia podaj jego wartość. Posortuj wynik wg wartości zamówień (w malejęcej kolejności)
select OrderID, sum(UnitPrice * Quantity * (1-Discount)) as totalValue
from [Order Details]
group by OrderID
order by totalValue desc
-- Zmodyfikuj zapytanie z poprzedniego punktu, tak aby zwracało tylko pierwszych 10 wierszy
select top 10 OrderID, sum(UnitPrice * Quantity * (1-Discount)) as totalValue
from [Order Details]
group by OrderID
order by totalValue desc
-- Podaj  nr zamówienia oraz wartość  zamówienia, dla zamówień, dla których łączna liczba
-- zamawianych jednostek produktów jest większa niż 250
select OrderID, sum(UnitPrice * Quantity * (1-Discount)) as totalValue
from [Order Details]
group by OrderID
having sum(Quantity) > 250
-- Podaj liczbę zamówionych jednostek produktów dla  produktów, dla których productid jest mniejszy niż 3
select Products.ProductID, Products.ProductName, sum([Order Details].Quantity) as totalOrderAmount
from [Order Details]
inner join Products
on [Order Details].ProductID = Products.ProductID
where Products.ProductID < 3
group by Products.ProductID, Products.ProductName


-- Ćwiczenie 2.
-- Ilu jest dorosłych czytekników
select count(*) as adultAmount
from adult
-- Ile jest dzieci zapisanych do biblioteki
select count(*) as childAmount
from juvenile
-- Ilu z dorosłych czytelników mieszka w Kaliforni (CA)
select count(*) as caliAdultsAmount
from adult
where state = 'CA'
-- Dla każdego dorosłego czytelnika podaj liczbę jego dzieci.
select juvenile.adult_member_no, member.firstname + ' ' + member.lastname as name,
       count(juvenile.member_no) as childrenAmount
from member
inner join juvenile
on member.member_no = juvenile.adult_member_no
group by juvenile.adult_member_no, member.lastname, member.firstname
-- Dla każdego dorosłego czytelnika podaj liczbę jego dzieci urodzonych przed 1998r
select juvenile.adult_member_no, member.firstname + ' ' + member.lastname as name,
       count(juvenile.member_no) as childrenAmount
from member
inner join juvenile
on member.member_no = juvenile.adult_member_no
where year(juvenile.birth_date) < 1998
group by juvenile.adult_member_no, member.lastname, member.firstname


-- Ćwiczenie 3.
-- Dla każdego czytelnika podaj liczbę zarezerwowanych przez niego książek
select reservation.member_no, member.firstname + ' ' + member.lastname as name,
       count(reservation.isbn) as reservationAmount
from reservation
inner join member
on reservation.member_no = member.member_no
group by reservation.member_no, member.firstname, member.lastname
-- Dla każdego czytelnika podaj liczbę wypożyczonych przez niego książek
select loan.member_no, member.firstname + ' ' + member.lastname as name,
       count(loan.isbn) as loanAmount
from loan
inner join member
on loan.member_no = member.member_no
group by loan.member_no, member.firstname, member.lastname
-- Dla każdego czytelnika podaj liczbę książek zwróconych przez niego w 2001r.
select loanhist.member_no, member.firstname + ' ' + member.lastname as name,
       count(loanhist.isbn)
from loanhist
left join member
on loanhist.member_no = member.member_no
where year(loanhist.in_date) = 2001
group by loanhist.member_no, member.firstname, member.lastname
-- Dla każdego czytelnika podaj sumę kar jakie zapłacił w 2001r
select loanhist.member_no, member.firstname + ' ' + member.lastname as name,
       iif(sum(loanhist.fine_paid) is null, 0, sum(loanhist.fine_paid)) as fineTotal
from loanhist
left join member
on loanhist.member_no = member.member_no
where year(in_date) = 2001
group by loanhist.member_no, member.firstname, member.lastname
-- Ile książek wypożyczono w maju 2001
select count(*) as '05/2001 loans'
from loanhist
where out_date >= '2001-05-01' and out_date < '2001-06-01'
-- Na jak długo średnio były wypożyczane książki w maju 2001
select avg(datediff(day, in_date, out_date)) as avgDayAmount
from loanhist
where out_date >= '2001-05-01' and out_date < '2001-06-01'


-- Ćwiczenie 4.
-- Dla każdego pracownika podaj liczbę obsługiwanych przez niego zamówień w 1997r
select Orders.EmployeeID, Employees.FirstName + ' ' + Employees.LastName as name,
       count(Orders.OrderID) as orderAmount
from Orders
left join Employees
on Orders.EmployeeID = Employees.EmployeeID
group by Orders.EmployeeID, Employees.FirstName, Employees.LastName
-- Dla każdego pracownika podaj ilu klientów (różnych klientów) obsługiwał ten pracownik w 1997r
select Orders.EmployeeID, Employees.FirstName + ' ' + Employees.LastName as name,
       count(distinct CustomerID) as orderAmount
from Orders
left join Employees
on Orders.EmployeeID = Employees.EmployeeID
group by Orders.EmployeeID, Employees.FirstName, Employees.LastName
-- Dla każdego spedytora/przewoźnika podaj łączną wartość "opłat za przesyłkę" dla przewożonych przez niego zamówień
select Orders.ShipVia, Shippers.CompanyName, sum(Orders.Freight) as totalFreight
from Orders
left join Shippers
on Orders.ShipVia = Shippers.ShipperID
group by Orders.ShipVia, Shippers.CompanyName
-- Dla każdego spedytora/przewoźnika podaj łączną wartość "opłat za przesyłkę"
-- przewożonych przez niego zamówień w latach od 1996 do 1997
select Orders.ShipVia, Shippers.CompanyName, sum(Orders.Freight) as totalFreight
from Orders
left join Shippers
on Orders.ShipVia = Shippers.ShipperID
where year(Orders.ShippedDate) >= 1996 and year(Orders.ShippedDate) <= 1997
group by Orders.ShipVia, Shippers.CompanyName


-- Ćwiczenie 5.
-- Dla każdego pracownika podaj liczbę obsługiwanych przez niego zamówień z podziałem na lata
select Orders.EmployeeID, Employees.FirstName + ' ' + Employees.LastName as name,
       year(Orders.OrderDate) as year, count(OrderID) as orderAmount
from Orders
left join Employees
on Orders.EmployeeID = Employees.EmployeeID
group by Orders.EmployeeID, Employees.FirstName, Employees.LastName, rollup(year(Orders.OrderDate))
order by Orders.EmployeeID, year(Orders.OrderDate)
-- Dla każdego pracownika podaj liczbę obsługiwanych przez niego zamówień z podziałem na lata i miesiące.
select Orders.EmployeeID, Employees.FirstName + ' ' + Employees.LastName as name,
       year(Orders.OrderDate) as year, month(Orders.OrderDate) as month, count(OrderID) as orderAmount
from Orders
left join Employees
on Orders.EmployeeID = Employees.EmployeeID
group by Orders.EmployeeID, Employees.FirstName, Employees.LastName,
         cube(year(Orders.OrderDate), month(Orders.OrderDate))
order by Orders.EmployeeID, year(Orders.OrderDate), month(Orders.OrderDate)

