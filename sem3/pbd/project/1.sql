-- 5.
select ProductName, UnitsInStock from Products p
    inner join Suppliers s on p.SupplierID = s.SupplierID