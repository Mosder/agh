-- Ćwiczenie 1.
-- Napisz polecenie select  za pomocą którego uzyskasz identyfikator/numer tytułu oraz tytuł książki
select title_no, title
from title
--  Napisz polecenie, które wybiera tytuł o numerze/identyfikatorze 10
select title
from title
where title_no = 10
-- Napisz polecenie select, za pomocą którego uzyskasz numer książki (nr tyułu)
-- i autora dla wszystkich książek, których autorem jest Charles Dickens lub Jane Austen
select title_no, author
from title
where author in ('Charles Dickens', 'Jane Austen')


-- Ćwiczenie 2.
-- Napisz polecenie, które wybiera numer tytułu i tytuł dla wszystkich  książek,
-- których tytuły zawierających słowo 'adventure'
select title_no, title
from title
where ' ' + title + ' ' like '% adventure %'
-- Napisz polecenie, które wybiera numer czytelnika,
-- oraz zapłaconą karę dla wszystkich książek, tore zostały zwrócone w listopadzie 2001
select member_no, fine_paid
from loanhist
where in_date >= '2001-11-01' and in_date < '2001-12-01'
-- Napisz polecenie, które wybiera wszystkie unikalne pary miast i stanów z tablicy adult.
select distinct state,  city
from adult
-- Napisz polecenie, które wybiera wszystkie tytuły z tablicy title i wyświetla je w porządku alfabetycznym.
select title
from title
order by title


--Ćwiczenie 3.
-- wybiera numer członka biblioteki (member_no), isbn książki (isbn)
-- i wartość naliczonej kary (fine_assessed) z tablicy loanhist  dla wszystkich wypożyczeń/zwrotów,
-- dla których naliczono karę (wartość nie NULL w kolumnie fine_assessed)
select member_no, isbn, fine_assessed
from loanhist
where fine_assessed is not null
-- stwórz kolumnę wyliczeniową zawierającą podwojoną wartość kolumny fine_assessed
select member_no, isbn, fine_assessed, fine_assessed * 2
from loanhist
where fine_assessed is not null
-- stwórz alias double_fine dla tej kolumny (zmień nazwą kolumny na double_fine)
select member_no, isbn, fine_assessed, fine_assessed * 2 as double_fine
from loanhist
where fine_assessed is not null
-- stwórz kolumnę o nazwie diff, zawierającą różnicę wartości w kolumnach double_fine i fine_assessed
select member_no, isbn, fine_assessed, fine_assessed * 2 as double_fine, fine_assessed as diff
from loanhist
where fine_assessed is not null
-- wybierz wiersze dla których wartość w kolumnie diff jest większa niż 3
select member_no, isbn, fine_assessed, fine_assessed * 2 as double_fine, fine_assessed as diff
from loanhist
where fine_assessed is not null and fine_assessed > 3


-- Ćwiczenie 4.
-- generuje pojedynczą kolumnę, która zawiera kolumny: firstname (imię członka biblioteki),
-- middleinitial (inicjał drugiego imienia) i lastname (nazwisko) z tablicy member
-- dla wszystkich członków biblioteki, którzy nazywają się Anderson
select firstname + ' ' + middleinitial + ' ' + lastname
from member
where lastname = 'Anderson'
-- nazwij tak powstałą kolumnę email_name (użyj aliasu email_name dla kolumny)
select firstname + ' ' + middleinitial + ' ' + lastname as email_name
from member
where lastname = 'Anderson'
-- zmodyfikuj polecenie, tak by zwróciło 'listę proponowanych loginów e-mail'
-- utworzonych przez połączenie imienia członka biblioteki,
-- z inicjałem drugiego imienia i pierwszymi dwoma literami nazwiska (wszystko małymi małymi literami).
select replace(lower(firstname + middleinitial + substring(lastname, 1, 2)), ' ', '') as email_name
from member
where lastname = 'Anderson'


-- Ćwiczenie 5.
-- Napisz polecenie, które wybiera title i title_no z tablicy title.
-- wynikiem powinna być pojedyncza kolumna o formacie jak w przykładzie poniżej:
-- The title is: Poems, title number 7
select 'The title is: ' + title + ', title number ' + cast(title_no as varchar) as title_info
from title
