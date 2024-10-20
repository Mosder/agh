# 1.
# a.
x = runif(10, 0, 10)
y = runif(10, 10, 20)
A = matrix(c(x,y), nrow=10, ncol=2)
At = t(A)
result = At%*%y
# b.
A = matrix(runif(9,0,10), nrow=3, ncol=3)
for (i in 1:3) A[i,i] = runif(1, 20, 30)
b = runif(3, 0, 10)
detA = det(A)
matrixEqSolution = solve(A, b)
# c.
c = runif(3, 0, 10)
B = cbind(A, c)
d = runif(4, 0, 10)
G = rbind(B, d)
# d.
dimnames(G) = list(c("Ala", "Renia", "Magda", "Maria"),
                   c("Róża", "Słonecznik", "Tulipan", "Niezapominajka"))
print(dim(G))
# e.
namesM = list(c("row1", "row2", "row3"), c("col1", "col2", "col3"))
M = matrix(runif(9, 0, 10), nrow=3, ncol=3, dimnames=namesM)
# f.
tab31 = array(x, c(3, 3, 2))
temp = runif(18, 0, 10)
dim(temp) <- c(3, 3, 2)


# 2.
# a.
list1 = list("strings"=c("string1", "string2", "string3"),
             "matrixNum"=matrix(runif(9,0,10), nrow=3, ncol=3),
             "matrixBool"=matrix(sample(c(TRUE,FALSE), 9, replace=TRUE), nrow=3, ncol=3))
print(list1)
print(typeof(list1))
squareRootMatrix = sqrt(list1$matrixNum)
# b.
smoking = sample(c(TRUE,FALSE), 10, replace=TRUE)
sex = sample(c("K", "M"), 10, replace=TRUE)
age = sample(1:100, 10, replace=TRUE)
badanie = data.frame(czy_pali=smoking, plec=sex, wiek=age)
for (colName in colnames(badanie))
  if (is.numeric(badanie[[colName]])) print(paste("Liczbowa kolumna: ", colName))
print(badanie)
print(paste("K - ", sum(sex == "K"), ", M - ", sum(sex == "M"), sep=""))


# 3.
# a.
write.table(badanie, file="badanie")
print(badanie)
Nowe_badanie = read.table("badanie")
print(Nowe_badanie)
#b.
write.table(boot::beaver, "beaverData")


# 4.
beaverData = read.table("beaverData")
scalar = 0
# a.
for (i in 1:length(beaverData$temp))
  scalar = scalar + beaverData$temp[i] * beaverData$activ[i]
# b.
zerosInVector = function(v) {
  return(sum(v == 0))
}
# c.
firstAndLastTrue = function(v) {
  last = FALSE
  first = FALSE
  for (i in 1:length(v)) {
    if (v[i]) {
      if (!first) first = i
      last = i
    }
  }
  if (!last) return(c(NA,NA))
  return(c(first, last))
}
# d.
moda = function(x) {
  tab = table(x)
  print(names(tab[which(tab == max(tab))]))
}
modaArray = array(sample(101:104, 27, replace=TRUE), c(3,3,3))
moda(modaArray)
