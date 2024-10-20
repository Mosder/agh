# 1.
data1 = boot::acme$market
data1partsAmount = as.integer(sqrt(length(data1)))
packetData1 = cut(data1, data1partsAmount)

# 2.
acmeData = boot::acme
acmeData$year =
  matrix(unlist(strsplit(acmeData$month, "/")), ncol=2, byrow=TRUE)[,2]
acmeData = acmeData[order(acmeData$year),]
acmeDistribution = table(acmeData$year)
meanFrame = data.frame(Year=c(), YearMean=c())
index = 1
row = 0
for (amount in acmeDistribution) {
  row = row + 1
  year = names(acmeDistribution)[row]
  endIndex = index + acmeDistribution[row] - 1
  mean1 = mean(acmeData$market[index:endIndex])
  index = endIndex + 1
  meanFrame = rbind(meanFrame, c(year, mean1))
}

# 3.
splitByVector = function(arrToSplit, vect) {
  if (sum(vect) != 1) return(FALSE)
  amountPer = as.integer(round(vect * length(arrToSplit)))
  index = 1
  list1 = list()
  for (am in amountPer) {
    endIndex = index + am - 1
    list1 = c(list1, list(arrToSplit[index:endIndex]))
    index = endIndex + 1
  }
  return(list1)
}
result3 = splitByVector(c(1,2,3,4,5,6,7,8,9,0), c(.3,.3,.3,.1))
print(result3)


# 4.
summary(boot::catsM)
