bernoulliFrame = data.frame()

calcProb = function(probs, amounts) {
  factProd = 1
  probProd = 1
  n = 0
  for (i in 1:(length(probs))) {
    probProd = probProd * probs[i]^(amounts[i])
    factProd = factProd * factorial(amounts[i])
    n = n + amounts[i]
  }
  return(factorial(n)*probProd/factProd)
}

bernoulliHelpFunction = function(probs, currRes, remainingN, row) {
  if (currRes == length(probs) - 1) {
    row = append(row, remainingN)
    prob = calcProb(probs, row)
    row = append(row, prob)
    bernoulliFrame <<- rbind(bernoulliFrame, row)
    return()
  }
  for (i in 0:remainingN) {
    row1 = append(row, i)
    bernoulliHelpFunction(probs, currRes+1, remainingN-i, row1)
  }
}

bernoulliGeneralized = function(names, probs, n) {
  bernoulliHelpFunction(probs, 0, n, c())
  colnames(bernoulliFrame) <<- append(names, "probability")
  return(bernoulliFrame)
}

names = c("jabłko", "gruszka", "śliwka")
probs = c(0.3, 0.2, 0.5)
n = 5
bernoulliGeneralized(names, probs, n)
print(bernoulliFrame)
