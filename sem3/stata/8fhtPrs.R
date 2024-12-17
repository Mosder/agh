prob = function(S, D, N) {
  return(((S[2] - S[1]) / (D[2] - D[1])) ^ N)
}

expectedFht = function(S, D, N) {
  return(1/prob(S,D,N))
}

maclaurinLog = function(p, n=100) {
  sum = 0
  for (i in 1:n) sum = sum + p^i / i
  return(-sum)
}

stopCondition = function(S, D, N, delta) {
  logFunction = if (N > 16) maclaurinLog else log
  if (N <= 16)
    return(ceiling(log(1-delta)/log(1-prob(S,D,N))))
  else
    return(ceiling(log(1-delta)/maclaurinLog(prob(S,D,N))))
}

S = c(0,0.1)
D = c(0,1)
deltas = c(0.6, 0.95)
Ns = c(1,2,5,10,50,100)
frame = data.frame()
print(frame)
for (N in Ns) {
  for (delta in deltas) {
    frame = rbind(frame, c(N, delta, expectedFht(S,D,N),
                           stopCondition(S,D,N,delta)))
  }
}
colnames(frame) = c('N', 'delta', 'expected FHT', 'stop condition')
print(frame)