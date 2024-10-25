library(vioplot)
library(carData)
library(car)

# 1.
da = boot::acme
boxplot(da$market)
boxplot(da$acme)

# 2.
vioplot(da$market)
vioplot(da$acme)

# 3.
scatterplot(da$market, da$acme)
plot(da$market, da$acme)

# 4.
x = seq(0, 10, .01)
normalDist = dnorm(x, 0, 3)
fDist = df(x, 3, 6)
chiSquare = dchisq(x, 3)
minY = min(normalDist, fDist, chiSquare)
maxY = max(normalDist, fDist, chiSquare)
plot(1, type = "n", xlim = c(0, 10), ylim = c(minY, maxY),
     xlab = "x axis", ylab = "y axis")
lines(x, normalDist, col = "red")
lines(x, fDist, col = "blue")
lines(x, chiSquare, col = "green")
legend("topright", legend = c("N(0,3)", "F(3,6)", "chisq(3)"), 
       col = c("red", "blue", "green"), lty = 1)

