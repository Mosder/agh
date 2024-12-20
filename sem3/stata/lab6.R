# 1.
m = 33
sigma = 8
n = 16
x_mean = 35
new_sigma = sigma / sqrt(n)
z = (x_mean - m) / new_sigma
probability = pnorm(z)
print(probability)

# 2.
stdev = 0.2
n = 25
x_mean = 3.15
trust = 0.95
zalpha <- qnorm(1 - (1-trust) / 2, mean = 0, sd = 1)
margin = zalpha * (stdev / sqrt(n))
lower_bound = x_mean - margin
upper_bound = x_mean + margin
print(lower_bound)
print(upper_bound)

# 3.
s = 11
n = 25
x_mean = 45
trust = 0.95
talpha = qt(1- (1-trust)/2, df=n-1)
margin = talpha * s / sqrt(n-1)
lower_bound = x_mean - margin
upper_bound = x_mean + margin
print(lower_bound)
print(upper_bound)

# 4.
s_hat = 1.7
n = 100
x_mean = 5.4
trust = 0.96
zalpha <- qnorm(1 - (1-trust) / 2, mean = 0, sd = 1)
margin = zalpha * s_hat / sqrt(n)
lower_bound = x_mean - margin
upper_bound = x_mean + margin
print(lower_bound)
print(upper_bound)

# 5.
s_squared = 13.5
n = 25
x_mean = 37.3
trust = 0.9
zalpha <- qnorm(1 - (1-trust) / 2, mean = 0, sd = 1)
margin = zalpha * (stdev / sqrt(n))
lower_bound = x_mean - margin
upper_bound = x_mean + margin
print(lower_bound)
print(upper_bound)
