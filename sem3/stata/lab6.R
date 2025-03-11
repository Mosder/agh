prnt = function(...) {
  print(paste(...))
}

# 1.
mean = 33
st_dev = 8
n = 16
x_mean = 35
mean_st_dev = st_dev / sqrt(n)
probability = pnorm(x_mean, mean=mean, sd=mean_st_dev)
prnt("Prawdopodobieństwo wynosi:", probability)

# 2.
stdev = 0.2
n = 25
x_mean = 3.15
trust = 0.95
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
margin = zalpha * (stdev / sqrt(n))
lower_bound = x_mean - margin
upper_bound = x_mean + margin
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 3.
s = 11
n = 25
x_mean = 45
trust = 0.95
talpha = qt(1- (1-trust)/2, df=n-1, lower.tail=TRUE, log.p=FALSE)
margin = talpha * s / sqrt(n-1)
lower_bound = x_mean - margin
upper_bound = x_mean + margin
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 4.
s_hat = 1.7
n = 100
x_mean = 5.4
trust = 0.96
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
margin = zalpha * s_hat / sqrt(n)
lower_bound = x_mean - margin
upper_bound = x_mean + margin
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 5.
s_squared = 13.5
n = 25
x_mean = 37.3
trust = 0.9
chji1 = qchisq((1-trust)/2, df=n-1, lower.tail=TRUE, log.p=FALSE)
chji2 = qchisq(1 - (1-trust)/2, df=n-1, lower.tail=TRUE, log.p=FALSE)
lower_bound = n * s_squared / chji2
upper_bound = n * s_squared / chji1
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 6.
n = 450
stdev = 0.8
trust = 0.99
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
lower_bound = stdev / (1 + zalpha / sqrt(2*n))
upper_bound = stdev / (1 - zalpha / sqrt(2*n))
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 7.
n = 200
m = 20
p_mean = m / n
trust = 0.9
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
margin = zalpha * sqrt((p_mean * (1 - p_mean))/n)
lower_bound = p_mean - margin
upper_bound = p_mean + margin
prnt("Przedział ufności: (", lower_bound, ",", upper_bound, ")")

# 8.
stdev = 1.5
max_error = 0.5
trust = 0.99
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
min_sample_count = ceiling(zalpha^2 * stdev^2 / max_error^2)
prnt("Minimalna liczebność próby:", min_sample_count)
PASWR::nsize(max_error, sigma=stdev, conf.level=trust, type="mu")

# 9.
max_error = 5
trust = 0.95
n0 = 10
x_mean = 142
s_hat_squared = 169
talpha = qt(1- (1-trust)/2, df=n0-1, lower.tail=TRUE, log.p=FALSE)
n = ceiling(talpha^2 * s_hat_squared / max_error^2)
prnt("Minimalna liczebność próby:", n)
PASWR::nsize(max_error, sigma=sqrt(s_hat_squared), conf.level=trust, type="mu")

# 10.
trust = 0.95
proportion = 0.0045
max_error = 0.01
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
min_sample_count = ceiling(zalpha^2 * proportion * (1-proportion) / max_error^2)
prnt("Minimalna liczebność próby:", min_sample_count)
PASWR::nsize(max_error, p=proportion, conf.level=trust, type="pi")

# 11.
trust = 0.90
proportion = 0.5
max_error = 0.08
zalpha = qnorm(1 - (1-trust) / 2, mean = 0, sd = 1,
               lower.tail=TRUE, log.p=FALSE)
min_sample_count = ceiling(zalpha^2 * proportion * (1-proportion) / max_error^2)
prnt("Minimalna liczebność próby:", min_sample_count)
PASWR::nsize(max_error, p=proportion, conf.level=trust, type="pi")
