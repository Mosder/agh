# 1.
da = boot::acme
boxplot(da$market)
boxplot(da$acme)
# 2.
vioplot(da$market)
ggplot2::geom_violin(da$market)
