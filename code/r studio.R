install.packages("DiagTest3Grp")
#library("DiagTest3Grp", lib.loc="~/R/win-library/3.4")

data <- c(34,46,47,48,52,53,55,56,56,56,57,58,59,59,68)
bw <- BW.ref(data)
x0 <- seq(0, 100, .1) 
KS.cdfvec <- Vectorize(KernelSmoothing.cdf, vectorize.args = "c0")
x0.cdf <- KS.cdfvec(xx = data, c0 = x0, bw = bw)
plot(x0, x0.cdf, type = "l")

ys <- c()

front <- data[1]
back <- tail(data, n=1)

for (i in 1:length(data))
{
  xylist <- approx(x=x0, y=x0.cdf, xout=data[i])
  ys <- c(ys, xylist$y)
}

ys

plot(ys)