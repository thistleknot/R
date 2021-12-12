library("DiagTest3Grp", lib.loc="~/R/win-library/3.2")

data <- c(34,46,47,48,52,53,55,56,56,56,57,58,59,59,68)
bw <- BW.ref(data)
x0 <- seq(0, 100, .1) 
KS.cdfvec <- Vectorize(KernelSmoothing.cdf, vectorize.args = "c0")
x0.cdf <- KS.cdfvec(xx = data, c0 = x0, bw = bw)
plot(x0, x0.cdf, type = "l")

xs <- c()
ys <- c()

front <- data[1]
back <- tail(data, n=1)

frontlist <- approx(x=x0, y=x0.cdf, xout=front)
backlist <- approx(x=x0, y=x0.cdf, xout=back)

#frontlist <- approx(x=x0, y=x0.cdf, xout=0)
#backlist <- approx(x=x0, y=x0.cdf, xout=100)

xwidth = back - front
#xwidth = 100

percentWidth = backlist$y - frontlist$y

percentWidth

arcFactor = 4

arcFactoring = (length(data)*arcFactor)

addwidth = percentWidth/(arcFactoring)

for (i in 0:arcFactoring)
{
  xylist <- approx(y=x0, x=x0.cdf, xout=frontlist$y+(addwidth*i))
  xs <- c(xs, xylist$x)
  ys <- c(ys, xylist$y)
}

arcLengths <- c()

topOfX = (length(xs)-1)

for (i in 1:topOfX) {
  arcLengths <- c(arcLengths, sqrt((xs[i+1]-xs[i])^2+(ys[i+1]-ys[i])^2))
}

sumOfArcs = sum(arcLengths)

arcPercents <- c(0)
runningSum = 0

for (i in 1:topOfX) {
  runningSum = runningSum + arcLengths[i]
  arcPercents <- c(arcPercents, runningSum / sumOfArcs)  
  #print(runningSum)
}
