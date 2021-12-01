sdlog <- 0.2
n <- 1000

x <- seq(0, qlnorm(0.99, sdlog=sdlog), len=n)
f <- dlnorm(x, sdlog=sdlog)
F <- plnorm(x, sdlog=sdlog)
y <- F/f
delta <- x + y
plot(x, delta)
summary(delta)
ddelta <- diff(delta)/diff(x)
plot(x[-1], ddelta)
summary(ddelta)

d <- -F*(log(x) + sdlog^2)/(f*x*sdlog^2)
plot(d[-1], ddelta)
# df <- diff(f) / diff(x)
# ddlnorm <- function(x, meanlog=0, sdlog=1) {
#   y <- 1/x + (log(x) - meanlog)/(x*sdlog^2)
#   return(-dlnorm(x, meanlog=meanlog, sdlog=sdlog)*y)
# }
# df0 <- ddlnorm(x, sdlog=0.3)
# plot(df, df0[-1])

bracket <- (log(x) - (meanlog <- 0))/(x*sdlog^2)


# f <- function(x, meanlog=0, sdlog=1) {
#   part1 <- 1/(x * sdlog * sqrt(2*pi))
#   part2 <- exp(-(log(x) - meanlog)^2 / (2*sdlog^2))
#   return(part1 * part2)
# }
# y2 <- f(x, sdlog=sdlog)
# plot(dlnorm(x, sdlog=sdlog), y2)

# plot(x, y, ylim=c(0,2))
dy_dx <- diff(y)/diff(x)
plot(x[-length(x)], dy_dx, log="y")
