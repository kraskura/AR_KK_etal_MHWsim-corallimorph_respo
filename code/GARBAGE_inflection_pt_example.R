
# Install and load necessary libraries
install.packages("changepoint")
install.packages("pracma")
library(changepoint)

# Generate a sample time series with inflection points
set.seed(123)
time <- 1:100
value <- c(rnorm(20, mean = 0, sd = 1), 
            rnorm(30, mean = 5, sd = 1), 
            rnorm(20, mean = 10, sd = 1), 
            rnorm(30, mean = 15, sd = 1))
df <- data.frame(time, value)

# Plot the time series
plot(df$time, df$value, type = "l", 
     main = "Time Series with Inflection Points", 
     xlab = "Time", ylab = "Value")

# Detect inflection points using cpt.mean function
cpt <- cpt.mean(df$value, 
                 method = "PELT", 
                 pen.value = 0.05)

# Print the inflection points
as.numeric(cpts(cpt))


# Plot the inflection points
abline(v = df$time[cpt$cps], col = "red", lwd = 2)
legend("topright", c("Inflection Points"), 
       lty = 1, col = c("red"), lwd = 2)


# aproach 2 -------
if(!require('pracma')) {
  install.packages('pracma')
  library('pracma')
}

x <- seq(0, 1, len = 1024)
pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.40, 0.44, 0.65, 0.76, 0.78, 0.81)
hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
wdt <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 0.005)

pSignal <- numeric(length(x))
for (i in seq(along=pos)) {
	pSignal <- pSignal + hgt[i]/(1 + abs((x - pos[i])/wdt[i]))^4
}
findpeaks(pSignal, npeaks=3, threshold=4, sortstr=TRUE)

## Not run: 
# plot(pSignal, type="l", col="navy")
# grid()
# x <- findpeaks(pSignal, npeaks=3, threshold=4, sortstr=TRUE)
# points(x[, 2], x[, 1], pch=20, col="maroon")## End(Not run)



