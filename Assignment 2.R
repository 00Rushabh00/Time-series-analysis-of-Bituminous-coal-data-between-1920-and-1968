load("/Users/rushabhkhara/Files/University/STAT3011/Data/classdata.RData")
library(MASS)

# Data

coal <- bicoal.tons
#Checking if it is a time series object

ts(coal)

# Time Series:
# Start = 1 
# End = 49 
# Frequency = 1 
# [1] 569 416 422 565 484 520 573 518 501 505 468 382 310 334 359 372 439 446 349 395
# [21] 461 511 583 590 620 578 534 631 600 438 516 534 467 457 392 467 500 493 410 412
# [41] 416 403 422 459 467 512 534 552 545

# Plotting the data

plot(coal,type="b",pch=1,main="Bitumous Coal Data",ylab="Production in 
Millions of Net Tons",xlab="Time")

# The variance looks quite stable and constant on the original scale.

# Transformation

# The data doesn't seem to have heteroscedasticity problem. However, it wouldn't
# harm to check for transformations to see if the data can have more constant
# variance.

# Plotting transformations

# Square Root
plot(sqrt(coal),type="b",pch=1,main="Square Root Transformation",
ylab="Production in Millions of Net Tons",xlab="Time")

# Log 
plot(log(coal),type="b",pch=1,main="Log Transformation",ylab="Production in 
Millions of Net Tons",xlab="Time")

# It is very evident looking at both the transformation plots that the variance
# remains noticably same. This means, we should proceed without transformation
# of the raw data. Transformation will add complexity to model without any
# significant improvement.

# Adding lowess curve to the trend.

plot(coal,type="b",pch=1,main="Bitumous Coal Data",ylab="Production in 
Millions of Net Tons",xlab="Time")
lines(lowess(time(coal),coal))

# We can notice that the lowess fit shows a cyclic effect but is fairly linear. 
# There is no point assuming their is a trend without checking by fitting models
# as the lowess curve might appear significantly non-linear or significantly 
# linear based on shaped and size of the plot.

# Let's now try to fit rlm models

# We will fit four trend models i.e no trend, linear, quadratic and cubic

# The data has 2 peaks which cannot be captured by the quadratic model and hence
# we are not going to plot it.

# Firstly, lets centre the time so their is no computational error when fitting
# the cubic model

x <- time(coal) - 1920

plot(coal, type="b", pch=1,
     main="Plot of Bitumous Coal Production between 1920 and 1968",
     ylab="Production in Millions of Net Tons",
     xlab="Time")

# Add a LOWESS line
lines(lowess(time(coal), coal), col="black")

# Add a horizontal line at the mean of the data
abline(h = mean(coal), col = "darkorange")

# Add a linear trend line
fit_lin <- rlm(coal ~ time(coal))
abline(fit_lin, col="darkgreen")

# Add a quadratic trend line
fit_quad <- rlm(coal ~ cbind(time(coal),time(coal)^2))
lines(as.vector(time(coal)),fitted(fit_quad),col="red")

# Add a cubic trend line
fit_cubic <- rlm(coal~cbind(time(coal),time(coal)^2,time(coal)^3)) 
lines(as.vector(time(coal)),fitted(fit_cubic), col = "blue")

# Add a legend
legend("topleft", 
       legend = c("LOWESS", "No Trend", "Linear", "Quadratic", "Cubic"),
       col = c("black", "darkorange", "darkgreen", "red", "blue"),
       lty = c(1, 1, 1, 1, 1),
       merge = TRUE,
       cex = 0.8)

# Observations from the plot:
# - Both the no trend model and the linear model exhibit similar patterns.
# - The quadratic model is limited in its ability to capture trends, as it can curve only once.
# - The cubic model identifies the initial drop but lags significantly in capturing subsequent rises.
# - Due to this lag, the cubic model isn't well-suited for this data.
# - Given the similarities in fit between the no trend and linear models, we prioritize simplicity.
# - Therefore, the no trend model is chosen.
# - It's important to note that the data window is too limited to infer periodic or seasonal effects.

# Given the time span it is very difficult to notice any seasonal effects.
# Even though we see a wave effect, we do not know whether it is occuring in
# regular intervals or not and making any assumptions outside the data time
# span is never recommended. This is why we will ignore the seasonal effect. 
# STL cannot be used as frequency is 1 and hence it would not be feasible.

# Irregular Effects

# As we have chosen the no trend model, we will calculate the residuals by 
# substracting the mean from each data point.

fit = mean(coal)

res = coal - fit

# ACF and PACF
Ident(res)

final_model <- Raic(res)

# It can be seen that the ACF slowly decays which indicates problem with 
# stationarity of the data. We might consider differencing the data to see if
# the problem can be rectified.

fit_diff = mean(diff(coal))

res_diff = diff(coal) - fit_diff

#ACF and PACF
Ident(res_diff)

final_model <- Raic(res_diff)

# As we can see the stationairity problem has been rectified by differencing
# the data. The PACF plot now suggest an AR(2) model.

res = final_model$resid[,2]
fv = rep(mean(diff(coal)), length(diff(coal)) - 1)

Ident(res)

# The AR(2) models seems like a perfect fit for the data provided. We see no
# significant spikes in PACF plot and only one spike at lag 2 which is 
# expected. The ACF/PACF shows no remaining dependence. Overall, it looks as if 
# an AR(2) captured most of the dependence.

# Diagnostic Plots

par(mfrow=c(2,2),oma=c(0,0,6,0))

plot(1:(length(diff(coal))-1),res,type="b",pch=1,xlab="Time",ylab="Residuals",
     main="Residual plot")

plot(fv,res,
     main="Residuals versus fitted values",
     ylab="Residuals",xlab="Fitted Values")

par(pty="s")
qqnorm(res,main="Quantile-Quantile plot",
       ylab="Residuals",xlab="Gaussian Quantiles")
qqline(res);abline(h=0,v=0,col="grey")
par(pty="m")

plot(fv,abs(res),
     main="Absolute Residuals versus fitted values",
     ylab="Absolute Residuals", xlab="Fitted Values")
lines(lowess(fv,abs(res)))

mtext("Residual plots from no trend and AR(2) fit to the bitumous coal data",
      outer=T,side=3,cex=1.5)

# Co-efficients of AR(2)
final_model$coef[1:2,2]

# Final Model

coal_diff = mean(coal_diff) + X_i

X_i = -0.136 * X_i{i - 1} - 0.296 * X_i{i - 2}









