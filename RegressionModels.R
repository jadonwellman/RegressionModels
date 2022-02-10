library('UsingR')
data(Galton)

x <- Galton$parent
y <- Galton$child
cov_xy <- cov(x,y)
sd_x <- sd(x)
mean_x <- mean(x)
sd_y <- sd(y)
mean_y <- mean(y)
cor_xy <- cov_xy / (sd_x * sd_y)
print(cor_xy == cor(x,y))

par(mfrow=c(1,2))
hist(x)
abline(v=mean_x, col="blue")
hist(y)
abline(v=mean_y, col="blue")

par(mfrow=c(1,3))

plot(x,jitter(y,4)) # jitter helps us see frequency
abline(v=mean_x,h=mean_y, col='blue')
abline(v=mean_x+sd_x,h=mean_y+sd_y,col='red')
abline(v=mean_x-sd_x,h=mean_y-sd_y,col='red')

beta_1 <- cor(y,x) * sd_y/sd_x
beta_0 <- mean_y - beta_1 * mean_x
abline(a=beta_0,b=beta_1,col='green')

x_shifted <- x-mean_x
y_shifted <- y-mean_y
plot(x_shifted,jitter(y_shifted,4))
# 'cov' doesn't need to subtract the means because it was shifted. So instead of
# (X-X_mean)*(Y-Y_mean) it is just X*Y. And then the 'cor' is cov/sd_x*sd_y and
# beta_1 then multiplies that by sd_y/sd_x so the sd_ys cancel out, so the 
# sd_x is squared. And the (n-1)s are cancelled out.
beta_1_shifted <- sum(x_shifted*y_shifted)/sum(x_shifted^2) 
abline(a=0,b=beta_1_shifted,col='green')

# Normalizing the data makes the slope the correlation
xn <- (x-mean_x)/sd_x
yn <- (y-mean_y)/sd_y
plot(xn,jitter(yn,4))
beta_1_n <- cor(yn,xn) * sd(xn)/sd(yn)
abline(a=0,b=beta_1_n,col='green')

# Exploring Simpson's Paradox and other elements of multivariate regression

require(datasets); data(swiss)
pairs(swiss)
cor(swiss)
summary(lm(Fertility ~ . , data = swiss))
# But..
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients
# Let's look at the impact of important variables across each other
summary(lm(Fertility ~ Agriculture+Education, data = swiss))$coefficients

# Let's simulate dependencies
n = 100; x2 <- 1 : n; x1 = .01 * x2 + runif(n, -.1, .1)
y = -1*x1 + 1*x2 + rnorm(n, sd = .01)
dat = data.frame(y = y, x1 = x1, x2 = x2)
pairs(dat)
cor(dat)
plot(y~x1)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef
library(ggplot2)
dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g
g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2
fit <- lm(y~x2)
plot(y~x2)
abline(fit[1],fit[2])
y_var <- sum((y-mean(y))^2)
x2_r_var <- sum(fit$residuals^2)
x2_var <- sum(fit$fitted.values^2)

# Let's go back to the swiss dataset
dat <- data.frame(y = swiss$Fertility, Ag = swiss$Agriculture, Ed = swiss$Education, 
                  ey = resid(lm(swiss$Fertility ~ swiss$Education)), 
                  eAg = resid(lm(swiss$Agriculture ~ swiss$Education)))
g = ggplot(dat, aes(y = y, x = Ag, colour = Ed))
g = g + geom_point(colour="grey50", size = 5) + 
  geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g
g2 = ggplot(dat, aes(y = ey, x = eAg, colour = Ed))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2

# Let's go back to the basics, trying to understand the relationships. To 
# find the coefficient of agriculture look at the relationship between
# the residuals left over from education in both the predictor and 
# response

require(datasets); data(swiss)
x1 <- swiss$Agriculture
x2 <- swiss$Education
y <- swiss$Fertility

plot(y~x1,ylim=c(0,100))
fit<-lm(y~x1)
abline(fit[1],fit[2])
fit$coefficients # be careful of Simpson's Paradox here...

lm(y~x1+x2)$coefficients

plot(y~x2)
fit<-lm(y~x2)
abline(fit[1],fit[2])
ey <- resid(lm(y~x2))

plot(x1~x2)
fit<-lm(x1~x2)
abline(fit[1],fit[2])
ex1 <- resid(lm(x1~x2))

plot(ey~0+ex1)
fit<-lm(ey~0+ex1)
abline(a=0,b=fit[1])
fit$coefficients

# Exploring Adjustments

# X not making a big difference. The diference of the means disregarding x is
# about the same as when including it. I think this happens when randomization 
# was used, so x was well distributed across both groups.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Where X makes a big difference, perhaps the experiment wasn't randomized well.
# Red it treated and blue is control. Went from a massive treatment effect to
# nothing when we accounted for x.
# Have to rely on the model to see affect when holding x constant.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# There is some overlap here, some direct evidence. Disregarding x, red is
# higher than blue. But with x, blue is higher (look at the intercepts)
# Simpson's Paradox.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# No "marginal effect" (without x), but when adjust for x there is.
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# We would get this wrong if we assumed the slopes were common across the two
# groups. This is an example with an interaction. No evidence of a treatment
# effect. Treatment depends on the level of x you are at (the interaction, see
# the left side and right side)
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1));
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Doesn't need to include binary factor. x2 here is continuous. Not much relationship
# between y and x1. But when regress out x2 there is a strong relationship. There
# was some residual left after comparing x2 to y.
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)
# If had OpenGL in this build, like Windows has:
#library(rgl)
#plot3d(x1, x2, y)
ey <- resid(lm(y~x2))
ex1 <- resid(lm(x1~x2))
plot(ey~ex1)
fit<-lm(ey~0+ex1)
summary(fit)$coefficients

data(swiss); par(mfrow=c(2,2))
fit <- lm(Fertility~.,data=swiss)
plot(fit)

# Judging when to add or remove variables

n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3) # effect on betas
round(dffits(fit)[1:10],3) # effect on the response
round(cooks.distance(fit)[1:10],3) # total effect on all betas
round(hatvalues(fit)[1:10],3) # amount of leverage

# Too many regressors on R^2

n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
y <- rnorm(n); x <- NULL; r <- NULL
for (i in 1 : n){
  x <- cbind(x, rnorm(n))
  r <- c(r, summary(lm(y ~ x))$r.squared)
}
lines(1 : n, r, lwd = 3)
abline(h = 1)

# On variance

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n);
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2],
    coef(lm(y ~ x1 + x2))[2],
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

# Logistic regression

x = seq(-10, 10, length = 1000)
beta0 = 0; 
beta1s = seq(.25, 1.5, by = .1)
#beta1s = 0.25 # slope is flatter
#beta1s = 1.5 # slope is more extreme
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = FALSE)
sapply(beta1s, function(beta1) {
  y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
  lines(x, y, type = "l", lwd = 3)
}
)

x = seq(-10, 10, length = 1000)
beta0s = seq(-2, 2, by = .5); beta1 = 1
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = FALSE)
sapply(beta0s, function(beta0) {
  y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
  lines(x, y, type = "l", lwd = 3)
}
)

x = seq(-10, 10, length = 1000)
beta0 = 0; beta1 = 1
p = 1 / (1 + exp(-1 * (beta0 + beta1 * x)))
y = rbinom(prob = p, size = 1, n = length(p))

plot(x, y, frame = FALSE, xlab = "x", ylab = "y")
lines(lowess(x, y), type = "l", col = "blue", lwd = 3)
fit = glm(y ~ x, family = binomial)
lines(x, predict(fit, type = "response"), lwd = 3, col = "red")

# Poisson Exercises

data(Seatbelts)
Seatbelts <- as.data.frame(Seatbelts)
plot(Seatbelts$DriversKilled)
plot(Seatbelts$law)
# log(mean of DriversKilled) = B0 + B1*kms +...
mdl <- glm(DriversKilled~kms+PetrolPrice+law,poisson,Seatbelts)
summary(mdl)
coef <- summary(mdl)$coef
# look at the influences on the yearly mean of drivers killed
# of "law", the mean of Drivers Killed / day, dropped 11%
# of PetrolPrice, every pound increase would drop it 99%
# of kms, every increase of kms driven, would drop it just 0.001%
exp(coef[,1])
mean(Seatbelts$DriversKilled)
# this is deaths when all covariates are 0. Brian said it was for the average
# million miles (kms/1000) driven and average petrol price (normalized in sd 
# units) and no law b/c he centered and transformed those two, i didn't
exp(coef[1,1]) 
# also consider confidence intervals
exp(confint(mdl))

# Let's do the Brian way..
Seatbelts <- Seatbelts %>% mutate(mmc = (kms - mean(kms))/1000,
                    ppsd = (PetrolPrice-mean(PetrolPrice))/sd(PetrolPrice))
plot(Seatbelts$mmc)
plot(Seatbelts$kms)
plot(Seatbelts$ppsd)
plot(Seatbelts$PetrolPrice)

mdl2 <- glm(DriversKilled~mmc+ppsd+law,poisson,Seatbelts)
coef2 <- summary(mdl2)$coef
# the law dropped average yearly deaths 11%, as before. each additional 1M miles
# drops it 1%, and every increase in one sd (0.012) of Petrol Price drops it 5%.
# The intercept says that when no law, and average distance driven and
# average petrol price, 124 driver deaths happen in a year
exp(coef2[,1])

# Question 2

lmdl <- lm(log(DriversKilled)~mmc+ppsd+law,Seatbelts)
summary(lmdl)
coef <- summary(lmdl)$coef
exp(coef[,1]) # relative to geometric means

# 3

mdl3 <- glm(DriversKilled~mmc+ppsd+law,poisson,Seatbelts,offset=log(drivers))
summary(mdl3)
coef <- summary(mdl3)$coef
exp(coef[,1]) 

#4

mdlA <- glm(DriversKilled~law,poisson,Seatbelts)
mdlB <- glm(DriversKilled~ppsd+law,poisson,Seatbelts)
mdlC <- glm(DriversKilled~mmc+ppsd+law,poisson,Seatbelts)
anova(mdlA,mdlB,mdlC)

# Quiz

# 1

library(MASS)
library(dplyr)
data("shuttle")
head(shuttle)
str(shuttle)
summary(shuttle)
plot(shuttle$wind,shuttle$use)

# Odds = 0.78 + 0.97*(wind=tail)
# Head: 0.78
# Tail: 1.75
mdl <- glm(use~wind,binomial,shuttle)
coef <- summary(mdl)$coef
exp(coef[,1])

# Odds = 0.75 + 1.03*(wind=head)
# Head: 1.79
# Tail: 0.75
shuttle$wind <- relevel(shuttle$wind,'tail')
mdl <- glm(use~wind,binomial,shuttle)
coef <- summary(mdl)$coef
exp(coef[,1])

# log(odds) = B0 + B1*wind = log(p/(1-p))
# exp(B1) = odds = p/(1-p)
mdl <- glm(use~wind,binomial,shuttle)
coef <- summary(mdl)$coef
coef
# If there is a tailwind the odds of giving the autolander a green light
# are .75 or 43%. If there is a headwind the odds are 1.79 or 64%.
# Odds = 0.75 + 1.03*(wind=head)
# But the p-values are weak
e_coef <- exp(coef[,1])
e_coef
# Give the estimated odds ratio for autolander use comparing head winds, labeled 
# as "head" in the variable headwind (numerator) to tail winds (denominator).
(as.numeric(e_coef[1])+as.numeric(e_coef[2]))/as.numeric(e_coef[1])

