# load packages -----------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(GGally)
library(readxl)
library(MASS) # for BOXCOX
library(car) # for VIF
library(performance) # for nicer multicollinearity plot
library(jtools)
library(sandwich)
library(goodness-of-fit)
library(pscl)
# load data ---------------------------------------------------------------
rm(list = ls())


data <- read_excel("data/data.xlsx")
glimpse(data)

# EDA ---------------------------------------------------------------------

# analysis missing values check
# univariate numerical check 
# univariate graphical check
# multivariate numerical check
# multivariate graphical check
# outliers check

# define regressors
data$Direction <- as.factor(data$Direction)
X <- data[-c(1,3)]

# define dependent
y <- data[3]

# check presence of missing values
print(c('is there any missing value? ', any(is.na(data))))

# univariate numerical analysis for numeric variables
summary(X)
var(X[-4])


# for the graphical analysis look at the following pairs plot (AGGREGATE DATA)

ggpairs(X)

# in order to check the differences in distribution due to difference in the
# union_shop variable, use this: (DISAGGREGATE DATA wrt Union_shop)
# Create the ggpairs plot
plot <- ggpairs(X, aes(color = Direction, alpha = .5),
                lower = list(continuous = 'smooth'), legend = 1) +
  theme(legend.position = "bottom", text = element_text(size = 12), 
        axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8))
# Remove alpha from the legend
plot <- plot +
  scale_alpha_identity(guide = "none")
# Display the plot
print(plot)

# Create the ggpairs plot with histogram
hist(y$Apprentices, breaks=seq(0,250,10))
hist(y$Apprentices, breaks=seq(0,250,10), xlab = "Number of Apprentices", ylab = "Frequency")

mean(y$Apprentices)
var(y$Apprentices)

# The variance is much greater than the mean, which suggests that we will 
# have over-dispersion in the model.

#Outliers detenction

par(mfrow=c(1,1))
boxplot(X[-4], las = 2, col = c("red", "steelblue", "yellow"), ylab ="(%)")

# MODEL 1 ---------------------------------------------------------------------

poisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                      + Direction , data, family = poisson(link = "log"))
summary(poisson.model)

qpoisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction , data, family = quasipoisson())
summary(qpoisson.model)

cov.m1 <- vcovHC(poisson.model, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(poisson.model), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson.model)/std.err), lower.tail=FALSE),
               LL = coef(poisson.model) - 1.96 * std.err,
               UL = coef(poisson.model) + 1.96 * std.err)
r.est

with(poisson.model, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# from the test above, we can evince that data does not fit the model well

#MODEL ASSESSMENT
residuals <- residuals(poisson.model)
plot(fitted(poisson.model), residuals, type = "p", xlab = "Fitted values", ylab = "Residuals")

hist(residuals, main = "Residuals Histogram")
density_res <- density(residuals)
plot(density_res, main = "Residuals Density Plot")

lambda <- mean(y$Apprentices)  # Calculate the mean of the response variable
expected_quantiles <- qpois(ppoints(length(residuals)), lambda)
qqplot(expected_quantiles, residuals, main = "Q-Q Plot")

# Calculate the Pearson residuals
pearson_resid <- residuals(poisson.model, type = "pearson")
# Calculate the Pearson chi-square statistic
pearson_chi_sq <- sum(pearson_resid^2)
# Obtain the residual degrees of freedom
df_resid <- df.residual(poisson.model)
# Estimate the dispersion parameter
dispersion <- pearson_chi_sq / df_resid
# Print the estimated dispersion parameter
print(dispersion)

epiDisplay::poisgof(poisson.model)

# MODEL 2: removing the outlier ---------------------------------------------------------------------
rm(list = ls())
data <- read_excel("data/data2.xlsx")
glimpse(data)

# define regressors
data$Direction <- as.factor(data$Direction)
X <- data[-c(1,3)]

# define dependent
y <- data[3]

hist(y$Apprentices, breaks=seq(0,250,10))

mean(y$Apprentices)
var(y$Apprentices)

poisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                    + Direction , data, family = poisson(link = "log"),
                   control = glm.control(maxit = 1000))
summary(poisson.model)

qpoisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction , data, family = quasipoisson(),
                   control = glm.control(maxit = 1000))
summary(qpoisson.model)

with(poisson.model, cbind(res.deviance = deviance, df = df.residual,
                         p = pchisq(deviance, df.residual, lower.tail=FALSE)))


poisson.model2<-glm(Apprentices ~ Distance + Population + Degree_Urb
                    + Direction , data, family = quasipoisson(),
                    control = glm.control(maxit = 1000))
summary(poisson.model2)

# MODEL 3: log of population and distance ---------------------------------------------------------------------
data$Distance = log(data$Distance)
data$Population = log(data$Population)
poisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction, data , family = poisson(link = "log"),
                   control = glm.control(maxit = 1000))
summary(poisson.model)

poisson.model<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction, data , family = quasipoisson(),
                   control = glm.control(maxit = 1000))
summary(poisson.model)

# MODEL 4: interactions ---------------------------------------------------------------------
ggpairs(X)

#without the log
rm(list = ls())
data <- read_excel("data/data2.xlsx")
glimpse(data)
data$Direction <- as.factor(data$Direction)
X <- data[-c(1,3)]
y <- data[3]
poisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                    + Direction)^2 , data, family = poisson(link="log"))
summary(poisson.model3)
poisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                   + Direction)^2 , data, family = quasipoisson())
summary(poisson.model3)

#with the log
rm(list = ls())
data <- read_excel("data/data2.xlsx")
glimpse(data)
data$Direction <- as.factor(data$Direction)
X <- data[-c(1,3)]
y <- data[3]
data$Distance = log(data$Distance)
data$Population = log(data$Population)

poisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                   + Direction)^2 , data, family = poisson(link="log"))
summary(poisson.model3)

poisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                   + Direction)^2 , data, family = quasipoisson())
summary(poisson.model3)

with(poisson.model3, cbind(res.deviance = deviance, df = df.residual,
                           p = pchisq(deviance, df.residual, lower.tail=FALSE)))
