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
library(AER)
library(AICcmodavg)
library(performance)
library(gridExtra)
library(dplyr)

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
dispersion_test(poisson.model)

qpoisson.model1<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction , data, family = quasipoisson())
summary(qpoisson.model1)

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

qpoisson.model1<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction , data, family = quasipoisson(),
                   control = glm.control(maxit = 1000))
summary(qpoisson.model1)

# fit negative binomial model
nb_model <- glm.nb(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction, data)

# print summary of the model
summary(nb_model)

par(mfrow=c(1,1))

yhat <- predict(poisson.model, type = "response")
plot(yhat, residuals(poisson.model, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals")

# MODEL 3: log of population and distance ---------------------------------------------------------------------
data$Distance = log(data$Distance)
data$Population = log(data$Population)

nb_model2<-glm.nb(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction, data)
summary(nb_model2)

AIC(nb_model, nb_model2)


qpoisson.model2<-glm(Apprentices ~ Distance + Population + Degree_Urb
                   + Direction, data , family = quasipoisson(),
                   control = glm.control(maxit = 1000))
summary(qpoisson.model2)

# MODEL 4: interactions ---------------------------------------------------------------------
ggpairs(X)

#with the log

poisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                   + Direction)^2 , data, family = poisson(link="log"))
summary(poisson.model3)
dispersiontest(poisson.model3)

qpoisson.model3<-glm(Apprentices ~ (Distance + Population + Degree_Urb
                                   + Direction)^2 , data, family = quasipoisson())
summary(qpoisson.model3)

#F test to compare two quasi-poisson models (NESTED, H0: reduced model is sufficient)
drop_in_dev <- anova(qpoisson.model2, qpoisson.model3, test = "F")
drop_in_dev

nb.model<-glm.nb(Apprentices ~ (Distance + Population + Degree_Urb
                                + Direction)^2 , data)
summary(nb.model)

#MODEL ASSESSMENT

#1) linearity with log count
predicted <- predict(qpoisson.model3, type = "link")
observed <- y$Apprentices

plot(predicted, observed, xlab = "Predicted Linear Predictor", ylab = "Observed Log Count")
abline(lm(observed ~ predicted), col = "red")

#2) indep obs

residuals <- residuals(qpoisson.model3)
# Create a scatterplot of residuals against each other
plot(residuals[-length(residuals)], residuals[-1], xlab = "Residuals (i)", ylab = "Residuals (i+1)")

par(mfrow = c(2,2))
# Plot normality check
plot(check_normality(qpoisson.model3))
# Plot outliers check
plot(check_outliers(qpoisson.model3))
# Plot distribution check
plot(check_distribution(qpoisson.model3))
# Plot overdispersion check
plot(check_overdispersion(qpoisson.model3))

check_autocorrelation(qpoisson.model3)
check_zeroinflation(qpoisson.model3)
check_independence(qpoisson.model3)

performance::check_model(qpoisson.model2, plot = FALSE)

par(mfrow = c(1,1))
# Extract the residuals
my_resid <- resid(qpoisson.model3, type = "pearson")
# Plot the residuals against the fitted values
plot(qpoisson.model3$fitted.values, my_resid)

plot(qpoisson.model3, which=1)

check_zeroinflation(qpoisson.model3)

data_filtered <- filter(data, Apprentices != 0)

# Create the scatterplot with filtered data
ggplot(data = data_filtered, aes(x = Degree_Urb, y = log(Apprentices))) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Apprentices and LogDistance", x = "LogDistance", y = "Log(Apprentices)")

ggplot(data = data_filtered, aes(x = Distance, y = log(Apprentices))) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Apprentices and LogDistance", x = "LogDistance", y = "Log(Apprentices)")

#################################
data <- read_excel("data/data2.xlsx")
data$Direction <- as.factor(data$Direction)

nb_model1<-glm.nb(Apprentices ~ (Distance + Population + Degree_Urb + Direction), data)
summary(nb_model1)

data$Distance = log(data$Distance)
colnames(data)[colnames(data) == "Distance"] <- "LogDistance"
data$Population = log(data$Population)
colnames(data)[colnames(data) == "Population"] <- "LogPopulation"

nb_model2<-glm.nb(Apprentices ~ (LogDistance + LogPopulation 
                                 + Degree_Urb + Direction), data)
summary(nb_model2)

#AIC1 = 184.64
#AIC2 = 166.62 --> this is the preferred model

#now I add interactions to model pref
nb_model3<-glm.nb(Apprentices ~ (LogDistance + 
                                   LogPopulation + Degree_Urb + Direction)^2, data, maxit=10000)
summary(nb_model3)

lrtest(nb_model2, nb_model3)
# p-value of this test is 0.081

#starting from the full model in model 3

# Perform stepwise model selection
final_model<-stepAIC(nb_model2, direction = "both", trace = FALSE)

# Print summary of the selected model
summary(final_model)

