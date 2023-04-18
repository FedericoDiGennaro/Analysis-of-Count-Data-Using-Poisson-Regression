# load packages -----------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(GGally)
library(readxl)
library(MASS) # for BOXCOX
library(car) # for VIF
library(performance) # for nicer multicollinearity plot

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
X <- data[-c(1,3)]
X$Direction <- as.factor(X$Direction)

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
ggpairs(X[],
        aes(color = X$Direction, alpha = .5),
        lower = list(continuous = 'smooth'), legend = 1) +
  theme(legend.position = "bottom", text = element_text(size = 14), 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10) )

par(mfrow=c(2,2))

qqnorm(data$Distance, pch = 1, frame = FALSE, main = "Distance")
qqline(data$Distance, col = "steelblue", lwd = 2)

qqnorm(data$Population, pch = 1, frame = FALSE, main = "Population")
qqline(data$Population, col = "steelblue", lwd = 2)

qqnorm(data$Degree_Urb, pch = 1, frame = FALSE, main = "Degree Urbanisation")
qqline(data$Degree_Urb, col = "steelblue", lwd = 2)

#Outliers detenction

par(mfrow=c(1,1))
boxplot(X[-2], las = 2, col = c("red", "steelblue", "yellow"), ylab ="(%)")

# MODEL FITTING ---------------------------------------------------------------------

# linear regression with all the variables
# model selection (forward/backward selection)
# transformation of some of the variable (e.g. Box-Cox) and new linear regression with them

#linear regression with all the variables
model <- lm(p_stop ~ p_workforce + Union_shop + p_SectorA + p_agriculture, data=data)
coef(model)
summary(model)
vif(model)
vif_values <- vif(model)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

# abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation
# all acceptable values 

# Perform backward selection using AIC
final_model <- step(model, direction = "backward", trace = FALSE)
coef(final_model)
summary(final_model)

#transformations and new fit
ggpairs(X[-2])

new_X = X
new_X$p_SectorA = log(new_X$p_SectorA)
new_X$p_agriculture = log(new_X$p_agriculture)
# new_X$p_workforce = exp(new_X$p_workforce)

ggpairs(new_X[-2])

#linear regression with all the variables AND LOG TRANSFORM
model <- lm(y$p_stop ~ new_X$p_workforce + new_X$Union_shop + new_X$p_SectorA + 
              new_X$p_agriculture)
coef(model)
summary(model)

# Perform backward selection using AIC
final_model <- step(model, direction = "backward", trace = FALSE)
coef(final_model)
summary(final_model)

# BOX COX
model <- lm(p_stop ~ p_workforce + Union_shop + p_SectorA + p_agriculture, data=data)
bc <- boxcox(model, interp = TRUE)
lambda <- bc$x[which.max(bc$y)]
abline(v = lambda, col = "red")
print(c('Lambda that maximize log-likelihood is ', lambda))

data_bc <- data
data_bc$p_stop <- (data_bc$p_stop^lambda -1)/lambda
model_bc <- lm(p_stop ~ p_workforce + Union_shop + p_SectorA + p_agriculture, 
               data=data_bc)
summary(model_bc)


####################################
# since the result of this analysis are very poor, let's try to add
# the interaction terms (given the correlation among variables)


#linear regression with all the variables
model <- lm(p_stop ~ (p_workforce + Union_shop + p_SectorA + p_agriculture)^2, data=data)
summary(model)

# abline(v = 5, lwd = 3, lty = 2)    #add vertical line at 5 as after 5 there is severe correlation
# all acceptable values 

# Perform backward selection using AIC
final_model <- step(model, direction = "backward", trace = FALSE)
summary(final_model)

vif(final_model)
vif_values <- vif(final_model)           #create vector of VIF values

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

collinearity <- check_collinearity(final_model)
plot(collinearity)


# apply box-cox transformation 
model_box <- lm(p_stop ~ (p_workforce + Union_shop + p_SectorA + p_agriculture)^2, data=data)
bc <- boxcox(model_box, interp = TRUE)
lambda <- bc$x[which.max(bc$y)]
abline(v = lambda, col = "red")
print(c('Lambda that maximize log-likelihood is ', lambda))

data_bc <- data
data_bc$p_stop <- (data_bc$p_stop^lambda -1)/lambda
model_bc <- lm(p_stop ~ (p_workforce + Union_shop + p_SectorA + p_agriculture)^2, 
               data=data_bc)
summary(model_bc)
final_model_bc <- step(model_bc, direction = "backward", trace = FALSE)
summary(final_model_bc)







