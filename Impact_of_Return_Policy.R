# Return Policy - Jewelry Final Project - Econometrics 5:45pm
#Mia Bajic, Timothy Nguyen, McCall James, Jared Sinsheimer, Hale Tussing

# Clear the working space
rm(list = ls())

# Set the directory
setwd("C:/Users/miabajic/Downloads")

install.packages("readstata13")
install.packages("WDI")
install.packages("RJSONIO")
install.packages("doBy")
install.packages("countrycode")
install.packages("XML")
# Load the packages (must have been installed)
library(readstata13)
library(WDI)
library(RJSONIO)
library(sandwich)
library(readstata13)
library(stargazer)
library(ggplot2)
library(WDI)
library(doBy)
library(plyr)
library(countrycode)
library(dplyr)
library(tidyr)
library(XML)
library(gdata)
library(psych) 
library(ggeffects)
library(QuantPsyc)
library(usdm)
library(lmtest)
library(multiwayvcov)
library(foreign)
library(AER)
library(aod)
library(Rcpp)
library(mfx)
library(nnet)
library(reshape2)

# turn off scientific notation except for big numbers
options(scipen = 9) 
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#read data sets
physical_pcat_sales_returns <- read.dta13("BM store monthly prod_cat sales-returns.dta")
physical_sales_returns <- read.dta13("BM store monthly sales-returns.dta")
online_pcat_sales_returns <- read.dta13("Online store daily prod_cat sales-returns.dta")
online_sales_returns <- read.dta13("Online store daily sales-returns.dta")

#View datasets
View(physical_pcat_sales_returns)
View(physical_sales_returns)
View(online_pcat_sales_returns)
View(online_sales_returns)

#make tables of descriptive statistics for quantitative variables
stargazer(physical_pcat_sales_returns,type="text", median=TRUE, digits=2, title="Table 1. Physical Store Product Category Summary Statistics")
stargazer(physical_sales_returns,type="text", median=TRUE, digits=2, title="Table 2. Physical Store Summary Statistics")
stargazer(online_pcat_sales_returns,type="text", median=TRUE, digits=2, title="Table 3. Online Product Category Summary Statistics")
stargazer(online_sales_returns,type="text", median=TRUE, digits=2, title="Table 4. Online Summary Statistics")

###Data Manipulation###

#No zeroes as minimums
online_sales_returns$salesvalue <- online_sales_returns$salesvalue + 1
online_sales_returns$returnvalue <- online_sales_returns$returnvalue + 1
online_sales_returns$returnquantity <- online_sales_returns$returnquantity + 1

online_pcat_sales_returns$salesvalue <- online_pcat_sales_returns$salesvalue + 1
online_pcat_sales_returns$returnvalue <- online_pcat_sales_returns$returnvalue + 1
online_pcat_sales_returns$returnquantity <- online_pcat_sales_returns$returnquantity + 1

physical_sales_returns$returnvalue <- physical_sales_returns$returnvalue + 1
physical_sales_returns$returnquantity <- physical_sales_returns$returnquantity + 1

physical_pcat_sales_returns$salesvalue <- physical_pcat_sales_returns$salesvalue + 1
physical_pcat_sales_returns$returnvalue <- physical_pcat_sales_returns$returnvalue + 1
physical_pcat_sales_returns$returnquantity <- physical_pcat_sales_returns$returnquantity + 1


#create two new variables. post_change_date: 0 means observation is before policy change, 1 means observation is after policy is changed. new_policy: 0 means observation belongs to brand that does not change policy, 1 means observation belongs to brand that changes policy.

online_sales_returns$month_index <- (12*(online_sales_returns$year - 2013)) + online_sales_returns$month_dummy
online_sales_returns$time <- ifelse(online_sales_returns$month_index<10,0,1)
online_sales_returns$new_policy <- ifelse(online_sales_returns$store_number>7,0,1)

online_pcat_sales_returns$month_index <- (12*(online_pcat_sales_returns$year - 2013)) + online_pcat_sales_returns$month_dummy
online_pcat_sales_returns$time <- ifelse(online_pcat_sales_returns$month_index<10,0,1)
online_pcat_sales_returns$new_policy <- ifelse(online_pcat_sales_returns$store_number>7,0,1)

physical_sales_returns$time <- ifelse(physical_sales_returns$month_index<51,0,1)
physical_sales_returns$new_policy <- ifelse(physical_sales_returns$brand_number>7,0,1)

physical_pcat_sales_returns$time <- ifelse(physical_pcat_sales_returns$month_index<51,0,1)
physical_pcat_sales_returns$new_policy <- ifelse(physical_pcat_sales_returns$brand_number>7,0,1)


# create new data sets without null values
online_sales_returns_NN <- online_sales_returns[complete.cases(online_sales_returns), ]
online_pcat_sales_returns_NN <- online_pcat_sales_returns[complete.cases(online_pcat_sales_returns), ]
physical_sales_returns_NN <- physical_sales_returns[complete.cases(physical_sales_returns), ]
physical_pcat_sales_returns_NN <- physical_pcat_sales_returns[complete.cases(physical_pcat_sales_returns), ]

## Question 1: What is the impact of the policy change on online channel sales?
# Dataset: online_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue

# Check distributions of count variables
hist(online_sales_returns_NN$salesvalue)
online_sales_returns_NN$logsalesvalue = log(online_sales_returns_NN$salesvalue)
hist(online_sales_returns_NN$logsalesvalue)
hist(online_sales_returns_NN$salesquantity)
online_sales_returns_NN$logsalesquantity = log(online_sales_returns_NN$salesquantity)
hist(online_sales_returns_NN$logsalesquantity)
hist(online_sales_returns_NN$returnvalue)
online_sales_returns_NN$logreturnvalue = log(online_sales_returns_NN$returnvalue)
hist(online_sales_returns_NN$logreturnvalue)
hist(online_sales_returns_NN$returnquantity)
online_sales_returns_NN$logreturnquantity = log(online_sales_returns_NN$returnquantity)
hist(online_sales_returns_NN$logreturnquantity)


#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova
#Return value and return quantity happen after sales, so it is not good practice to use them to predict sales. ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(online_sales_returns_NN$salesquantity, online_sales_returns_NN$avg_female, online_sales_returns_NN$avg_age, online_sales_returns_NN$avg_income, online_sales_returns_NN$avg_homeowner, online_sales_returns_NN$avg_residency, online_sales_returns_NN$avg_childowner, online_sales_returns_NN$time, online_sales_returns_NN$new_policy)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset


model1=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.

# Heteroskedasticity
pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals don't demonstrate visual pattern.

gqtest(model1) # Goldfeld-Quandt test is  significant, implying there is heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is still insignificant.


### Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity

#First: Poisson
# don't actually use OLS because you can get negative values 

model1<- lm(logsalesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_sales_returns_NN) 

stargazer(model1,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

online_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 

ggplot(online_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(online_sales_returns_NN$pred_sales_quantity) # no negatives


#Poisson model
poisson1 <- glm(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=online_sales_returns_NN)

#dependent variable is log sales quantity
stargazer(poisson1,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
# Not going to use this
## Delete this comment later The interaction term coefficient is .08. This means that the expected log count when a store experiences the new return policy  is .08. Introducing the new return policy is associated with a .08 increase in log sales quantity.

# Model fit assessment 
poisson1a <- glm(salesquantity~1, data=online_sales_returns_NN, family="poisson")

lrtest(poisson1, poisson1a) # We can use the residual deviance to perform a goodness of fit test for the overall model. The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the predicted values are identical to the observed. Therefore, if the residual difference is small enough, the goodness of fit test will not be significant, indicating that the model fits the data. We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson1, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 
# Not going to use this, delete later
#Let's obtain IRRs. Sales quantity is 100*(1.0824-1)% higher aka 8.24% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.0824 times the incident rate for old return policy. 

# Check for heteroscedasticity
gqtest(poisson1) # Goldfeld-Quandt test is significant, indicates heteroscedasticity
bptest(poisson1) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(poisson1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(poisson1, poisson1,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
# Robust standard errors make our interaction term insigificant.

# Visualize
#meffects <- ggpredict(poisson1, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable


# Part 2: Negative Binomial

negbin1 <- glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data = online_sales_returns_NN)

stargazer(negbin1,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is insignificant. 

# Model fit assessment
negbin1a <- glm.nb(salesquantity ~ 1, data = online_sales_returns_NN) 

lrtest(negbin1, negbin1a) # # Model fits the data because LR test statistics is  significant.

# Choosing between Poisson and Negative Binomial regressions

lrtest(poisson1, negbin1) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin1, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # We might be interested in looking at incident rate ratios rather than coefficients. To do this, we can exponentiate our model coefficients. However, coefficient on interaction term is insignificant.

stargazer(negbin1, poisson1, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="IRR Comparison", type="text", 
          column.labels=c("Neg. Binomial", "Poisson"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # We might be interested in looking at incident rate ratios rather than coefficients. To do this, we can exponentiate our model coefficients. Comparing NB to Poisson doesn't help us because Poisson is significant but doesn't have model fit, and our LR test said we should use negative binomial but the coefficient is insignificant on it.

# Check for heteroscedasticity
gqtest(negbin1) # Goldfeld-Quandt test is significant, indicates heteroscedasticity
bptest(negbin1) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(negbin1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin1, negbin1,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Coefficient on interaction term is still insignificant.

# Visualize the output


#meffects2 <- ggpredict(negbin1, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time

#ggplot(meffects2,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="NB Time/Policy Interation on Sales Quantity", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Question 2: What is the impact of the policy change on physical store sales?
# Dataset: physical_sales_returns

# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue

# Check distributions of count variables
hist(physical_sales_returns_NN$salesvalue)
physical_sales_returns_NN$logsalesvalue = log(physical_sales_returns_NN$salesvalue)
hist(physical_sales_returns_NN$logsalesvalue)
hist(physical_sales_returns_NN$salesquantity)
physical_sales_returns_NN$logsalesquantity = log(physical_sales_returns_NN$salesquantity)
hist(physical_sales_returns_NN$logsalesquantity)
hist(physical_sales_returns_NN$returnvalue)
physical_sales_returns_NN$logreturnvalue = log(physical_sales_returns_NN$returnvalue)
hist(physical_sales_returns_NN$logreturnvalue)
hist(physical_sales_returns_NN$returnquantity)
physical_sales_returns_NN$logreturnquantity = log(physical_sales_returns_NN$returnquantity)
hist(physical_sales_returns_NN$logreturnquantity)

#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#Return value and return quantity happen after sales, so it is not good practice to use them to predict sales. ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(physical_sales_returns_NN$salesquantity, physical_sales_returns_NN$avg_female, physical_sales_returns_NN$avg_age, physical_sales_returns_NN$avg_income, physical_sales_returns_NN$avg_homeowner, physical_sales_returns_NN$avg_residency, physical_sales_returns_NN$avg_childowner, physical_sales_returns_NN$time, physical_sales_returns_NN$new_policy)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset

model1=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. Let's check heteroskedasticity just in case.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals are clustered around 9 and 12. WHY??


gqtest(model1) # Goldfeld-Quandt test is  significant, implying there is heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is still significant. A return policy change from 90 to 45 days decreases sales value by 3.33% in physical stores.

### Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity
#First: Poisson

#Linear
model1<- lm(logsalesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_sales_returns_NN) 

stargazer(model1,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

physical_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 


ggplot(physical_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(physical_sales_returns_NN$pred_sales_quantity) # Range doesn't go into negatives...

#Poisson
poisson2 <- glm(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=physical_sales_returns_NN)


stargazer(poisson2,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#remove this later
#The interaction term coefficient is .11. This means that the expected log count when a store experiences the new return policy  is .11. Introducing the new return policy is associated with a .11 increase in log sales quantity.

# Model fit assessment 
poisson2a <- glm(salesquantity~1, data=physical_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson2, poisson2a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson2, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# #remove this later
#Let's obtain IRRs. Sales quantity is 100*(1.1184-1)% higher aka 11.84% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1184 times the incident rate for old return policy. 

# Check for heteroscedasticity 
gqtest(poisson2) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity
bptest(poisson2) # Breusch-Pagan test is significant, indicates heteroscedasticity


consstder <- sqrt(diag(vcovHC(poisson2, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson2, type="HC1"))) # produces Huber-White robust standard errors

stargazer(poisson2, poisson2,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#remove this later
#robust standard errors do not change any significance of any coefficients. Coefficient on interaction term is still significant. 


#meffects <- ggpredict(poisson2, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




### Part 2: Negative Binomial ###

negbin2 <- glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data = physical_sales_returns_NN)

stargazer(negbin2,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is significant


## Model fit assessment
negbin2a <- glm.nb(salesquantity ~ 1, data = physical_sales_returns_NN) 

lrtest(negbin2, negbin2a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson2, negbin2) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.


# Obtain IRRs
stargazer(negbin2, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Looking at IRRs, the coefficient on interaction term is still significant with one * (at 10% level) Sales quantity is 100*(1.1786-1)% higher aka 17.86% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1786 times the incident rate for old return policy.

#stargazer(negbin2, poisson2, 
#         apply.coef = exp, t.auto=F, p.auto = F,
#        title="IRR Comparison", type="text", 
#       column.labels=c("Neg. Binomial", "Poisson"),
#      df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
### this doesn't matter b/c although coefficient on interaction term in Poisson model is significant, the LR test told us we do not have the model fit w/ Poisson. When using Neg Binomial, we have model fit b/c LR test was significant & the coefficient on our interaction term is significant at the 10% level


# Check for heteroscedasticity 
gqtest(negbin2) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity
bptest(negbin2) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(negbin2, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin2, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin2, negbin2,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Robust standard errors change significance of some coefficients but do not completely eliminate/bring significance. Our interpretation remains the same as before.

# Visualize output


#meffects <- ggpredict(negbin2, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interaction on Sales Quantity", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Question 3: What is the impact of the policy change on online channel returns?
# Dataset: online_sales_returns

# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

# Check distributions of count variables
hist(online_sales_returns_NN$returnvalue)
online_sales_returns_NN$logreturnvalue = log(online_sales_returns_NN$returnvalue)
hist(online_sales_returns_NN$logreturnvalue)
hist(online_sales_returns_NN$returnquantity)
online_sales_returns_NN$logreturnquantity = log(online_sales_returns_NN$returnquantity)
hist(online_sales_returns_NN$logreturnquantity)

hist(online_sales_returns_NN$salesvalue)
online_sales_returns_NN$logsalesvalue = log(online_sales_returns_NN$salesvalue)
hist(online_sales_returns_NN$logsalesvalue)
hist(online_sales_returns_NN$salesquantity)
online_sales_returns_NN$logsalesquantity = log(online_sales_returns_NN$salesquantity)
hist(online_sales_returns_NN$logsalesquantity)

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova


#Multicollinearity check
df=data.frame(online_sales_returns_NN$logsalesquantity, online_sales_returns_NN$logsalesvalue, online_sales_returns_NN$logreturnquantity, online_sales_returns_NN$avg_female, online_sales_returns_NN$avg_age, online_sales_returns_NN$avg_income, online_sales_returns_NN$avg_homeowner, online_sales_returns_NN$avg_residency, online_sales_returns_NN$avg_childowner, online_sales_returns_NN$time, online_sales_returns_NN$new_policy)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. Take out logsalesquantity because of high VIF and doesn't tell as much as logsalesvalue

df2=data.frame(online_sales_returns_NN$logsalesvalue, online_sales_returns_NN$logreturnquantity, online_sales_returns_NN$avg_female, online_sales_returns_NN$avg_age, online_sales_returns_NN$avg_income, online_sales_returns_NN$avg_homeowner, online_sales_returns_NN$avg_residency, online_sales_returns_NN$avg_childowner, online_sales_returns_NN$time, online_sales_returns_NN$new_policy)

cor(df2) # Generates the correlation matrix
vifstep(df2, th=10000) # Calculates VIF scores. Take out logsalesvalue because of high VIF and doesn't tell as much as logreturnquantity about returns

df3=data.frame(online_sales_returns_NN$logreturnquantity, online_sales_returns_NN$avg_female, online_sales_returns_NN$avg_age, online_sales_returns_NN$avg_income, online_sales_returns_NN$avg_homeowner, online_sales_returns_NN$avg_residency, online_sales_returns_NN$avg_childowner, online_sales_returns_NN$time, online_sales_returns_NN$new_policy)

cor(df3) # Generates the correlation matrix
vifstep(df3, th=10000) # Calculates VIF scores. Take out logsalesvalue because of high VIF and doesn't tell as much as logreturnquantity about returns


#First Model
model1=lm(logreturnvalue~time*new_policy+avg_income+avg_childowner,data=online_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.

## Heteroskedasticity
pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals
ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals are clustered in two. WHY?!

gqtest(model1) # Goldfeld-Quandt test is  insignificant, implying there is no heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) #coefficient on interaction term still insignificant

### Model 2: Poisson/Negative Binomial Regression - dependent variable: returnquantity

#Linear
model1=lm(logreturnquantity~time*new_policy+avg_income+avg_childowner,data=online_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

online_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted return quantity for each observation in the data 


ggplot(online_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(online_sales_returns_NN$pred_return_quantity) 
# range is not negative
# Still I think we shouldn't use

#Poisson
poisson3 <- glm(returnquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=online_sales_returns_NN)
stargazer(poisson3,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is 0.15 and is significant. This means that the expected log count when a store experiences the new return policy  is 0.15.

# Model fit assessment 
poisson3a <- glm(returnquantity~1, data=online_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 
lrtest(poisson3, poisson3a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson3, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Return quantity is 100*(1.1583-1)% higher aka 15.83% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1583 times the incident rate for old return policy. But since we don't have model fit this is meaningless.

# Check for heteroscedasticity 
gqtest(poisson3) # Goldfeld-Quandt test is significant, indicates heteroscedasticity
bptest(poisson3) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(poisson3, type="HC1"))) # produces Huber-White robust standard errors

stargazer(poisson3, poisson3,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#Still meaningless because no model fit, still significant.



#meffects <- ggpredict(poisson3, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable





### Part 2: Negative Binomial ###
negbin3 <- glm.nb(returnquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data = online_sales_returns_NN)

stargazer(negbin3,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is not significant 

## Model fit assessment
negbin3a <- glm.nb(returnquantity ~ 1, data = online_sales_returns_NN) 
lrtest(negbin3, negbin3a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions
lrtest(poisson3, negbin3) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.


# Obtain IRRs
stargazer(negbin3, apply.coef = exp, t.auto=F, p.auto = F, title="Regression Results", type="text", column.labels=c("IRRs"), df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Insignificant coefficient on interaction term.

gqtest(negbin3) # Goldfeld-Quandt test is significant, indicates heteroscedasticity

bptest(negbin3) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(negbin3, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin3, negbin3,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#same significance w and w/o Robust St Er. interaction term still insignificant


#meffects <- ggpredict(negbin3, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Question 4: What is the impact of the policy change on physical store returns?
# Dataset: physical_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

# Check distributions of count variables
hist(physical_sales_returns_NN$returnvalue)
physical_sales_returns_NN$logreturnvalue = log(physical_sales_returns_NN$returnvalue)
hist(physical_sales_returns_NN$logreturnvalue)
hist(physical_sales_returns_NN$returnquantity)
physical_sales_returns_NN$logreturnquantity = log(physical_sales_returns_NN$returnquantity)
hist(physical_sales_returns_NN$logreturnquantity)

hist(physical_sales_returns_NN$salesvalue)
physical_sales_returns_NN$logsalesvalue = log(physical_sales_returns_NN$salesvalue)
hist(physical_sales_returns_NN$logsalesvalue)
hist(physical_sales_returns_NN$salesquantity)
physical_sales_returns_NN$logsalesquantity = log(physical_sales_returns_NN$salesquantity)
hist(physical_sales_returns_NN$logsalesquantity)

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(physical_sales_returns_NN$logsalesquantity, physical_sales_returns_NN$logsalesvalue, physical_sales_returns_NN$logreturnquantity, physical_sales_returns_NN$avg_female, physical_sales_returns_NN$avg_age, physical_sales_returns_NN$avg_income, physical_sales_returns_NN$avg_homeowner, physical_sales_returns_NN$avg_residency, physical_sales_returns_NN$avg_childowner, physical_sales_returns_NN$time, physical_sales_returns_NN$new_policy)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. Take out logsalesquantity because of high VIF and doesn't tell as much as logsalesvalue

df2=data.frame(physical_sales_returns_NN$logsalesvalue, physical_sales_returns_NN$logreturnquantity, physical_sales_returns_NN$avg_female, physical_sales_returns_NN$avg_age, physical_sales_returns_NN$avg_income, physical_sales_returns_NN$avg_homeowner, physical_sales_returns_NN$avg_residency, physical_sales_returns_NN$avg_childowner, physical_sales_returns_NN$time, physical_sales_returns_NN$new_policy)

cor(df2) # Generates the correlation matrix
vifstep(df2, th=10000) # Calculates VIF scores. Take out logsalesvalue because of high VIF and doesn't tell as much as logreturnquantity about returns

df3=data.frame(physical_sales_returns_NN$logreturnquantity, physical_sales_returns_NN$avg_female, physical_sales_returns_NN$avg_age, physical_sales_returns_NN$avg_income, physical_sales_returns_NN$avg_homeowner, physical_sales_returns_NN$avg_residency, physical_sales_returns_NN$avg_childowner, physical_sales_returns_NN$time, physical_sales_returns_NN$new_policy)

cor(df3) # Generates the correlation matrix
vifstep(df3, th=10000) # Calculates VIF scores.

model1=lm(logreturnvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals are clustered in two.

gqtest(model1) # Goldfeld-Quandt test is significant, implying there is heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. A return policy change from 90 to 45 days decreases return value by 47.25% in physical stores.

# Poisson and Negative Binomial

#Linear
model1=lm(logreturnquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

physical_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted return quantity for each observation in the data 

ggplot(physical_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(physical_sales_returns_NN$pred_return_quantity) 
#negative range

#Poisson
poisson4 <- glm(returnquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_sales_returns_NN, family="poisson")

stargazer(poisson4,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is .07 and significant. This means that the expected log count when a store experiences the new return policy  is .07. Introducing the new return policy is associated with a .07 increase in log sales quantity.

# Model fit assessment 
poisson4a <- glm(returnquantity~1, data=physical_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson4, poisson4a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson4, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Let's obtain IRRs. Return quantity is 100*(1.077-1)% higher aka 7.7% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.077 times the incident rate for old return policy. Meaningless though because we don't have model fit.

# Check for heteroscedasticity
gqtest(poisson4) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity p-value=1???
bptest(poisson4) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(poisson4, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson4, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson4, poisson4,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#robust standard errors do not change any significance of any coefficients

# Visualization


#meffects <- ggpredict(poisson4, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




### Part 2: Negative Binomial ###
negbin4 <- glm.nb(returnquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_sales_returns_NN)

stargazer(negbin4,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is significant

## Model fit assessment
negbin4a <- glm.nb(returnquantity ~ 1, data = physical_sales_returns_NN) 
lrtest(negbin4, negbin4a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions
lrtest(poisson4, negbin4) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin4, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Looking at IRRs, the coefficient on interaction term is still significant. Return quantity is 100*(0.8199-1)% lower aka 18.01% lower when a store experiences a new return policy.

# Check for heteroscedasticity ## 
gqtest(negbin4) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity (pvalue=1?)
bptest(negbin4) # Breusch-Pagan test is significant, indicates heteroscedasticity
consstder <- sqrt(diag(vcovHC(negbin4, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin4, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin4, negbin4,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


# Visualize output


#meffects <- ggpredict(negbin4, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Question 5: What is the impact of the policy change on product level online sales and returns as well as on product level physical store sales and returns? [Similar to questions 1, 2, 3, and 4 but at product level]

online_pcat_sales_returns_NN$product_category.df <- online_pcat_sales_returns_NN$product_category
physical_pcat_sales_returns_NN$product_category.df <- physical_pcat_sales_returns_NN$product_category

#turning product_category into a factor variable
online_pcat_sales_returns_NN$product_category.df =factor(online_pcat_sales_returns_NN$product_category.df)
physical_pcat_sales_returns_NN$product_category.df =factor(physical_pcat_sales_returns_NN$product_category.df)
#testing if product_category is a factor variable
is.factor(online_pcat_sales_returns_NN$product_category.df)
is.factor(physical_pcat_sales_returns_NN$product_category.df)


# Check distributions of count variables
hist(online_pcat_sales_returns_NN$salesvalue)
online_pcat_sales_returns_NN$logsalesvalue = log(online_pcat_sales_returns_NN$salesvalue)
hist(online_pcat_sales_returns_NN$logsalesvalue)

hist(online_pcat_sales_returns_NN$salesquantity)
online_pcat_sales_returns_NN$logsalesquantity = log(online_pcat_sales_returns_NN$salesquantity)
hist(online_pcat_sales_returns_NN$logsalesquantity)

hist(online_pcat_sales_returns_NN$returnvalue)
online_pcat_sales_returns_NN$logreturnvalue = log(online_pcat_sales_returns_NN$returnvalue)
hist(online_pcat_sales_returns_NN$logreturnvalue)

hist(online_pcat_sales_returns_NN$returnquantity)
online_pcat_sales_returns_NN$logreturnquantity = log(online_pcat_sales_returns_NN$returnquantity)
hist(online_pcat_sales_returns_NN$logreturnquantity)

hist(physical_pcat_sales_returns_NN$salesvalue)
physical_pcat_sales_returns_NN$logsalesvalue = log(physical_pcat_sales_returns_NN$salesvalue)
hist(physical_pcat_sales_returns_NN$logsalesvalue)

hist(physical_pcat_sales_returns_NN$salesquantity)
physical_pcat_sales_returns_NN$logsalesquantity = log(physical_pcat_sales_returns_NN$salesquantity)
hist(physical_pcat_sales_returns_NN$logsalesquantity)

hist(physical_pcat_sales_returns_NN$returnvalue)
physical_pcat_sales_returns_NN$logreturnvalue = log(physical_pcat_sales_returns_NN$returnvalue)
hist(physical_pcat_sales_returns_NN$logreturnvalue)

hist(physical_pcat_sales_returns_NN$returnquantity)
physical_pcat_sales_returns_NN$logreturnquantity = log(physical_pcat_sales_returns_NN$returnquantity)
hist(physical_pcat_sales_returns_NN$logreturnquantity)



## Part 1: What is the impact of the policy change on online channel sales at the product level?
# Dataset: online_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue

#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner+product_category.df, data=online_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova
#Return value and return quantity happen after sales, so it is not good practice to use them to predict sales. 

#Multicollinearity check
df=data.frame(online_pcat_sales_returns_NN$salesquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset

model1=lm(logsalesvalue~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals don't demonstrate pattern.

gqtest(model1) # Goldfeld-Quandt test is  significant, implying there is heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 
#Coefficient still insignificant.

#Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity
#Poisson
#Linear
model1=lm(logsalesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) #Insignificant coefficient.

online_pcat_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 

ggplot(online_pcat_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(online_pcat_sales_returns_NN$pred_sales_quantity)
#Negative range

#Poisson
poisson5 <- glm(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN,family="poisson")
stargazer(poisson5,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is -.05 and significant. This means that the expected log count when a store experiences the new return policy  is -.05. Introducing the new return policy is associated with a .05 decrease in sales quantity.

# Model fit assessment 
poisson5a <- glm(salesquantity~1, data=online_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson5, poisson5a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson5, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(.9554-1)% lower aka 0.46% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 0.9554 times the incident rate for old return policy. 

# Check for heteroscedasticity 
gqtest(poisson5) # Goldfeld-Quandt test is  significant, indicates heteroscedasticity
bptest(poisson5) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(poisson5, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson5, poisson5,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#Coefficient is now insignificant.

#

#meffects <- ggpredict(poisson5, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




### Part 2: Negative Binomial 
negbin5 <- glm.nb(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)

stargazer(negbin5,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is not significant

## Model fit assessment
negbin5a <- glm.nb(salesquantity ~ 1, data = online_pcat_sales_returns_NN) 
lrtest(negbin5, negbin5a) # Model fits the data because LR test statistics is  significant.

lrtest(poisson5, negbin5) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin5, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Still insignificant

# Check for heteroscedasticity 
gqtest(negbin5) # Goldfeld-Quandt test is  significant, indicates  heteroscedasticity
bptest(negbin5) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(negbin5, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(negbin5, negbin5,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Still insignificant

#

#meffects <- ggpredict(negbin5, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable


# Part 2: What is the impact of the policy change on physical store sales at the product level?
# Dataset: physical_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue

#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+product_category.df+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#Multicollinearity check
df=data.frame(physical_pcat_sales_returns_NN$salesquantity, physical_pcat_sales_returns_NN$avg_female, physical_pcat_sales_returns_NN$avg_age, physical_pcat_sales_returns_NN$avg_income, physical_pcat_sales_returns_NN$avg_homeowner, physical_pcat_sales_returns_NN$avg_residency, physical_pcat_sales_returns_NN$avg_childowner, physical_pcat_sales_returns_NN$time, physical_pcat_sales_returns_NN$new_policy, physical_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset

model1=lm(logsalesvalue~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. Let's check heteroskedasticity.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals don't demonstrate pattern.

gqtest(model1) # Goldfeld-Quandt test is  insignificant, implying there is no heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is still significant. Sales value decreased by 8.04%.


#Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity

#Linear
model1=lm(logsalesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

physical_pcat_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 


ggplot(physical_pcat_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) +geom_histogram(binwidth=.5, position="dodge")

range(physical_pcat_sales_returns_NN$pred_sales_quantity) 
#Negative range

#Poisson
poisson6 <- glm(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=physical_pcat_sales_returns_NN)

stargazer(poisson6,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is .31 and significant. This means that the expected log count when a store experiences the new return policy  is .31. Introducing the new return policy is associated with a .31 increase in log sales quantity.

# Model fit assessment 
poisson6a <- glm(salesquantity~1, data=physical_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson6, poisson6a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson6, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(1.3615-1)% higher aka 36.15% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.3615 times the incident rate for old return policy. But no model fit so irrelevant.

# Check for heteroscedasticity 
gqtest(poisson6) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity (pvalue=1)
bptest(poisson6) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(poisson6, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson6, type="HC1"))) # produces Huber-White robust standard errors

stargazer(poisson6, poisson6,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#interaction term coefficient still significant, but no model fit

#

#meffects <- ggpredict(poisson6, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable


### Part 2: Negative Binomial
negbin6 <- glm.nb(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

stargazer(negbin6,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is significant 

## Model fit assessment
negbin6a <- glm.nb(salesquantity ~ 1, data = physical_pcat_sales_returns_NN) 
lrtest(negbin6, negbin6a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson6, negbin6) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin6, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # # Let's obtain IRRs. Sales quantity is 100*(1.1740-1)% higher aka 17.4% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1740 times the incident rate for old return policy.

# Check for heteroscedasticity 
gqtest(negbin6) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity (pvalue=1)
bptest(negbin6) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(negbin6, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin6, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin6, negbin6,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Coefficient still significant.

# 

#meffects <- ggpredict(negbin6, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Part 3: What is the impact of the policy change on online channel returns at the product level?
# Dataset: online_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner+product_category.df, data=online_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(online_pcat_sales_returns_NN$logsalesquantity, online_pcat_sales_returns_NN$logsalesvalue, online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. Take out logsalesquantity because of high VIF and doesn't tell as much as logsalesvalue

df2=data.frame(online_pcat_sales_returns_NN$logsalesvalue, online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df2) # Generates the correlation matrix
vifstep(df2, th=10000) # Calculates VIF scores. Take out logsalesvalue because of high VIF and doesn't tell as much as logreturnquantity about returns

df3=data.frame(online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df3) # Generates the correlation matrix
vifstep(df3, th=10000) # Calculates VIF scores. 

model1=lm(logreturnvalue~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. Let's check heteroskedasticity.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals don't demonstrate pattern.

gqtest(model1) # Goldfeld-Quandt test is  insignificant, implying there is no heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. A return policy change from 90 to 45 days decreases return value by 39.72% in online stores at the product level.


#Model 2: Poisson/Negative Binomial Regression - dependent variable: returnquantity
#Linear
model1=lm(logreturnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

online_pcat_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted return quantity for each observation in the data 

ggplot(online_pcat_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(online_pcat_sales_returns_NN$pred_return_quantity) 
#No negative numbers

poisson7 <- glm(returnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=online_pcat_sales_returns_NN)

stargazer(poisson7,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is 0.11. This means that the expected log count when a store experiences the new return policy  is -0.11. Introducing the new return policy is associated with a 0.11 decrease in log sales quantity.

# Model fit assessment 
poisson7a <- glm(returnquantity~1, data=online_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson7, poisson7a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson7, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Return quantity is 100*(1.1159-1)% higher aka 11.59% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1159 times the incident rate for old return policy. 

# Check for heteroscedasticity
gqtest(poisson7) # Goldfeld-Quandt test is  significant, indicates heteroscedasticity
bptest(poisson7) # Breusch-Pagan test is significant, indicates heteroscedasticity
HWrobstder <- sqrt(diag(vcovHC(poisson7, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson7, poisson7,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#Still significant


#meffects <- ggpredict(poisson7, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




### Part 2: Negative Binomial 
negbin7 <- glm.nb(returnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_pcat_sales_returns_NN)

stargazer(negbin7,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is significant

## Model fit assessment
negbin7a <- glm.nb(returnquantity ~ 1, data = online_pcat_sales_returns_NN) 
lrtest(negbin7, negbin7a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson7, negbin7) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin7, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # still significant

# Check for heteroscedasticity 
gqtest(negbin7) # Goldfeld-Quandt test is  significant, indicates  heteroscedasticity
bptest(negbin7) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(negbin7, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin7, negbin7,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Not significant anymore


#meffects2 <- ggpredict(negbin7, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects2,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="NB Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable



# Part 4: What is the impact of the policy change on physical store returns at the product level?
# Dataset: physical_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+product_category.df+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(physical_pcat_sales_returns_NN$logsalesquantity, physical_pcat_sales_returns_NN$logsalesvalue, physical_pcat_sales_returns_NN$logreturnquantity, physical_pcat_sales_returns_NN$avg_female, physical_pcat_sales_returns_NN$avg_age, physical_pcat_sales_returns_NN$avg_income, physical_pcat_sales_returns_NN$avg_homeowner, physical_pcat_sales_returns_NN$avg_residency, physical_pcat_sales_returns_NN$avg_childowner, physical_pcat_sales_returns_NN$time, physical_pcat_sales_returns_NN$new_policy, physical_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. All under 3.

model1=lm(logreturnvalue~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. 

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals do demonstrate visible pattern.

gqtest(model1) # Goldfeld-Quandt test is  significant, implying heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. A return policy change from 90 to 45 days decreases return value by 49% in physical stores at the product level.

###Model 2: Poisson/Negative Binomial Regression - dependent variable: returnquantity
#Linear
model1=lm(logreturnquantity~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

physical_pcat_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted purchase quantity for each observation in the data 

ggplot(physical_pcat_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) + geom_histogram(binwidth=.5, position="dodge")

range(physical_pcat_sales_returns_NN$pred_return_quantity) 
#no negatives

poisson8 <- glm(returnquantity~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=physical_pcat_sales_returns_NN)
stargazer(poisson8,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is -.04. This means that the expected log count when a store experiences the new return policy  is -.04. Introducing the new return policy is associated with a .04 decrease in log sales quantity.

# Model fit assessment 
poisson8a <- glm(returnquantity~1, data=physical_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson8, poisson8a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson8, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(0.9589-1)% lower aka 4.11% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 9.9589 times the incident rate for old return policy. No model fit though.

# Check for heteroscedasticity
gqtest(poisson8) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity (pvalue=1)
bptest(poisson8) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(poisson8, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson8, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson8, poisson8,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#still significant

#

#meffects <- ggpredict(poisson8, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable


### Part 2: Negative Binomial 
negbin8 <- glm.nb(returnquantity~time*new_policy+logsalesquantity+logsalesvalue+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

stargazer(negbin8,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#The interaction term coefficient is -.12. This means that the expected log count when a store experiences the new return policy  is -.12. Introducing the new return policy is associated with a .12 decrease in log sales quantity.

## Model fit assessment
negbin8a <- glm.nb(returnquantity ~ 1, data = physical_pcat_sales_returns_NN) 
lrtest(negbin8, negbin8a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson8, negbin8) # The insignificant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is more appropriate than the negative binomial model. We will use Poisson. But we do not have model fit for Poisson. So we are out of luck.

#Obtain IRRs
stargazer(negbin8, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Return quantity is 100*(0.8863-1)% higher aka 11.37% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 0.8863 times the incident rate for old return policy. 

# Check for heteroscedasticity 
gqtest(negbin8) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity (pvalue=1)
bptest(negbin8) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(negbin8, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin8, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin8, negbin8,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#same significance


#meffects <- ggpredict(negbin8, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable



#Question 6: How does the impact of the policy change vary across product categories?

#product_category: the indicator variable for product categories
#1-Bridal
#3-Solitaires
#4-Diamond Fashion
#11-Diamond Solitaires Jewelry
#20- Diamond Wedding Band
#2- Gold Wed Bands
#5-Semi Precious
#6-Mens
#7-Gold Earrings
#8-In House Special Event
#9-Beads
#10-Piercings / Close Out 
#12-Gold Chain / Jewelry 
##13-Watches 
#14-Pre-Owned 
#15-Specialized Jewelry
#16-Estate
#17-Events
#18-Trade Ins 
#19-Repair / Warranty
#21-Sterling Silver
                                       
online_pcat_sales_returns_NN$product_category.df <- online_pcat_sales_returns_NN$product_category
physical_pcat_sales_returns_NN$product_category.df <- physical_pcat_sales_returns_NN$product_category
                                       
                                       
#Categorizing product_category by the data description
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 1] <- "Bridal"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 2] <- "Gold Wed Bands"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 3] <- "Solitaires"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 4] <- "Diamond Fashion"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 5] <- "Semi Precious"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 6] <- "Mens"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 7] <- "Gold Earrings"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 8] <- "In House Special Event"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 9] <- "Beads"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 10] <- "Piercings/Close Out"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 11] <- "Diamond Solitaires Jewelry"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 12] <- "Gold Chain/Jewelry"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 13] <- "Watches"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 14] <- "Pre-Owned"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 15] <- "Specialized Jewelry"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 16] <- "Estate"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 17] <- "Events"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 18] <- "Trade Ins"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 19] <- "Repair/Warranty"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 20] <- "Diamond Wedding Band"
online_pcat_sales_returns_NN$product_category.df[online_pcat_sales_returns_NN$product_category.df == 21] <- "Sterling Silver"
                                       
                                       
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 1] <- "Bridal"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 2] <- "Gold Wed Bands"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 3] <- "Solitaires"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 4] <- "Diamond Fashion"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 5] <- "Semi Precious"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 6] <- "Mens"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 7] <- "Gold Earrings"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 8] <- "In House Special Event"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 9] <- "Beads"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 10] <- "Piercings/Close Out"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 11] <- "Diamond Solitaires Jewelry"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 12] <- "Gold Chain/Jewelry"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 13] <- "Watches"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 14] <- "Pre-Owned"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 15] <- "Specialized Jewelry"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 16] <- "Estate"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 17] <- "Events"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 18] <- "Trade Ins"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 19] <- "Repair/Warranty"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 20] <- "Diamond Wedding Band"
physical_pcat_sales_returns_NN$product_category.df[physical_pcat_sales_returns_NN$product_category.df == 21] <- "Sterling Silver"
                                       
#turning product_category into a factor variable
online_pcat_sales_returns_NN$product_category.df =factor(online_pcat_sales_returns_NN$product_category.df)
physical_pcat_sales_returns_NN$product_category.df =factor(physical_pcat_sales_returns_NN$product_category.df)
#testing if product_category is a factor variable
is.factor(online_pcat_sales_returns_NN$product_category.df)
is.factor(physical_pcat_sales_returns_NN$product_category.df)
                                       

                                       
# Check distributions of count variables
hist(online_pcat_sales_returns_NN$salesvalue)
online_pcat_sales_returns_NN$logsalesvalue = log(online_pcat_sales_returns_NN$salesvalue)
hist(online_pcat_sales_returns_NN$logsalesvalue)
                                       
hist(online_pcat_sales_returns_NN$salesquantity)
online_pcat_sales_returns_NN$logsalesquantity = log(online_pcat_sales_returns_NN$salesquantity)
hist(online_pcat_sales_returns_NN$logsalesquantity)
                                       
hist(online_pcat_sales_returns_NN$returnvalue)
online_pcat_sales_returns_NN$logreturnvalue = log(online_pcat_sales_returns_NN$returnvalue)
hist(online_pcat_sales_returns_NN$logreturnvalue)
                                       
hist(online_pcat_sales_returns_NN$returnquantity)
online_pcat_sales_returns_NN$logreturnquantity = log(online_pcat_sales_returns_NN$returnquantity)
hist(online_pcat_sales_returns_NN$logreturnquantity)
                                       
hist(physical_pcat_sales_returns_NN$salesvalue)
physical_pcat_sales_returns_NN$logsalesvalue = log(physical_pcat_sales_returns_NN$salesvalue)
hist(physical_pcat_sales_returns_NN$logsalesvalue)

hist(physical_pcat_sales_returns_NN$salesquantity)
physical_pcat_sales_returns_NN$logsalesquantity = log(physical_pcat_sales_returns_NN$salesquantity)
hist(physical_pcat_sales_returns_NN$logsalesquantity)
                                       
hist(physical_pcat_sales_returns_NN$returnvalue)
physical_pcat_sales_returns_NN$logreturnvalue = log(physical_pcat_sales_returns_NN$returnvalue)
hist(physical_pcat_sales_returns_NN$logreturnvalue)
                                       
hist(physical_pcat_sales_returns_NN$returnquantity)
physical_pcat_sales_returns_NN$logreturnquantity = log(physical_pcat_sales_returns_NN$returnquantity)
hist(physical_pcat_sales_returns_NN$logreturnquantity)
                                       
                                       
                                       
## Part 1: What is the impact of the policy change on online channel sales at the product level?
# Dataset: online_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue
                                       
#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner+product_category.df, data=online_pcat_sales_returns_NN)
                                       
step <- stepAIC(model0,direction="both")
step$anova
#Return value and return quantity happen after sales, so it is not good practice to use them to predict sales. 
                                       
#Multicollinearity check
df=data.frame(online_pcat_sales_returns_NN$salesquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)
                                       
cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset
                                       
model1=lm(logsalesvalue~time*new_policy*product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.
                                       
# Heteroskedasticity
                                       
pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals
                                       
ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals demonstrate some visible pattern but not important.
                                       
gqtest(model1) # Goldfeld-Quandt test is  significant, implying there is heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity
                                       
consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 
                                       
stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) #Significant coefficients: .4603 for sterling silver, .4809 for semi precious, -1.9372 for pre-owned, 1.0598 for gold wed bands, 0.6859 for gold earrings
                                       
                                       
#Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity
#Poisson
#Linear
model1=lm(logsalesquantity~time*new_policy*product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) #Insignificant coefficient.
                                       
online_pcat_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 
                                       
ggplot(online_pcat_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) + geom_histogram(binwidth=.5, position="dodge")
                                       
range(online_pcat_sales_returns_NN$pred_sales_quantity)
#Negative range
                                       
#Poisson
poisson5 <- glm(salesquantity~time*new_policy*product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN,family="poisson")
stargazer(poisson5,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is -.05 and significant. This means that the expected log count when a store experiences the new return policy  is -.05. Introducing the new return policy is associated with a .05 decrease in sales quantity.
                                       
# Model fit assessment 
poisson5a <- glm(salesquantity~1, data=online_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 
                                       
lrtest(poisson5, poisson5a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.
                                       
stargazer(poisson5, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(.9554-1)% lower aka 0.46% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 0.9554 times the incident rate for old return policy. 
                                       
# Check for heteroscedasticity 
gqtest(poisson5) # Goldfeld-Quandt test is  significant, indicates heteroscedasticity
bptest(poisson5) # Breusch-Pagan test is significant, indicates heteroscedasticity
                                       
HWrobstder <- sqrt(diag(vcovHC(poisson5, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson5, poisson5,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#Coefficient is now insignificant.
                                       
#
#meffects <- ggpredict(poisson5, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  
                                       
#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
#labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
#labs(colour="Changed policy?") + 
#scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
#scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
#theme(axis.title.x=element_blank())# make the plot more self-readable
                                       
                                       
                                
### Part 2: Negative Binomial 
negbin5 <- glm.nb(salesquantity~time*new_policy*product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
                                       
stargazer(negbin5,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is not significant
                                       
## Model fit assessment
negbin5a <- glm.nb(salesquantity ~ 1, data = online_pcat_sales_returns_NN) 
lrtest(negbin5, negbin5a) # Model fits the data because LR test statistics is  significant.
                                       
lrtest(poisson5, negbin5) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.
                                       
# Obtain IRRs
stargazer(negbin5, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
                                       
                                       
# Check for heteroscedasticity 
gqtest(negbin5) # Goldfeld-Quandt test is  significant, indicates  heteroscedasticity
bptest(negbin5) # Breusch-Pagan test is significant, indicates heteroscedasticity
                                       
HWrobstder <- sqrt(diag(vcovHC(negbin5, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(negbin5, negbin5,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#0.86 for pre-owned, -2.99 for gold earrings
                                       
# 
                                       
#meffects <- ggpredict(negbin5, terms=c("time","new_policy", "product_category.df"))
# generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())+facet_wrap(~facet)# make the plot more self-readable



# Part 2: What is the impact of the policy change on physical store sales at the product level?
# Dataset: physical_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: salesvalue

#Let's see what R thinks is a good model
model0 <- lm(logsalesvalue~time+new_policy+product_category.df+logreturnvalue+logreturnquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#Multicollinearity check
df=data.frame(physical_pcat_sales_returns_NN$salesquantity, physical_pcat_sales_returns_NN$avg_female, physical_pcat_sales_returns_NN$avg_age, physical_pcat_sales_returns_NN$avg_income, physical_pcat_sales_returns_NN$avg_homeowner, physical_pcat_sales_returns_NN$avg_residency, physical_pcat_sales_returns_NN$avg_childowner, physical_pcat_sales_returns_NN$time, physical_pcat_sales_returns_NN$new_policy, physical_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores . All VIFs are less than 3, indicating there is no multicollinearity in the dataset

model1=lm(logsalesvalue~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals demonstrate some visible pattern.

gqtest(model1) # Goldfeld-Quandt test is  insignificant, implying there is no heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is still insignificant.


#Model 2: Poisson/Negative Binomial Regression - dependent variable: salesquantity

#Linear
model1=lm(logsalesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

physical_pcat_sales_returns_NN$pred_sales_quantity<-predict(model1) # let's look at the predicted sales quantity for each observation in the data 


ggplot(physical_pcat_sales_returns_NN, aes(pred_sales_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(physical_pcat_sales_returns_NN$pred_sales_quantity) 
#Negative range

#Poisson
poisson6 <- glm(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=physical_pcat_sales_returns_NN)

stargazer(poisson6,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is .31 and significant. This means that the expected log count when a store experiences the new return policy  is .31. Introducing the new return policy is associated with a .31 increase in log sales quantity.

# Model fit assessment 
poisson6a <- glm(salesquantity~1, data=physical_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson6, poisson6a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson6, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(1.3615-1)% higher aka 36.15% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.3615 times the incident rate for old return policy. But no model fit so irrelevant.

# Check for heteroscedasticity 
gqtest(poisson6) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity (pvalue=1)
bptest(poisson6) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(poisson6, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson6, type="HC1"))) # produces Huber-White robust standard errors

stargazer(poisson6, poisson6,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#interaction term coefficient still significant, but no model fit

#
#meffects <- ggpredict(poisson6, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable



### Part 2: Negative Binomial ###
negbin6 <- glm.nb(salesquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

stargazer(negbin6,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is significant 

## Model fit assessment
negbin6a <- glm.nb(salesquantity ~ 1, data = physical_pcat_sales_returns_NN) 
lrtest(negbin6, negbin6a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson6, negbin6) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin6, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # # Let's obtain IRRs. Sales quantity is 100*(1.1721-1)% higher aka 17.21% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1721 times the incident rate for old return policy.

# Check for heteroscedasticity 
gqtest(negbin6) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity (pvalue=1)
bptest(negbin6) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(negbin6, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin6, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin6, negbin6,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Coefficient still significant.

# 

#meffects <- ggpredict(negbin6, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Sales Quantity Product Level", y = "Log of sales quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable



# Part 3: What is the impact of the policy change on online channel returns at the product level?
# Dataset: online_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(online_pcat_sales_returns_NN$logsalesquantity, online_pcat_sales_returns_NN$logsalesvalue, online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. Take out logsalesquantity because of high VIF and doesn't tell as much as logsalesvalue

df2=data.frame(online_pcat_sales_returns_NN$logsalesvalue, online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df2) # Generates the correlation matrix
vifstep(df2, th=10000) # Calculates VIF scores. Take out logsalesvalue because of high VIF and doesn't tell as much as logreturnquantity about returns

df3=data.frame(online_pcat_sales_returns_NN$logreturnquantity, online_pcat_sales_returns_NN$avg_female, online_pcat_sales_returns_NN$avg_age, online_pcat_sales_returns_NN$avg_income, online_pcat_sales_returns_NN$avg_homeowner, online_pcat_sales_returns_NN$avg_residency, online_pcat_sales_returns_NN$avg_childowner, online_pcat_sales_returns_NN$time, online_pcat_sales_returns_NN$new_policy, online_pcat_sales_returns_NN$product_category)

cor(df3) # Generates the correlation matrix
vifstep(df3, th=10000) # Calculates VIF scores. 

model1=lm(logreturnvalue~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is insignificant. Let's check heteroskedasticity.

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals demonstrate visible pattern.

gqtest(model1) # Goldfeld-Quandt test is  insignificant, implying there is no heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant now. A return policy change from 90 to 45 days decreases return value by 1.6% in online stores at the product level.


#Model 2: Poisson/Negative Binomial Regression - dependent variable: returnquantity
#Linear
model1=lm(logreturnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

online_pcat_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted return quantity for each observation in the data 

ggplot(online_pcat_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) +
  geom_histogram(binwidth=.5, position="dodge")

range(online_pcat_sales_returns_NN$pred_return_quantity) 
#No negative numbers

poisson7 <- glm(returnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=online_pcat_sales_returns_NN)

stargazer(poisson7,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is 0.11. This means that the expected log count when a store experiences the new return policy  is -0.11. Introducing the new return policy is associated with a 0.11 decrease in log sales quantity.

# Model fit assessment 
poisson7a <- glm(returnquantity~1, data=online_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson7, poisson7a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson7, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Return quantity is 100*(1.1159-1)% higher aka 11.59% higher when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 1.1159 times the incident rate for old return policy. 

# Check for heteroscedasticity
gqtest(poisson7) # Goldfeld-Quandt test is  significant, indicates heteroscedasticity
bptest(poisson7) # Breusch-Pagan test is significant, indicates heteroscedasticity
HWrobstder <- sqrt(diag(vcovHC(poisson7, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson7, poisson7,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#Still significant


#meffects <- ggpredict(poisson7, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable



### Part 2: Negative Binomial 
negbin7 <- glm.nb(returnquantity~time*new_policy+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=online_pcat_sales_returns_NN)

stargazer(negbin7,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))# The coefficient on interaction term is insignificant

## Model fit assessment
negbin7a <- glm.nb(returnquantity ~ 1, data = online_pcat_sales_returns_NN) 
lrtest(negbin7, negbin7a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson7, negbin7) # The significant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is less appropriate than the negative binomial model. We will use Negative Binomial model.

# Obtain IRRs
stargazer(negbin7, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # still insignificant

# Check for heteroscedasticity 
gqtest(negbin7) # Goldfeld-Quandt test is  significant, indicates  heteroscedasticity
bptest(negbin7) # Breusch-Pagan test is significant, indicates heteroscedasticity

HWrobstder <- sqrt(diag(vcovHC(negbin7, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin7, negbin7,  
          se=list(NULL, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#Significant now. 
exp(.05)
#Return quantity is 100*(1.0512-1)% higher aka 5.12% higher when the return policy goes from 90 to 45 days.



#meffects2 <- ggpredict(negbin7, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects2,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="NB Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




# Part 4: What is the impact of the policy change on physical store returns at the product level?
# Dataset: physical_pcat_sales_returns
# Model 1: OLS (interacting time*new_policy) - dependent variable: returnvalue

#Let's see what R thinks is a good model
model0 <- lm(logreturnvalue~time+new_policy+product_category.df+logsalesvalue+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

step <- stepAIC(model0,direction="both")
step$anova

#ANOVA takes out our demographic variables which we would like to keep.

#Multicollinearity check
df=data.frame(physical_pcat_sales_returns_NN$logsalesquantity, physical_pcat_sales_returns_NN$logsalesvalue, physical_pcat_sales_returns_NN$logreturnquantity, physical_pcat_sales_returns_NN$avg_female, physical_pcat_sales_returns_NN$avg_age, physical_pcat_sales_returns_NN$avg_income, physical_pcat_sales_returns_NN$avg_homeowner, physical_pcat_sales_returns_NN$avg_residency, physical_pcat_sales_returns_NN$avg_childowner, physical_pcat_sales_returns_NN$time, physical_pcat_sales_returns_NN$new_policy, physical_pcat_sales_returns_NN$product_category)

cor(df) # Generates the correlation matrix
vifstep(df, th=10000) # Calculates VIF scores. All under 3.

model1=lm(logreturnvalue~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. 

# Heteroskedasticity

pred<-predict(model1) #obtain fitted values
res=resid(model1) # obtain residuals

ggplot(df, aes(y=res, x=pred)) + geom_point(size=2.5) # Let's check heteroscedasticity visually first. Residuals do demonstrate visible pattern.

gqtest(model1) # Goldfeld-Quandt test is  significant, implying heteroscedasticity
bptest(model1) # Breusch-Pagan test is significant, implying heteroscedasticity

consstder <- sqrt(diag(vcovHC(model1, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(model1, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(model1, model1,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) # Coefficient on interaction term is significant. A return policy change from 90 to 45 days decreases return value by 1.35% in physical stores at the product level.

###Model 2: Poisson/Negative Binomial Regression - dependent variable: returnquantity
#Linear
model1=lm(logreturnquantity~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN)
stargazer(model1, 
          title="Regression Results", type="text", 
          column.labels=c("OLS with Interaction"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

physical_pcat_sales_returns_NN$pred_return_quantity<-predict(model1) # let's look at the predicted purchase quantity for each observation in the data 

ggplot(physical_pcat_sales_returns_NN, aes(pred_return_quantity, fill = new_policy)) + geom_histogram(binwidth=.5, position="dodge")

range(physical_pcat_sales_returns_NN$pred_return_quantity) 
#no negatives

poisson8 <- glm(returnquantity~time*new_policy+logsalesvalue+logsalesquantity+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,family="poisson", data=physical_pcat_sales_returns_NN)
stargazer(poisson8,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 
#The interaction term coefficient is -.04. This means that the expected log count when a store experiences the new return policy  is -.04. Introducing the new return policy is associated with a .04 decrease in log sales quantity.

# Model fit assessment 
poisson8a <- glm(returnquantity~1, data=physical_pcat_sales_returns_NN, family="poisson") # This is the command to run a logit on null model 

lrtest(poisson8, poisson8a) # We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

stargazer(poisson8, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(0.9589-1)% lower aka 4.11% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 9.9589 times the incident rate for old return policy. No model fit though.

# Check for heteroscedasticity
gqtest(poisson8) # Goldfeld-Quandt test is not significant, indicates no heteroscedasticity (pvalue=1)
bptest(poisson8) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(poisson8, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(poisson8, type="HC1"))) # produces Huber-White robust standard errors
stargazer(poisson8, poisson8,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
#still significant

#
#meffects <- ggpredict(poisson8, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable




### Part 2: Negative Binomial 
negbin8 <- glm.nb(returnquantity~time*new_policy+logsalesquantity+logsalesvalue+product_category.df+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner, data=physical_pcat_sales_returns_NN)

stargazer(negbin8,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#The interaction term coefficient is -.04. This means that the expected log count when a store experiences the new return policy  is -.04. Introducing the new return policy is associated with a .04 decrease in log sales quantity.

## Model fit assessment
negbin8a <- glm.nb(returnquantity ~ 1, data = physical_pcat_sales_returns_NN) 
lrtest(negbin8, negbin8a) # # Model fits the data because LR test statistics is  significant.

## Choosing between Poisson and Negative Binomial regressions

lrtest(poisson8, negbin8) # The insignificant p-value indicates that the Poisson model, which holds the dispersion parameter at constant, is more appropriate than the negative binomial model. We will use Poisson. But we do not have model fit for Poisson. So we are out of luck.

#Obtain IRRs
stargazer(negbin8, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# Let's obtain IRRs. Sales quantity is 100*(0.8950-1)% higher aka 10.50% lower when a store experiences a new return policy. The output indicates that the incident rate for a new return policy is 0.8950 times the incident rate for old return policy. 

# Check for heteroscedasticity 
gqtest(negbin8) # Goldfeld-Quandt test is NOT significant, indicates NO heteroscedasticity (pvalue=1)
bptest(negbin8) # Breusch-Pagan test is significant, indicates heteroscedasticity

consstder <- sqrt(diag(vcovHC(negbin8, type="const"))) # produces normal standard errors
HWrobstder <- sqrt(diag(vcovHC(negbin8, type="HC1"))) # produces Huber-White robust standard errors 

stargazer(negbin8, negbin8,  
          se=list(consstder, HWrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))
#same significance


#meffects <- ggpredict(negbin8, terms=c("time","new_policy")) # generates a tidy data frame at two different values of time  

#ggplot(meffects,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  #labs(title="Time/Policy Interation on Return Quantity Product Level", y = "Log of return quantity") +
  #labs(colour="Changed policy?") + 
  #scale_colour_discrete(labels=c("No: Brand 10", "Yes: Brands 2 and 6")) +
  #scale_x_continuous(breaks=c(0,1), labels=c("Before Policy Change", "After Policy Change")) +
  #theme(axis.title.x=element_blank())# make the plot more self-readable


#create subsets for each product category
list_online_prodsales <- list()
for(i in 1:21) { 
  A <-paste("online_pcat_sales_returns_NN",i,sep="")
  assign(A, online_pcat_sales_returns_NN[online_pcat_sales_returns_NN$product_category == i,]) 
  B <- get(paste("online_pcat_sales_returns_NN",i,sep=""))
  list_online_prodsales[[A]] <- B
}

list_physical_prodsales <- list()
for(i in 1:21) { 
  A <-paste("physical_pcat_sales_returns_NN",i,sep="")
  assign(A, physical_pcat_sales_returns_NN[physical_pcat_sales_returns_NN$product_category == i,]) 
  B <- get(paste("physical_pcat_sales_returns_NN",i,sep=""))
  list_physical_prodsales[[A]] <- B
}

#PHYSICAL SALES VALUE
modela1=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN1)

stargazer(modela1,
          se=list(cse(modela1)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela2=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN2)

stargazer(modela2,
          se=list(cse(modela2)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela3=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN3)

stargazer(modela3,
          se=list(cse(modela3)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela4=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN4)

stargazer(modela4,
          se=list(cse(modela4)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela5=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN5)

stargazer(modela5,
          se=list(cse(modela5)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela6=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN6)

stargazer(modela6,
          se=list(cse(modela6)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela7=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN7)

stargazer(modela7,
          se=list(cse(modela7)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela8=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN8)

stargazer(modela8,
          se=list(cse(modela8)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela9=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN9)

stargazer(modela9,
          se=list(cse(modela9)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela10=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN10)

stargazer(modela10,
          se=list(cse(modela10)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela11=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN11)

stargazer(modela11,
          se=list(cse(modela11)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela12=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN12)

stargazer(modela12,
          se=list(cse(modela12)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela13=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN13)

stargazer(modela13,
          se=list(cse(modela13)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela14=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN14)

stargazer(modela14,
          se=list(cse(modela14)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela15=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN15)

stargazer(modela15,
          se=list(cse(modela15)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela16=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN16)

stargazer(modela16,
          se=list(cse(modela16)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela17=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN17)

stargazer(modela17,
          se=list(cse(modela17)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modela18=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN18)

#stargazer(modela18,
#          se=list(cse(modela18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical SV - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela19=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN19)

stargazer(modela19,
          se=list(cse(modela19)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 19"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela20=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN20)

stargazer(modela20,
          se=list(cse(modela20)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modela21=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN21)

stargazer(modela21,
          se=list(cse(modela21)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SV - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))



#PHYSICAL RETURN VALUE

modelb1=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN1)

stargazer(modelb1,
          se=list(cse(modelb1)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb2=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN2)

stargazer(modelb2,
          se=list(cse(modelb2)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb3=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN3)

stargazer(modelb3,
          se=list(cse(modelb3)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb4=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN4)

stargazer(modelb4,
          se=list(cse(modelb4)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb5=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN5)

stargazer(modelb5,
          se=list(cse(modelb5)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb6=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN6)

stargazer(modelb6,
          se=list(cse(modelb6)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb7=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN7)

stargazer(modelb7,
          se=list(cse(modelb7)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb8=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN8)

stargazer(modelb8,
          se=list(cse(modelb8)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb9=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN9)

stargazer(modelb9,
          se=list(cse(modelb9)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb10=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN10)

stargazer(modelb10,
          se=list(cse(modelb10)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb11=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN11)

stargazer(modelb11,
          se=list(cse(modelb11)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb12=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN12)

stargazer(modelb12,
          se=list(cse(modelb12)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb13=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN13)

stargazer(modelb13,
          se=list(cse(modelb13)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb14=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN14)

stargazer(modelb14,
          se=list(cse(modelb14)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb15=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN15)

stargazer(modelb15,
          se=list(cse(modelb15)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb16=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN16)

stargazer(modelb16,
          se=list(cse(modelb16)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb17=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN17)

stargazer(modelb17,
          se=list(cse(modelb17)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelb18=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN18)

#stargazer(modelb18,
#          se=list(cse(modelb18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical RV - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb19=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN19)

stargazer(modelb19,
          se=list(cse(modelb19)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 19"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb20=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN20)

stargazer(modelb20,
          se=list(cse(modelb20)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelb21=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN21)

stargazer(modelb21,
          se=list(cse(modelb21)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RV - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


#ONLINE SALES VALUE

modelc1=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN1)

stargazer(modelc1,
          se=list(cse(modelc1)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc2=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN2)

stargazer(modelc2,
          se=list(cse(modelc2)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc3=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN3)

stargazer(modelc3,
          se=list(cse(modelc3)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc4=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN4)

stargazer(modelc4,
          se=list(cse(modelc4)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc5=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN5)

stargazer(modelc5,
          se=list(cse(modelc5)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc6=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN6)

stargazer(modelc6,
          se=list(cse(modelc6)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc7=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN7)

stargazer(modelc7,
          se=list(cse(modelc7)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc8=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN8)

stargazer(modelc8,
          se=list(cse(modelc8)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc9=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN9)

stargazer(modelc9,
          se=list(cse(modelc9)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc10=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN10)

stargazer(modelc10,
          se=list(cse(modelc10)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc11=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN11)

stargazer(modelc11,
          se=list(cse(modelc11)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc12=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN12)

stargazer(modelc12,
          se=list(cse(modelc12)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc13=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN13)

stargazer(modelc13,
          se=list(cse(modelc13)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc14=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN14)

stargazer(modelc14,
          se=list(cse(modelc14)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc15=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN15)

stargazer(modelc15,
          se=list(cse(modelc15)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc16=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN16)

stargazer(modelc16,
          se=list(cse(modelc16)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc17=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN17)

stargazer(modelc17,
          se=list(cse(modelc17)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelc18=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN18)

#stargazer(modelc18,
#          se=list(cse(modelc18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online SV - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelc19=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN19)

#stargazer(modelc19,
#          se=list(cse(modelc19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online SV - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc20=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN20)

stargazer(modelc20,
          se=list(cse(modelc20)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelc21=lm(logsalesvalue~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN21)

stargazer(modelc21,
          se=list(cse(modelc21)),
          title="Regression Results", type="text", 
          column.labels=c("Online SV - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


#ONLINE RETURN VALUE

modeld1=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN1)

stargazer(modeld1,
          se=list(cse(modeld1)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld2=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN2)

stargazer(modeld2,
          se=list(cse(modeld2)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld3=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN3)

stargazer(modeld3,
          se=list(cse(modeld3)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld4=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN4)

stargazer(modeld4,
          se=list(cse(modeld4)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 4 "),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld5=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN5)

stargazer(modeld5,
          se=list(cse(modeld5)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld6=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN6)

stargazer(modeld6,
          se=list(cse(modeld6)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld7=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN7)

stargazer(modeld7,
          se=list(cse(modeld7)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld8=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN8)

stargazer(modeld8,
          se=list(cse(modeld8)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld9=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN9)

stargazer(modeld9,
          se=list(cse(modeld9)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld10=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN10)

stargazer(modeld10,
          se=list(cse(modeld10)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld11=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN11)

stargazer(modeld11,
          se=list(cse(modeld11)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld12=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN12)

stargazer(modeld12,
          se=list(cse(modeld12)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld13=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN13)

stargazer(modeld13,
          se=list(cse(modeld13)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld14=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN14)

stargazer(modeld14,
          se=list(cse(modeld14)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld15=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN15)

stargazer(modeld15,
          se=list(cse(modeld15)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld16=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN16)

stargazer(modeld16,
          se=list(cse(modeld16)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld17=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN17)

stargazer(modeld17,
          se=list(cse(modeld17)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modeld18=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN18)

#stargazer(modeld18,
#          se=list(cse(modeld18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online RV - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modeld19=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN19)

#stargazer(modeld19,
#          se=list(cse(modeld19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online RV - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld20=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN20)

stargazer(modeld20,
          se=list(cse(modeld20)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modeld21=lm(logreturnvalue~time*new_policy+logsalesvalue+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN21)

stargazer(modeld21,
          se=list(cse(modeld21)),
          title="Regression Results", type="text", 
          column.labels=c("Online RV - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


#PHYSICAL SALES QUANTITY

modele1=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN1)

stargazer(modele1,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele1)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele2=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN2)

stargazer(modele2,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele2)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele3=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN3)

stargazer(modele3,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele3)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele4=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN4)

stargazer(modele4,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele4)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele5=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN5)

stargazer(modele5,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele5)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele6=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN6)

stargazer(modele6,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele6)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele7=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN7)

stargazer(modele7,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele7)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele8=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN8)

stargazer(modele8,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele8)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele9=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN9)

stargazer(modele9,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele9)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele10=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN10)

stargazer(modele10,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele10)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele11=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN11)

stargazer(modele11,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele11)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele12=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN12)

stargazer(modele12,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele12)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele13=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN13)

stargazer(modele13,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele13)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele14=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN14)

stargazer(modele14,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele14)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele15=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN15)

stargazer(modele15,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele15)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele16=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN16)

stargazer(modele16,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele16)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele17=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN17)

stargazer(modele17,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele17)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modele18=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN18)

#stargazer(modele18,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modele18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical SQ - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modele19=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN19)

#stargazer(modele19,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modele19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical SQ - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele20=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN20)

stargazer(modele20,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele20)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modele21=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN21)

stargazer(modele21,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modele21)),
          title="Regression Results", type="text", 
          column.labels=c("Physical SQ - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))



#PHYSICAL RETURN QUANTITY

modelf1=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN1)

stargazer(modelf1,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf1)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf2=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN2)

stargazer(modelf2,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf2)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf3=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN3)

stargazer(modelf3,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf3)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf4=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN4)

stargazer(modelf4,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf4)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf5=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN5)

stargazer(modelf5,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf5)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf6=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN6)

stargazer(modelf6,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf6)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf7=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN7)

stargazer(modelf7,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf7)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf8=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN8)

stargazer(modelf8,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf8)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf9=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN9)

stargazer(modelf9,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf9)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf10=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN10)

stargazer(modelf10,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf10)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf11=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN11)

stargazer(modelf11,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf11)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf12=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN12)

stargazer(modelf12,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf12)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf13=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN13)

stargazer(modelf13,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf13)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf14=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN14)

stargazer(modelf14,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf14)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf15=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN15)

stargazer(modelf15,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf15)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf16=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN16)

stargazer(modelf16,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf16)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf17=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN17)

stargazer(modelf17,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf17)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelf18=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN18)

#stargazer(modelf18,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelf18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical RQ - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelf19=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN19)

#stargazer(modelf19,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelf19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Physical RQ - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf20=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN20)

stargazer(modelf20,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf20)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelf21=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=physical_pcat_sales_returns_NN21)

stargazer(modelf21,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelf21)),
          title="Regression Results", type="text", 
          column.labels=c("Physical RQ - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


#ONLINE SALES QUANTITY

modelg1=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN1)

stargazer(modelg1,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg1)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg2=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN2)

stargazer(modelg2,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg2)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg3=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN3)

stargazer(modelg3,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg3)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg4=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN4)

stargazer(modelg4,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg4)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 4"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg5=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN5)

stargazer(modelg5,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg5)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg6=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN6)

stargazer(modelg6,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg6)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg7=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN7)

stargazer(modelg7,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg7)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg8=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN8)

stargazer(modelg8,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg8)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg9=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN9)

stargazer(modelg9,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg9)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg10=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN10)

stargazer(modelg10,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg10)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg11=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN11)

stargazer(modelg11,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg11)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg12=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN12)

stargazer(modelg12,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg12)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg13=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN13)

stargazer(modelg13,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg13)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg14=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN14)

stargazer(modelg14,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg14)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg15=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN15)

stargazer(modelg15,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg15)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg16=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN16)

stargazer(modelg16,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg16)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg17=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN17)

stargazer(modelg17,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg17)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelg18=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN18)

#stargazer(modelg18,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelg18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online SQ - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelg19=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN19)

#stargazer(modelg19,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelg19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online SQ - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg20=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN20)

stargazer(modelg20,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg20)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelg21=glm.nb(salesquantity~time*new_policy+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN21)

stargazer(modelg21,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelg21)),
          title="Regression Results", type="text", 
          column.labels=c("Online SQ - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


#ONLINE RETURN QUANTITY

modelh1=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN1)

stargazer(modelh1,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh1)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh2=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN2)

stargazer(modelh2,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh2)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh3=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN3)

stargazer(modelh3,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh3)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 3"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh4=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN4)

stargazer(modelh4,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh4)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 4 "),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh5=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN5)

stargazer(modelh5,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh5)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh6=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN6)

stargazer(modelh6,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh6)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 6"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh7=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN7)

stargazer(modelh7,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh7)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 7"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh8=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN8)

stargazer(modelh8,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh8)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 8"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh9=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN9)

stargazer(modelh9,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh9)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 9"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh10=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN10)

stargazer(modelh10,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh10)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 10"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh11=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN11)

stargazer(modelh11,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh11)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 11"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh12=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN12)

stargazer(modelh12,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh12)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 12"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh13=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN13)

stargazer(modelh13,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh13)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 13"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh14=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN14)

stargazer(modelh14,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh14)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 14"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh15=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN15)

stargazer(modelh15,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh15)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 15"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh16=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN16)

stargazer(modelh16,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh16)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 16"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh17=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN17)

stargazer(modelh17,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh17)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 17"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelh18=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN18)

#stargazer(modelh18,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelh18)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online RQ - 18"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#modelh19=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN19)

#stargazer(modelh19,
#          apply.coef = exp, t.auto=F, p.auto = F,
#          se=list(cse(modelh19)),
#          title="Regression Results", type="text", 
#          column.labels=c("Online RQ - 19"),
#          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh20=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN20)

stargazer(modelh20,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh20)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 20"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

modelh21=glm.nb(returnquantity~time*new_policy+logsalesquantity+avg_residency+avg_homeowner+avg_female+avg_age+avg_income+avg_childowner,data=online_pcat_sales_returns_NN21)

stargazer(modelh21,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(cse(modelh21)),
          title="Regression Results", type="text", 
          column.labels=c("Online RQ - 21"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

