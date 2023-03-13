# libraries
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(olsrr)
library(car)

# read database
jobcorps <- read_dta("data/jobcorps.dta")
View(jobcorps)

controls <- c("white", "black","hispanic","eng_sec_language","havekids_at_bline","male","educlevel_at_bline","hsgrad_at_bline",   "ged_at_bline","mos_wrk_lyr_bline","earn_lyr_bline","wage_bline","anywlfr_bline","alcohol_bline","arrest_bline")
controls 

#reduce database
jobcorps <- jobcorps %>% select(controls)
View(jobcorps)

jobcorps <- jobcorps %>% drop_na()

#correlation
cormatrix <- cor(jobcorps)[11,]
cormatrix

#factors
jobcorps$white <- as.factor(jobcorps$white)
jobcorps$black <- as.factor(jobcorps$black)
jobcorps$hispanic <- as.factor(jobcorps$hispanic)
jobcorps$eng_sec_language <- as.factor(jobcorps$eng_sec_language)
jobcorps$havekids_at_bline <- as.factor(jobcorps$havekids_at_bline)
jobcorps$male <- as.factor(jobcorps$male)
jobcorps$hsgrad_at_bline <- as.factor(jobcorps$hsgrad_at_bline)
jobcorps$ged_at_bline <- as.factor(jobcorps$ged_at_bline)
jobcorps$anywlfr_bline <- as.factor(jobcorps$anywlfr_bline)
jobcorps$alcohol_bline <- as.factor(jobcorps$alcohol_bline)

# ols ---------------------

# model 1: all possible variables
model1 <- lm(data = jobcorps, formula = earn_lyr_bline ~ white + black + hispanic +
              eng_sec_language + havekids_at_bline + male + educlevel_at_bline +
              hsgrad_at_bline + ged_at_bline + mos_wrk_lyr_bline + wage_bline +
              anywlfr_bline + alcohol_bline + arrest_bline)
summary(model1)
# ### residuals
plot(model1$residuals)
hist(model1$residuals)
# ### confidence intervals
confint.lm(model1)
# ### log likelihood
logLik(model1)
# ### VIF
vif(model1)

# all possible combinations
# this commands takes a lot of time to run!
k <- ols_step_all_possible(model1)
print(k)
plot(k)

# model 2: all significative
model2 <- lm(data = jobcorps, formula = earn_lyr_bline ~ havekids_at_bline + male +
               mos_wrk_lyr_bline + wage_bline +
               anywlfr_bline + alcohol_bline + arrest_bline)
summary(model2)
# ### residuals
plot(model2$residuals)
hist(model2$residuals)
# ### confidence intervals
confint.lm(model2)
# ### log likelihood
logLik(model2)
# ### VIF
vif(model2)

