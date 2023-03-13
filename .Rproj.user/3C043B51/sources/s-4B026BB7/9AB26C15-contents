# libraries
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)

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

# ols function
model <- lm(data = jobcorps, formula = earn_lyr_bline ~ white + black + hispanic +
              eng_sec_language + havekids_at_bline + male + educlevel_at_bline +
              hsgrad_at_bline + ged_at_bline + mos_wrk_lyr_bline + wage_bline +
              anywlfr_bline + alcohol_bline + arrest_bline)
summary(model)
plot(model$residuals)
logLik(model)

# ols function all significative
model2 <- lm(data = jobcorps, formula = earn_lyr_bline ~ havekids_at_bline + male +
               mos_wrk_lyr_bline + wage_bline +
               anywlfr_bline + alcohol_bline + arrest_bline)
summary(model2)
plot(model2$residuals)
logLik(model2)

