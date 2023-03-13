#libraries
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(car)

# read data
WagesPanel <- read_csv("data/WagesPanel.csv")
View(WagesPanel)

#database cleaning
WagesPanel$id <- as.factor(WagesPanel$id)
WagesPanel$educ <- as.factor(WagesPanel$educ)
WagesPanel$collgrad <- as.factor(WagesPanel$collgrad)
WagesPanel$msp <- as.factor(WagesPanel$msp)
WagesPanel$nev_mar <- as.factor(WagesPanel$nev_mar)
WagesPanel$not_smsa <- as.factor(WagesPanel$not_smsa)
WagesPanel$c_city <- as.factor(WagesPanel$c_city)
WagesPanel$south <- as.factor(WagesPanel$south)
WagesPanel$black <- as.factor(WagesPanel$black)
WagesPanel$union <- as.factor(WagesPanel$union)


#a. Generate a plot that describes the evolution of wages over time 
#   for all individuals in the sample.

fig <- plot_ly(WagesPanel, x=~year, y=~id, z=~lwage)
fig <- fig %>% add_markers()
fig

#b. Linear regression for each year
years <- unique(WagesPanel$year)

year_82 <- WagesPanel %>% filter(year==82)
year_83 <- WagesPanel %>% filter(year==83)
year_85 <- WagesPanel %>% filter(year==85)
year_87 <- WagesPanel %>% filter(year==87)
year_88 <- WagesPanel %>% filter(year==88)


model_82 <- lm(data=year_82, formula = lwage ~ hours + age + educ + collgrad +
                 msp + nev_mar + not_smsa + c_city + south + black + union + exper +
                 exper2 + tenure + tenure2)
summary(model_82)

model_83 <- lm(data=year_83, formula = lwage ~ hours + age + educ + collgrad +
                 msp + nev_mar + not_smsa + c_city + south + black + union + exper +
                 exper2 + tenure + tenure2)
summary(model_83)

model_85 <- lm(data=year_85, formula = lwage ~ hours + age + educ + collgrad +
                 msp + nev_mar + not_smsa + c_city + south + black + union + exper +
                 exper2 + tenure + tenure2)
summary(model_85)

model_87 <- lm(data=year_87, formula = lwage ~ hours + age + educ + collgrad +
                 msp + nev_mar + not_smsa + c_city + south + black + union + exper +
                 exper2 + tenure + tenure2)
summary(model_87)

model_88 <- lm(data=year_88, formula = lwage ~ hours + age + educ + collgrad +
                 msp + nev_mar + not_smsa + c_city + south + black + union + exper +
                 exper2 + tenure + tenure2)
summary(model_88)

# to see if there are any temporal differences, we can compare the confidence intervals
# of each model
confint.lm(model_82)
confint.lm(model_83)
confint.lm(model_85)
confint.lm(model_87)
confint.lm(model_88)

# c. temporal variables only
model_temporal_82 <- lm(data = year_82, formula = lwage ~ hours + age + exper + tenure)
summary(model_temporal_82)

model_temporal_83 <- lm(data = year_83, formula = lwage ~ hours + age + exper + tenure)
summary(model_temporal_83)

model_temporal_85 <- lm(data = year_85, formula = lwage ~ hours + age + exper + tenure)
summary(model_temporal_85)

model_temporal_87 <- lm(data = year_87, formula = lwage ~ hours + age + exper + tenure)
summary(model_temporal_87)

model_temporal_88 <- lm(data = year_88, formula = lwage ~ hours + age + exper + tenure)
summary(model_temporal_88)

# once more;
# to see if there are any temporal differences, we can compare the confidence intervals
# of each model
confint.lm(model_temporal_82)
confint.lm(model_temporal_83)
confint.lm(model_temporal_85)
confint.lm(model_temporal_87)
confint.lm(model_temporal_88)

# we can see that in all the models, the estimates are not very precise.
# using the summaries from the first models we create a model with the significant variables

# d. significant variables model
# we use the subset of the last year to have the most updated information
# another option could have been to obtain the mean per covariable per individual
significant_model <- lm(data = year_88, formula = lwage ~ exper + 
                          tenure + black + union + south + not_smsa)
summary(significant_model)
vif(significant_model)
confint.lm(significant_model)

