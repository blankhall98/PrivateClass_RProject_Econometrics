#libraries
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)

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


