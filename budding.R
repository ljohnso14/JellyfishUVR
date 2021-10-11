###########################################
# Jelly Polyp UV Experiment 
# REU 2019 
# Co-Authors: LE Johnson, LM Treible
##########################################

#### Links I used when researching how to take into accounted nested within subjects variables 
#### https://stat.ethz.ch/pipermail/r-help/2006-October/115572.html
#### https://stat.ethz.ch/pipermail/r-help/2006-October/115572.html
#### https://lme4.r-forge.r-project.org/book/Ch2.pdf
#### https://maths-people.anu.edu.au/~johnm/r-book/2edn/xtras/mlm-lme.pdf


#### example code for nested linear mixed model 
#### lme(y ~ time * tx, random = ~time | therapist/subject, data = df)
#### lme(polyps ~ treatment * time, random = ~time|jar/dishid, data = df, na.action = na.omit)


## Importing data ##


# data where all the controls are combined into one group 
df.combinedcontrol <- read.csv("polypclonedata_combinedcontrol.csv", stringsAsFactors = F)


## Organizng data ##
library(tidyverse)

df.combinedcontrol.gather <- df.combinedcontrol %>% gather(key = "Time", value = "Polyps", Day00:Day27)


## Analyzing Data using Linear Mixed Models - Factorial Repeated Measure ANOVAs ##
## Analysis that is presented in 09/07/2021 version of working manuscript
library(lme4)
library(nlme)
library(emmeans)

model <-lme(Polyps ~ Treatment * Time, random = ~1 |JarID/DishID, data = df.combinedcontrol.gather, na.action = na.omit)
aovresults <- anova(model)
aovresults

emm <- emmeans(model, ~ Treatment * Time)
pairsresults <- pairs(emm, simple = "Treatment")
pairsresults

outputname <- "nopolyps_aov_pairs_results.doc"
capture.output(no.polyps.anova, file = outputname, append = T)
write('', file = outputname, append = T)
capture.output(no.polyps.pairs, file = outputname, append = T)


## Analyzing Data using Generalized Linear Mixed Models - Factorial Repeated Measure ANOVAs ##
## THIS IS WHERE ALL THE ERRORS AND WARNINGS COME 

model <- glm(Polyps ~ Treatment + (1|DishID), data = df.combinedcontrol.gather, na.action = na.omit, family = poisson(link = "log"))
summary(model)
# Error in 1 | DishID : 
# operations are possible only for numeric, logical or complex types
# 9/5/2021 maybe instead of multiplying by time, account for time?

# 8/8/2021 #
model <- glmer(Polyps ~ Treatment * Time + (1|JarID/DishID), data = df.combinedcontrol.gather, na.action = na.omit, family = poisson(link = "log"))
# Warning messages:
# 1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
# failure to converge in 10000 evaluations
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# unable to evaluate scaled gradient
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  : Model failed to converge: degenerate  Hessian with 1 negative eigenvalues


# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://www.rpubs.com/zerohour/control1

model <- glmer(Polyps ~ Treatment + Time + (1|JarID/DishID), data = df.combinedcontrol.gather, na.action = na.omit, family = poisson(link = "log"))
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0109455 (tol = 0.002, component 1)
model <- glmer(Polyps ~ Treatment + (1|JarID/DishID), data = df.combinedcontrol.gather, na.action = na.omit, family = poisson(link = "log"))


summary(model)
#





################################################################################
## Graphing Data
################################################################################

# Getting color scheme I want

vignette("ggplot2-specs")

install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

display.brewer.pal(n = 5, name = 'BuPu')


brewer.pal(n = 5, name = "BuPu")
# c("#B3CDE3", "#8C96C6", "#8856A7", "#810F7C")

library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(ggpubr)


ggline(df.combinedcontrol.gather, x = "Time", y = "Polyps", 
       combine = T,
       color = "Treatment",
       add = c("mean_se"),
       add.params = list(),
       ylab = expression("Number of polyps (mean ? s.e.)"),
       xlab = "Day of Experiment",
       shape = "Treatment",
       palette = c("#8C96C6", "#810F7C", "#8856A7", "#B3CDE3"),
       size = .3,
       font.label = list(size = 30, color = "black"),
       ggtheme = theme_classic()) +
  scale_x_discrete(breaks=c("Day00","Day01", "Day02", "Day03", "Day04", "Day05", "Day06", "Day07", "Day09", "Day11", "Day13", "Day15", "Day17", "Day19", "Day21", "Day23", "Day25", "Day27"),
                   labels=c("0", "1", "2", "3", "4", "5", "6", "7", "9", "11", "13", "15", "17", "19","21","23","25","27"))

