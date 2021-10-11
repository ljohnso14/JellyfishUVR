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


df.detach <- read.csv("DetachmentData_2021-09-07.csv", stringsAsFactors = F)

## Organizng data ##
library(tidyverse)

df.detach.gather <- df.detach %>% gather(key = "Time", value = "Detachment", Day00:Day27)

# Trying to run a binomial glm, but running into errors
# ERRORS 
model1 <- glmer(Detachment ~ UV * Time + (1|Dish), data = df.detach.gather, na.action = na.omit, family = binomial)
summary(model1)


#################################################################
#### Detachment Data ############################################
#################################################################


######
# 9/5/2021 ********
df <- read.csv("DetachmentData_2021-09-05.csv", stringsAsFactors = T) # the no.days.detach isn't correct, need to account for the every other day at the end of experiment
# UV, Jar, Dish, detach.yes.no, no.days.detach

df$UV <- relevel(df$UV, ref = "Control") 

# df$JarID <- as.factor(df$JarID)
# df$Dish <- as.factor(df$Dish)

model <- glm(no.days.detach ~ UV + (1|JarID/Dish), data = df, na.action = na.omit, family = poisson(link = "log"))

(model.summary <- summary(model))

model2 <- glm(no.days.detach ~ UV + (1|JarID) + (1|Dish), data = df, na.action = na.omit, family = poisson(link = "log"))

(model2.summary <- summary(model2))

# lol, I don't ned to ad dishID as a random effect because I'm not looking over time
model3 <- glm(no.days.detach ~ UV + (1|JarID), data = df, na.action = na.omit, family = poisson(link = "log"))
(model3.summary <- summary(model3))


model <- glm(detach.yes.no ~ UV + (1|JarID), data = df, na.action = na.omit, family = binomial())
summary(model)

#https://easystats.github.io/blog/posts/performance_check_collinearity/
hist(df$no.days.detach)

library(MASS)
install.packages("VGAM")
library(VGAM)
model4 <- vglm(no.days.detach ~ UV + (1|JarID), data = df, na.action = na.omit, family = negative.binomial())


capture.output(model.summary, file = "detachment_results_2021-09-05.doc", append = T)

install.packages("mdthemes")

df$UV <- factor(df$UV, level = c("Control", "AA", "BB", "AB"))

detach <- ggplot(df, aes(UV, no.days.detach, fill = UV)) 

detach + 
  stat_boxplot(geom = "errorbar", width = 0.2, show.legend = F) +
  geom_boxplot(color = "black", size = .7, show.legend = F, outlier.colour = NA) + 
  scale_fill_manual(values=c("#B3CDE3", "#8C96C6", "#8856A7", "#810F7C")) +
  geom_jitter(alpha = 0.25, width = 0.2, size = 1.5, color = "black", show.legend = F) +
  mdthemes::md_theme_classic() +
  labs(x = NULL, y = expression(paste("Total Days Detached")),color = "black") +
  stat_summary(fun.y=mean, geom="point", color = "black", shape = 4, show.legend = F) +
  theme_classic()

######




df <- read.csv("DetachmentData_2021-07-08.csv", stringsAsFactors = F) #the format of my table isn't right so I manually made the table in R

D <- as.table(rbind(c(11,7), c(11,1), c(4,8), c(0,12)))
dimnames(D) <- list(treatment = c("Control", "AA", "BB", "AB"), status = c("Attached", "Detached"))

det <- as.table(rbind(c(11,11,4,0), c(7,1,8,12)))
dimnames(det) <- list(status = c("Attached","Detached"), treatment = c("Control", "AA", "BB", "AB"))


install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)

outputname = "detach_chisq_2021-07-08.doc"
capture.output(D, file = outputname)
capture.output(chisq.test(D), file = outputname, append = T)
capture.output(chisq.posthoc.test(D), file = outputname, append = T)
write('', file = outputname, append = T)
write('', file = outputname, append = T)
write('', file = outputname, append = T)
capture.output(det, file = outputname, append = T)
capture.output(chisq.test(det), file = outputname, append = T)
capture.output(chisq.posthoc.test(det), file = outputname, append = T)

D2 <- c(7/18, 1/12, 8/12, 12/12)
names(D2) <- c("Control", "AA", "BB", "AB")
barplot(D2, col = c("#B3CDE3","#8C96C6", "#8856A7", "#810F7C"), ylab = "Percent Detached")















