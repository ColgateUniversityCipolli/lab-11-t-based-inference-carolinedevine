################################################################################
# Lab 11
# Caroline Devine
# Due 04/15/25
################################################################################

# Zebra Finches
# - measure dopamine using fibre photometry, 
#    changes in the fluorescence indicate dopamine changes in real time

################################################################################
# Question 1
################################################################################
library(tidyverse)
library(pwr) # make sure to cite
?pwr

power <- power.t.test(delta = 0.65, sig.level = 0.05,
             power = 0.8,
             type = "one.sample",
             alternative = "two.sided")

# The number of observations that the researchers need to detect a moderate-to-large effect
# of d = 0.65 is n = 20.58 ~ 21

################################################################################
# Question 2
################################################################################

# downloaded excel file called source data for figure 2
# isolated the columns for the data needed to replicate Figure 2(g)
# those columns were closer_vals and further_vals

# This is the description for Figure 2(g) on the article
# 2g is a scatter plot of averaged delta(F) / F signals for all syllables for 
# closer (blue) and further (red) renditions

dat.fig2g <- read_csv("fig2gdata.csv")

dat.fig2g <- dat.fig2g |>
  mutate(difference = closer_vals - further_vals)
View(dat.fig2g)

################################################################################
# Question 3: Summarize the Data
################################################################################
library(e1071)
# Part A: Summarize the Further Data
sum.further <- dat.fig2g |>
  summarize(
    n = n(),
    mean = mean(further_vals, na.rm = T),
    sd = sd(further_vals, na.rm = T),
    skew = skewness(further_vals, na.rm=T)
  )
view(sum.further)


further.plot <- ggplot(data = dat.fig2g)+
  geom_boxplot(aes(x=further_vals, y = after_stat(density)),    # Density histogram
                 breaks=seq(1,6, length.out=10),
                 color="grey30", fill="lightgray")+             # Specify bins
  geom_hline(yintercept=0)+                                   # Add x-axis
  theme_bw() +                                                # Remove gray background
  xlab("Further Values") +                   # x-axis label
  ylab("Density")                                             # y-axis label





# Part B: Summarize the Closer Data
sum.closer <- dat.fig2g |>
  summarize(
    mean = mean(closer_vals, na.rm = T),
    sd = sd(closer_vals, na.rm = T),
    skew = skewness(closer_vals, na.rm=T)
  )
view(sum.closer)

# Part C: Summarize the Paired Differences
sum.dif <- dat.fig2g |>
  summarize(
    mean = mean(difference, na.rm = T),
    sd = sd(difference, na.rm = T),
    skew = skewness(difference, na.rm=T)
  )
view(sum.dif)

