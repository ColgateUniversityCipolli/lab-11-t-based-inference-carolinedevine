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

## Combined Plot
dat.long <- dat.fig2g|>
  pivot_longer(cols = c("closer_vals", "further_vals", "difference"), 
               names_to = "Condition",                   
               values_to = "Fluoresence")

combined.plot <- ggplot(data = dat.long)+
  geom_boxplot(aes(x=Condition,  
                   y=Fluoresence))+
  theme_bw()+                  
  geom_hline(yintercept=0)+                                   
  theme_bw() +                                                
  xlab("Location") +                                    
  ylab("Fluoresence (% Change)") +
  ggtitle("Dopamine Levels by Distance")


################################################################################
# Question 4
################################################################################
library(effectsize)

### Part A: Close Responses ###
mu0 <- 0
x <- dat.fig2g$closer_vals
# TEST OF WHAT THEY DID, REPORTED ONE-SIDED, BUT DID TWO SIDED
# p-value
(p.val <- 2*pt(q=-abs(t.stat), df = n-1)) # this is the two tailed version
# Hedges G
g <- hedges_g(x = x, mu = mu0, alternative = "greater")
interpret_hedges_g(g)
g.list <- as.list(g)

# T-test
close.stats <- t.test(x=x, mu = mu0, conf.level = 0.95, alternative = "greater") # one tailed t-test

# Report Stats
close.table <- tibble(
  t = close.stats$statistic,
  p = close.stats$p.value,
  g = g.list$Hedges_g,
  CI.low = g.list$CI_low,
  CI.high = g.list$CI_high,
)
view(close.table)


### Part B: Far Responses ###
# Hedges G
x2 <- dat.fig2g$further_vals
g2 <- hedges_g(x = x2, mu = mu0, alternative = "less")
interpret_hedges_g(g2)
g2.list <- as.list(g2)

# T-test
far.stats <- t.test(x=x2, mu = mu0, conf.level = 0.95, alternative = "less") # one tailed t-test

# Report Stats
far.table <- tibble(
  t = far.stats$statistic,
  p = far.stats$p.value,
  g = g2.list$Hedges_g,
  CI.low = g2.list$CI_low,
  CI.high = g2.list$CI_high,
)
view(far.table)

### Part C: Difference ###
# Hedges G
x3 <- dat.fig2g$difference
g3 <- hedges_g(x = x3, mu = mu0, alternative = "two.sided")
interpret_hedges_g(g3)

# T-test
dif.stats <- t.test(x=x3, mu = mu0, conf.level = 0.95, alternative = "two.sided") # paired t-test
