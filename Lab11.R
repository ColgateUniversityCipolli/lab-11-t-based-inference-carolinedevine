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

# T-test
close.stats <- t.test(x=x, mu = mu0, conf.level = 0.95, alternative = "greater") # one tailed t-test
CI <- t.test(x=x, mu = mu0, conf.level = 0.95, alternative = "two.sided")
?t.test
# Report Stats
close.table <- tibble(
  t = close.stats$statistic,
  p = close.stats$p.value,
  g = g$Hedges_g,
  CI.low = CI$conf.int[1],
  CI.high = CI$conf.int[2]
)
view(close.table)


### Part B: Far Responses ###
# Hedges G
x2 <- dat.fig2g$further_vals
g2 <- hedges_g(x = x2, mu = mu0, alternative = "less")

# T-test
far.stats <- t.test(x=x2, mu = mu0, conf.level = 0.95, alternative = "less") # one tailed t-test
CI2 <- t.test(x=x2, mu = mu0, conf.level = 0.95, alternative = "two.sided")

# Report Stats
far.table <- tibble(
  t = far.stats$statistic,
  p = far.stats$p.value,
  g = g2$Hedges_g,
  CI.low = CI2$conf.int[1],
  CI.high = CI2$conf.int[2]
)
view(far.table)

### Part C: Difference ###
# Hedges G
x3 <- dat.fig2g$difference
g3 <- hedges_g(x = x3, mu = mu0, alternative = "two.sided")

# T-test
dif.stats <- t.test(x=x3, mu = mu0, conf.level = 0.95, alternative = "two.sided") # paired t-test

# Report Stats
dif.table <- tibble(
  t = dif.stats$statistic,
  p = dif.stats$p.value,
  g = g3$Hedges_g,
  CI.low = dif.stats$conf.int[1],
  CI.high = dif.stats$conf.int[2],
)
view(dif.table)


################################################################################
# Question 5
################################################################################

# Reverse Engineer the hypothesis Test plot

################################################################################
### Part A: Close Responses ###
################################################################################

mu0 <- 0
x.close <- dat.fig2g$closer_vals
xbar <- mean(x.close)
s <- sd(x.close)
n <- length(x.close)
t.stat <- (xbar - mu0)/(s/sqrt(n))
g <- hedges_g(x = x.close, mu = mu0, alternative = "greater")
p <- close.stats$p.value

#########
# Plot it
#########
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t    = t.stat, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x= x.close,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
close.test.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions (one for close)
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.95, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="red", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Closer Responses",
          subtitle=bquote(H[0]==0*";"~H[a]>0))


################################################################################
### Part B: Far Responses ###
################################################################################

mu0 <- 0
x.far <- dat.fig2g$further_vals
xbar <- mean(x.far)
s <- sd(x.far)
n <- length(x.far)
t.stat <- (xbar - mu0)/(s/sqrt(n))
g2 <- hedges_g(x = x.far, mu = mu0, alternative = "less")
p2 <- far.stats$p.value

#########
# Plot it
#########
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t    = t.stat, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x= x.far,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
far.test.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions (one for far)
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.05, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t<=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="red", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Farther Responses",
          subtitle=bquote(H[0]==0*";"~H[a]<0))

################################################################################
### Part C: Difference Responses ###
################################################################################

mu0 <- 0
x.diff <- dat.fig2g$difference
xbar <- mean(x.diff)
s <- sd(x.diff)
n <- length(x.diff)
t.stat <- (xbar - mu0)/(s/sqrt(n))
g3 <- hedges_g(x = x.diff, mu = mu0, alternative = "two.sided")
p3 <- dif.stats$p.value

#########
# Plot it
#########
# For plotting the null distribution
ggdat.t <- tibble(t=seq(-5,5,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1))
# For plotting the observed point
ggdat.obs <- tibble(t    = t.stat, 
                    y    = 0) # to plot on x-axis

# Resampling to approximate the sampling distribution 
# on the data
R <- 1000
resamples <- tibble(t=numeric(R))
for(i in 1:R){
  curr.sample <- sample(x= x.diff,
                        size=n,
                        replace=T)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(n))
}

t.breaks <- c(-5, qt(0.025, df = n-1), # rejection region (left)
              0, 
              qt(0.975, df = n-1), 5,  # rejection region (right)
              t.stat)                  # t-statistic observed
xbar.breaks <- t.breaks * s/(sqrt(n)) + mu0

# Create Plot
diff.test.plot <- ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions - left
  geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # rejection regions - right
  geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
              aes(x=t, ymin=0, ymax=pdf.null),
              fill="red", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.obs, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")+
  # clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Paired Differences (close - far)",
          subtitle=bquote(H[0]==0*";"~H[a]!=0))

## COMBINED ##
library(patchwork)
close.test.plot + far.test.plot + diff.test.plot
