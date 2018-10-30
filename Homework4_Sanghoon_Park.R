getwd()
setwd("C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/W10")
dir.create(path= "Figures")
dir.create(path= "Tables")
#Exercise 1
#For this exercise, use the WDI data we downloaded during our Chapter 4 
#R tutorial in class. As in class, we will assume that each variable represents 
#a population, ignoring repeated measurement over time, etc.
library(WDI)
WDI.data <- WDI(country="all", indicator=c("SH.VAC.TTNS.ZS"), start=1995, end=2005)
WDI.data$tetanus.vaccination <- WDI.data$SH.VAC.TTNS.ZS
WDIsearch(string="SH.VAC.TTNS.ZS", field='indicator')
## A
set.seed(1224)
sampling.size <- 100
mean.pop.100 <- rep(NA, 100)
lower.pop.100 <- rep(NA, 100)
upper.pop.100 <- rep(NA, 100)

for(x in 1:sampling.size) {
  samp.pop <- sample(WDI.data$tetanus.vaccination, 70)
  mean.pop.100[x] <- mean(samp.pop, na.rm = TRUE)
  lower.pop.100[x] <- mean.pop.100[x] - (2.58 * (sd(samp.pop, na.rm = TRUE)/sqrt(length(samp.pop))))
  upper.pop.100[x] <- mean.pop.100[x] + (2.58 * (sd(samp.pop, na.rm = TRUE)/sqrt(length(samp.pop))))
}
sd.pop.100 <- sd(samp.pop, na.rm = TRUE)
mean.pop.100
lower.pop100
upper.pop.100
sd.pop.100
# Put the vectors together with a vector for sample number
estimates.df.100 <- as.data.frame(cbind(lower.pop.100, mean.pop.100, upper.pop.100))
estimates.df.100$sample.no <- c(1:100)

##B
# Plot the results (more useful now that we have 20 samples)
library(ggplot2)
estimates.df.100$outside <- ifelse(estimates.df.100$lower.pop.100 > 
                                    mean(WDI.data$tetanus.vaccination,
                                         na.rm = TRUE) | 
                                    estimates.df.100$upper.pop.100 < 
                                    mean(WDI.data$tetanus.vaccination, 
                                         na.rm = TRUE), 1, 0)


ggplot(data = estimates.df.100, aes(x = sample.no, color = as.factor(outside))) +
  geom_pointrange(aes(y = mean.pop.100, ymin = lower.pop.100, ymax = upper.pop.100)) + 
  geom_hline(data = WDI.data, aes(yintercept = mean(tetanus.vaccination, na.rm = TRUE))) +
  theme_bw() + 
  scale_color_manual(name = "", values=c("#9999CC", "#CC6666")) +
  theme(legend.position="none") +
  xlab("Sample number") + ylab("Estimates") + ggtitle("Sample means for Tetanus Vaccination, with 99% confidence intervals (N=70)")

##C


##D
set.seed(1224)
mean.pop.10 <- rep(NA, 100)
lower.pop.10 <- rep(NA, 100)
upper.pop.10 <- rep(NA, 100)
for(x in 1:sampling.size) {
  samp.pop2 <- sample(WDI.data$tetanus.vaccination, 10)
  mean.pop.10[x] <- mean(samp.pop2, na.rm = TRUE)
  lower.pop.10[x] <- mean.pop.10[x] - (2.58 * (sd(samp.pop2, na.rm = TRUE)/sqrt(length(samp.pop2))))
  upper.pop.10[x] <- mean.pop.10[x] + (2.58 * (sd(samp.pop2, na.rm = TRUE)/sqrt(length(samp.pop2))))
}

sd.pop.10 <- sd(samp.pop2, na.rm = TRUE)
estimates.df.10 <- as.data.frame(cbind(lower.pop.10, mean.pop.10, upper.pop.10))
estimates.df.10$sample.no <- c(1:100)

estimates.df.10$outside <- ifelse((estimates.df.10$lower.pop.10 > 
                                    mean(WDI.data$tetanus.vaccination, na.rm = TRUE) | 
                                    estimates.df.10$upper.pop.10 < mean(WDI.data$tetanus.vaccination, 
                                                                        na.rm = TRUE)), 1, 0)

str(estimates.df.10)
table(table(estimates.df.10$sample.no[estimates.df.10$outside==1]))

ggplot(data = estimates.df.10, aes(x = sample.no, color = as.factor(outside))) +
  geom_pointrange(aes(y = mean.pop.10, ymin = lower.pop.10, ymax = upper.pop.10)) + 
  geom_hline(data = WDI.data, aes(yintercept = mean(tetanus.vaccination, na.rm = TRUE))) +
  theme_bw() + 
  scale_color_manual(name = "", values=c("#9999CC", "#CC6666")) +
  theme(legend.position="none") +
  xlab("Sample number") + ylab("Estimates") + ggtitle("Sample means for Tetanus Vaccination, with 99% confidence intervals (N=10)")


x.bar <- 23.44
s <- 4.72
n <- 5534
z.95 <- 1.96
se = s/sqrt(n) #standard error
lower <- x.bar - z.95*se
upper <- x.bar + z.95*se
ci <- as.data.frame(cbind(lower, upper))
ci