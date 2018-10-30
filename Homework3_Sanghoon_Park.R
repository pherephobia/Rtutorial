### POLI 502 FA18: Homework 3
### Due: (uploaded to Blackboard) October 19
### created by Sanghoon Park
### sp23@email.sc.edu
getwd()
setwd("C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/W6")
# remove all objects before the session starts
rm(list=ls())
#Exercise1  Download the American National Election Studies 2012 time series data 
#from the class Blackboard website, and load it as an R data frame called "anes."
#The ANES has over 2,200 variables-a prime example of data for which you need to use a codebook.
#Even with the (included) user guide/codebook, 
#it takes quite a while to familiarize oneself with the available ANES variables.
library(plyr)
library(foreign)
library(ggplot2)
anes <- read.dta("anes_timeseries_2012_Stata12.dta", convert.factors = TRUE)
levels(anes$libcpre_self)
class(anes$libcpre_self)
#A
anes$libcpre_self.re <- revalue(anes$libcpre_self,
                                c("-9. Refused" = NA, "-8. Don't know" = NA,
                                  "-2. Haven't thought much about this" = NA, 
                                  "1. Extremely liberal" = -3, "2. Liberal" = -2,
                                  "3. Slightly liberal" = -1, "4. Moderate; middle of the road" = 0,
                                  "5. Slightly conservative" = 1, "6. Conservative" = 2, 
                                  "7. Extremely conservative" =3))
#B
table(anes$libcpre_self)
table(anes$libcpre_self.re) # The two variables without missing are identical.
class(anes$libcpre_self.re)
libcpre_self.label <- c( "Extremely liberal", "Liberal", "Slightly liberal", 
                         "Moderate", "Slightly conservative",
                         "Conservative", "Extremely conservative")
anes$libcpre_self.nu <- as.numeric(anes$libcpre_self.re)
ideology.self <- ggplot(data = na.omit(subset(anes, 
                                              select = "libcpre_self.nu")), 
                        aes(x=libcpre_self.nu))
self.histogram <- ideology.self + geom_histogram(bins = 7) +
  labs(title = "Histogram of Self-placement ideology") + 
  scale_x_continuous("Liberal/conservative self-placement", 
                     breaks = c(1:7), labels = c(libcpre_self.label))
self.histogram

#C
levels(anes$dem_raceeth_x)
anes$race <- revalue(anes$dem_raceeth_x, c("-9. Missing" = NA))
levels(anes$race)
table(anes$race)
anes$race.nu <- as.numeric(anes$race)
newanes <- anes[!is.na(anes$race), ]
race.histogram <- ggplot(newanes, aes(x=libcpre_self.nu)) +
  geom_histogram(bins = 7) +
  labs(title = "Histogram of Self-placement ideology by racial groups") + 
  scale_x_continuous("Liberal/conservative self-placement", 
                     breaks = c(1:7), labels = c(libcpre_self.label)) + 
  facet_grid(. ~ newanes$race, labeller = label_wrap_gen(multi_line = TRUE)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
race.histogram
ggsave(file="figure/graph1.png", plot=race.histogram, width=10, height=5)
#D
table(anes$libcpre_self.nu)
prop.table(table(as.numeric(anes$libcpre_self.re)))
#E
sum(table(anes$libcpre_self.re))
prop.ExtrCon<- as.numeric(prop.table(table(anes$libcpre_self.re))[7])
prop.ExtrCon
choose(100, 20)*prop.ExtrCon^20*(1-prop.ExtrCon)^(100-20)

#F
choose((100-1),(20-1))*prop.ExtrCon^20*(1-prop.ExtrCon)^(100-20)

#TEXTBOOK

#3.16
mu <- 1500
std <- 300
#Pr(X >= 1900) = Pr(z >= a1) = 1-Pr(z < a1)
Z1 <- pnorm(1900, mu, std)
Pr1900 <- 1 - Z1
Z2 <- pnorm(2100, mu, std)
Pr2100 <- 1 - Z2
#For conditional probability,
Answer <- Pr2100/Pr1900
Answer

#3.40
##a
p <- 0.65
n <- 15
k <- 10
2002*(p)^k*(1-p)^(n-k)
##B
3003*(p)^k*(1-p)^(n-k)
##C
((p)^1)*((1-p)^(3-1)