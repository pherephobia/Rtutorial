getwd()
setwd("C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/W11")
dir.create(path= "Dataset")
#Exercise1 Open the ANES 2012 time series data, saving it as an R object 
########## called anes (available in the assignments folder on Blackboard). 
########## You will use these data for Exercise 1.
rm(list=ls())
library(foreign)
anes <- read.dta("Dataset/anes_timeseries_2012_Stata12.dta", convert.factors = TRUE)

###A. Use the party ID variable (pid x) to code a dichotomous variable 
##### equal to 1 for Democrats (including strong, weak, and leaning) and 
##### 0 for Republicans. Exclude true independents and 
##### the various missing values (refused/no answer/etc.) from the new measure.
library(plyr)
levels(anes$pid_x)
anes$pid.d <- revalue(anes$pid_x, c("-2. Missing" = NA, "4. Independent" = NA))
anes$pid.d.nu <- as.numeric(anes$pid.d)
summary(anes$pid.d.nu)
table(anes$pid.d)
class(anes$pid.d)
table(anes$pid.d.nu)
anes$pid.d <- ifelse(anes$pid.d.nu < 4, 1, 0) #Then, Democrats equal to 1, otherwise 0
table(anes$pid.d)

###B. Recode the education variable (dem edu) as a numerical variable, 
##### removing all missing values and the "other" category.
##### You should be left with an indicator that varies from 1 to 16.
anes$edu <- revalue(anes$dem_edu, c("-9. Refused" = NA, "-8. Don't know" = NA,
                                      "95. Other {SPECIFY}" = NA))
anes$edu.nu <- as.numeric(anes$edu)
table(anes$edu.nu)

###C. Conduct a difference of means test to examine whether mean education 
##### differs for Democrats and Republicans. Do you reject the null hypothesis?
##### Does the substantive difference in mean education between parties seem important?

ttest <- t.test(anes$edu.nu[anes$pid.d == 1], anes$edu.nu[anes$pid.d == 0])
ttest
mean.edurep
mean.edudem
table(anes$edu.nu[anes$pid.d == 1])
summary(anes$edu.nu[anes$pid.d == 1])
length(anes$edu.nu[anes$pid.d == 1])
table(anes$edu.nu[anes$pid.d == 0])
summary(anes$edu.nu[anes$pid.d == 0])
length(anes$edu.nu[anes$pid.d == 0])
###D. What is the level of measurement for the education variable you created? 
##### Could this level of measurement potentially cause problems in the test 
##### described in part C?
class(anes$edu)
table(anes$edu)

#I created education variable as an ordinal without missings or non-responses 
## and changed it into numeric for test the difference of means in education between party ids.
## We used the education variable as a numeric but we have to be careful to interpret the result as
## it is not shows same interval across the education levels. So the one unit of difference in edu-
## cation level does not show accurate difference in education level between party id of democrats and republicans.


###E. Conduct an ANOVA to examine whether mean education varies by party 
##### identification, this time using the full 7-point scale (pid x again, 
##### but omitting the various missing values).

anes$pid.d2 <- revalue(anes$pid_x, c("-2. Missing" = NA))
model_eduperpid <- aov(edu.nu ~ as.factor(pid.d2), data = anes)
summary(model_eduperpid)

###F. What do the results from the ANOVA tell us? What is the null hypothesis,
##### and do you reject it? What are the limitations regarding 
##### what ANOVA can tell us about the difference between groups?

#Null hypothesis is that there is no difference in the level of education 
##### among the party id groups. But the result shows that there exists statistically
##### significantly difference at least one of the groups.


#Exercise2 
#### A. Using the latest Quality of Governance time series data (easy to find online),
######  aggregate as appropriate(using ddply, summarize) to create a dataset that 
######  records for each region, each year, the number of states experiencing 
######  inter-state conflicts (use the UCDP-PRIO inter-state armed conflict variable
######  and the QOG region identifier). 
######  However, rather than a simple count, code the armed conflict variable 
###### in three categories:   
############  (1) 0 states experiencing inter-state armed conflict, 
############  (2) 1 state experiencing inter-state armed conflict, 
############  (3) 2 or more states experiencing inter-state armed conflict.
QOG <- read.dta(file = "http://www.qogdata.pol.gu.se/data/qog_std_ts_jan18.dta")
names(QOG)
QOG_summary <- subset(QOG, select = c(ccode, cname, year, ucdp_type2, ht_region))
library(plyr)
install.packages("sjlabelled")
library(sjlabelled)


class(QOG_summary$intlconflict.F)
table(QOG_summary$ht_region)


QOG_agg <- ddply(QOG_summary, .(ht_region, year), summarize, 
                 sum.intlconflict = sum(ucdp_type2, na.rm = TRUE))

#### B. What is the level of analysis of this new dataset? 
######  at what level of measurement is the armed conflict variable?
## The level of analysis of this new dataset(QOG_agg) is region-year. 
## And the level of measurement of armed confict variable is continuous.

QOG_agg$intlconflict <- ifelse(QOG_agg$sum.intlconflict==0 , 1,
                                   ifelse(QOG_agg$sum.intlconflict ==1, 2, 3))
QOG_agg$intlconflict.F <- 
  factor(QOG_agg$intlconflict, 
         labels=c("0 states experiencing inter-state armed conflict",
                  "1 state experiencing inter-state armed conflict",
                  "2 or more states experiencing inter-state armed conflict"))
summary(QOG_agg$sum.intlconflict)
#### C. Create a crosstab for region and the ordinal armed conflict variable. 
######  Conduct a chi-squared (chi^2) test, acting as though we meet assumptions 
######  regarding independent observations (though we likely do not).
table(QOG_agg$intlconflict.F, QOG_agg$ht_region)
xtabs( ~ ht_region + intlconflict.F, data = QOG_agg)
xtabs( ~ ht_region + intlconflict, data = QOG_agg)
model.region.conflict <- xtabs( ~ ht_region + intlconflict.F, data = QOG_agg)
Chisq <- chisq.test(model.region.conflict)
Chisq
#### D. Explain the result of the chi^2 test.
#### What is the null hypothesis, and do you reject it? What are the limitations
#### regarding what the chi-squared test can tell us about the dependence between 
#### these two variables?

#RESULTS: As the p-value of the chi-square test is less than the typical 
########  significance level(0.05), we cannot accept the null hypothesis. 
########  I can reject the null hypothesis with statistical evidence.
########  The null hypothesis of chi-square test is 
########  ``there is no association (=independence) between the two variables". 
########  With chi-square test, we can get the information of 
########  the precense and strength of the association. However, we cannot know the
########  direction of the association.

#### E. If we want to know whether the prevelance of inter-state conflict
######## (i.e., how many conflicts occured) varies by region, what might be 
######## problematic with the conflict variable that we operationlized?

## Our Inter-state conflict variable shows how many countries experiencing inter-state
##### and we can know how are they distributed by regions. The problem of our variable is that
##### it cannot distinguish between two cases. When the two conflicting states are in the same region,
##### The inter-state conflcit variable for the region would be counted as two. 
##### However, If the two states are in different regions, 
##### then the variable would be divded into two different regions.
##### Thus, some regions can be overcounted for the conflict cases.

#### F. For several observations, the conflict variable records that exactly 
######## one state experiences inter-state conflict. How is this possible given 
######## that at least two states must fight in order for an interstate conflict to exist?
######## This is related with the answer of E. If the two states are in the different regions, then
######## Each regions count only one for themselves.


#### From the Text
######## Exercise 5.30 Diamonds, Part II.
n.d.99 <- 23
n.d.10 <- 23
mean.d.99 <- 44.51
mean.d.10 <- 56.81
sd.d.99 <- 13.32
sd.d.10 <- 16.13
t.text <- qt(c(.975), df=22)
se <- sqrt((sd.d.99^2 / n.d.99) + (sd.d.10^2/n.d.10))
MoE <- t.text*se
ci95 <- c((mean.d.99-mean.d.10)-MoE, (mean.d.99-mean.d.10)+MoE)
ci95
######## Exercise 5.45 Coffee, depression, and physical activity
########## (a) Null hypothesis: the average physical activity level is 
##########     equal among the different levels of coffee consumption.
##########     Alternative hypothesis: At least an average physical activity 
##########     level is not equal among the different levels of coffee consumption.
########## (b) Since this study wants to investigate the relationship between
##########     coffee consumption and exercise, they have to check three conditions.
##########     First, Independent: for comparing, we have to assume that 
##########     the subjects in the different groups are independent to each other. 
##########     And it is satisfied in this study.
##########     Second: Normal distribution. As the sample size are even larger than 30,
##########     we can expect that the sampling distribution of sample mean would be
##########     approximately normal according to central limit theorem.
##########     Third: Constant variability, homoskedasticity. I looks satisfied too.
##########     Becuase the standard deviations across the groups seem quite constant.
########## (c) 
DF.g <- (5 - 1)
DF.e <- (50739 -5)
DF.total <- DF.g + DF.e
DF.total
SS <- 25575327 - 25564819
SS
MS.c <- SS/DF.g
MS.c
MS.r <- 25564829/DF.e
MS.r
F.value <- MS.c/MS.r
F.value
########## (d) ANOVA

######## Exercise 6.32 Full body scan, Part I.
########## (a)
n.rep <- 318
should.rep <- 264
n.dem <- 369
should.dem <- 299
prop.rep <- should.rep/n.rep
prop.dem <- should.dem/n.dem
prop.both <- (should.rep + should.dem)/(n.rep+n.dem)

#### Independence within groups: As this sample are randomly selected, and  
######                           the number of Republicans and Democrats are 
######                           less than 10% of all Republicans and Democrats, 
######                           this condition is satisficed.
#### Independence between groups: The sampled REpublicans and Democrats are 
######                           independent of each other. This condition is satisficed.
#### Success-failure: At least 10 observed successes and 10 observed failures in the two groups.
######                            This condition is also satisficed.
#### Null hypothesis: The proportion of answer in should of Republicans is equal to 
######                the proportion of answer in should of Democrats.
#### Alternative hypothesis: not equal
### As the sample size is larger than 30, calculate z
z <- (prop.rep-prop.dem)/(sqrt(prop.both*(1-prop.both))*sqrt((1/n.rep)+(1/n.dem)))
z
p.value <- 2 * (1 - pnorm(z))
### As the p-value is greater than 0.05, it is difficult to reject null hypothesis.
### We have insufficient evident to support that there is a difference in the
### proportion of Republicans and Democrats who think the full-body scans should
### be applied in the airports.
#### (b) Type I error is to reject the null hypothesis when the null is true.
#######  Also, Type II error is to fail to reject the null hypothesis when the null is false.
#######  In (a), since we failed to reject the null hypothesis, 
#######  it is possible to get a Type II error which fails to reject the false null hypothesis.