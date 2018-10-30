### POLI 502 FA18: Homework 2
### Due: (uploaded to Blackboard) Friday, 10/5, at 6 PM
### created by Sanghoon Park
### sp23@email.sc.edu
rm(list=ls())
getwd()
setwd("C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/W5")
# remove all objects before the session starts
# Install and load World Development Indicators package
install.packages("WDI", repos="http://cran.us.r-project.org")
library(WDI)
# Load a data frame using 5 WDI variables over 10 years
WDI.data <- WDI(country="all", indicator=c("IQ.CPA.GNDR.XQ","IC.BUS.EASE.XQ",
                                           "SP.DYN.CBRT.IN", "AG.LND.AGRI.K2", "AG.LND.AGRI.ZS"), start=1995, end=2005)
WDI.data <- read.csv(file = "WDI.data.csv")
#A. Aside from the ID variables (i.e., iso code, country, and year), explain
### what each variable is capturing, and its level of measurement (if numerical,
### explain whether it is discrete or continuous, and whether it is a ratio
### variable). 
### Hint: You will need the codebook. Googling the variable name takes you right to a useful webpages.
summary(WDI.data)
WDIsearch("IQ.CPA.GNDR.XQ", field="indicator") #ordinal
WDIsearch("IC.BUS.EASE.XQ", field="indicator") #ordinal
WDIsearch("SP.DYN.CBRT.IN", field="indicator") #numeric, continuous
WDIsearch("AG.LND.AGRI.K2", field="indicator") #numeric, continuous
WDIsearch("AG.LND.AGRI.ZS", field="indicator") #numeric, continuous
#B. Make a subset data to exclude all regions, retaining only countries.

wdi_data <- WDI_data
countries=wdi_data[[2]]
countryregion = as.data.frame(countries)
purecountry <- countryregion$region != "Aggregates"
countries_df <- countryregion[purecountry,]
WDI.data = subset(WDI.data, country %in% countries_df$country)

#C. The probability that I would be pointing to an area in which the land is
### arable of Honduras in 1998
### The ratio of Honduras agricultural area / Total area in 1998
HN1998 <- as.numeric(subset(WDI.data, WDI.data$country=="Honduras" & WDI.data$year==1998,
                            select='AG.LND.AGRI.ZS')/100)
HN1998
#D. If I point to three random spot on a map of Albania in 2004,
### What is the probability that all three spots would represent arable land?
### (The ratio of Albania arable area / total area)^3
AL2004 <- as.numeric(subset(WDI.data, WDI.data$country=="Albania" & WDI.data$year==2004,
                            select='AG.LND.AGRI.ZS')/100)
AL2004^3
#E. If you were to point to six random spots on a map of Chad in 2002, 
### what is the probability that at least one spot would represent arable land?
CHAD02 <- as.numeric(subset(WDI.data, WDI.data$country=="Chad" & WDI.data$year==2002, 
                            select='AG.LND.AGRI.ZS')/100)
1-((1-CHAD02)^6)

#Exercise 2.
#A. Go to http://correlatesofwar.org and download the appropriate data 
### necessary to create data frames for
### 1. inter-state war data from the COW war data 4.0
download.file(url = "http://correlatesofwar.org/data-sets/COW-war/inter-state-war-data/at_download/file", 
              destfile = "Inter-StateWarData_v4.0.csv", mode='wb')
InterStateWar <- read.csv(file = "Inter-StateWarData_v4.0.csv", na = c(-9))
### 2. dyadic trade data from the COW trade data 4.0
download.file(url = "http://correlatesofwar.org/data-sets/bilateral-trade/cow_trade_4.0", 
              destfile = "COW_Trade_4.0.zip", mode='wb')
unzip("COW_Trade_4.0.zip", exdir = getwd())
Dytrade <- read.csv(file = "COW_Trade_4.0/Dyadic_COW_4.0.csv", na = c(-9))
#B. Create a new data frame that is a subset of dyadic trade data 
### including only dyads in which at least one state is located in Europe
### (ccode between 200 and 399).
str(EuropeTrade)
str(Europe)
EuropeTrade <- Dytrade[which((Dytrade$ccode1 >=200 & Dytrade$ccode1 <=399 ) | (Dytrade$ccode2 >=200 & Dytrade$ccode2 <=399)), ]
table(EuropeTrade$ccode2)
table(Europe$ccode2)
View(EuropeTrade)
identical(EuropeTrade,Europe)
Europe <- subset(Dytrade, (ccode1 >=200 & ccode1<400) | (ccode2 >=200 & ccode2<400))
discrep <- mapply(setdiff, EuropeTrade, Europe)
discrep
num.discrep <- sapply(discrep, length)
#C. Create a new data frame that is a subset of the inter-state war data 
### including only wars that are fought, at least in part, in Europe.
Fight <- subset(InterStateWar, WhereFought %in% c(2, 11, 12, 14, 15, 19))
#D. Using the subset data frames from parts B. and C., 
### create a new data frame where the unit of observation is the year. 
### It should include the year range that is covered in both the dyadic 
### trade and inter-state war data frames. It should have variables 
### identifying 1) the mean dyadic trade (of dyads including at least one 
### European state) and 2) the count of wars that begin 
### (for the first time, and which are fought, at least in part,
library(plyr)

Europe.d1 <- Europe[, c(1,2,3, 6, 7)]
Fight.d1 <- Fight[, c(1, 4, 9)]
Europe.year <- ddply(Europe.d1, .(year), summarize, meantrade = mean(sum(flow1, flow2, na.rm = TRUE)))
Fight.year <- ddply(Fight.d1, .(StartYear1), summarize, sumwar = length(unique(WarNum)))
yearbase <- merge(x = Fight.year, y = Europe.year, by.x = "StartYear1", by.y = "year", all = TRUE)
?merge
#E. Using the new data frame from part D., Create a scatterplot where the x 
### axis represents the (natural) log of mean dyadic trade, while the y axis 
### represents the count of European war onsets.
library(ggplot2)
g <- ggplot(yearbase, aes(x=log(yearbase$meantrade), y=yearbase$sumwar))
g + geom_point() + labs(x = "The log of mean dyadic trade", 
                        y = "The count of European war")

summary(log(yearbase$meantrade))
attach(Trade_War_Year)
yearbase$logtrade <- log(yearbase$meantrade)
table(yearbase$logtrade)
plot(yearbase$logtrade, yearbase$sumwar, main="Mean Dyadic Trade vs. European War Onsets", 
     xlab="(Natural) log of mean dyadic trade", ylab="Count of European war onsets")

##From the Text
###Q2.2 Roulette wheel
####(a) P(red) is
prop.red <- 18/38
prop.red
prop.red2 <- 18/38
prop.red2
#Equally confient

###Q2.6 Dice rolls. If you roll a pair of fair dice, what is the probability of
####(a) getting a sum of 1? -> 0
0/36
####(b) getting a sum of 5? -> (1,4) (2,3). (3,2), (4,1)
4/36
####(c) getting a sum of 12? -> (6,6)
1/36

###Q2.17 2.17 Global warming
####(a) No, Earth is warming and Liberal Democrat are not mutuaaly exclusive.
####(b) Pr(Earth is W of LD) = Pr(Earth is Warming)+Pr(LD)-Pr(Earth is warming & LD)
Pr.WorLD <- 0.6+0.2-0.18
Pr.WorLD
####(c) Pr(Earth is Warming|LD) = Pr(Earth is warming & LD)/Pr(LD)
Pr.EWconLD <- 0.18/0.20
Pr.EWconLD
####(d) Pr(Earth is Warming|CR) = Pr(Earth is warming & CR)/P(CR)
Pr.EWconCR <- 0.11/0.33
Pr.EWconCR
####(e) Does it appear that whether or not a respondent believes 
#### the earth is warming is independent of their party and ideology? 
#### Explain your reasoning.
#### Earth is warming and party ideology are not independent as 
#### Pr.EWconLD and Pr.EWconCR are not equal
####(f) Pr(moderate/liber Repulbic|Earth is not warning) = Pr(Earth is not warming and moderate/LR)
Pr.Mod.LRconNotW <- 0.06/0.34
Pr.Mod.LRconNotW
###Q2.35 Hearts win; 3hearts: $50, 3black cards: $25, other: $0
####(a) Create a probability model for the amount you win at this game
####    , and find the expected winnings.
#Probablity model
prob.model <- data.frame("x" = c(0, 25, 50), "P(X = x)" = c(739/850, 2/17, 11/850))
#The expected winnings
exp.w <- 0*(739/850) + 25*(2/17) + 50*(11/850)

####(c) If the game costs $5 to play, should you play this game? Explain.
#### No, I don't paly this goame. According to the expected winnings, 
#### I am expected to lose the game.