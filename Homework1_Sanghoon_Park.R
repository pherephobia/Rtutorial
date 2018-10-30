# First, set the working directory
getwd()
setwd("C:/Users/phere/Dropbox/Scholar/2_Graduates/2018_03_Fall/POLI502_Methodology_of_Political_Analysis/W3")
# Create a figures subfolder
dir.create(path= "figures")
assign.size <- 1000000
set.seed(198912)
assign1 <- rep(NA, assign.size)
for (x in 1:assign.size) {
  MP <- rbinom(24, 1, 35/48)
  WP <- rbinom(24, 1, 35/48)
  assign1[x] <- (sum(MP) - sum(WP))/24 ##more efficient way
}
hist(assign1)
##correct

pdf("assignment_Q1.pdf", width=8, height=6)
hist(assign1, xlab = "Margin", main = NULL)
dev.off()

table(assign1[assign1 >= 0.3])
sum(table(assign1[assign1 >= 0.3]))/assign.size


#Assignment_Question3
# Load the BRFSS data
source("http://www.openintro.org/stat/data/cdc.R")
#A
str(cdc)
#genhlth: ordinal, discrete
#exerany: categorical, discrete
#hlthplan: categorical, discrete
#smoke100: categorical, discrete
#height: numerical
#weight  : numerical
#wtdesire: numerical
#age     : numerical
#gender  : categorical, discrete

#B
summary(cdc$height)
summary(cdc$age)
IQR(cdc$height)
IQR(cdc$age)
gender <- table(cdc$gender)
barplot(gender/20000)
prop.table(table(cdc$exerany))[1]
BRFSS.23.S <- subset(cdc, age < 23 & smoke100==1)
str(BRFSS.23.S)
cdc$age23.smoke100 <- ifelse(cdc$age < 23 & cdc$smoke100==1, 1, 0) #efficient
summary(cdc$age23.smoke100)

#E
plot(cdc$weight, cdc$wtdesire, xlab="Weight", 
     ylab="Desired weight", 
     main="Plot of current weight and desired weight")
library(ggplot2)
g <- ggplot(cdc, aes(cdc$weight, cdc$wtdesire))
g + labs(x = "Weight", y = "Desired weight", 
         title="Relationship between current weight and desired weight") + geom_point()
#F
cdc$wdiff <- cdc$weight-cdc$wtdesire
str(cdc$wdiff)
ggplot(cdc, aes(x=wdiff)) + geom_histogram()
summary(cdc$wdiff)
sd(cdc$wdiff)
#Standard deviation; 
str(cdc$weight)
cor(cdc$weight, cdc$wtdesire)
#G
#discrete numerical
setwd("C:/Users/phere/Documents/R")
#Question4
#(1) COW National Trade 4.0 (part of the COW Trade 4.0)
download.file(url = "http://correlatesofwar.org/data-sets/bilateral-trade/cow_trade_4.0", 
              destfile = "COW_Trade_4.0.zip", mode='wb')
unzip("COW_Trade_4.0.zip", exdir = getwd())
COWnatlTrade <- read.csv(file = "COW_Trade_4.0/National_COW_4.0.csv")
#(2) participant-level MID data 4.2
download.file(url = "http://correlatesofwar.org/data-sets/MIDs/militarized-interstate-disputes-4-2/at_download/file/", 
              destfile = "MID-level.zip", mode='wb')
unzip("MID-level.zip", exdir = getwd())
part.level <- read.csv(file = "MIDB_4.2.csv")
#(3) the state-year Direct Contiguity Data 3.2.
download.file(url = "http://correlatesofwar.org/data-sets/direct-contiguity/direct-contiguity-v3-2/at_download/file", destfile = "DirectContiguity320.zip", mode='wb')
unzip("DirectContiguity320.zip", exdir = getwd())
direct.contig <- read.csv(file = "DirectContiguity320/contdirs.csv")
head(COWnatlTrade) # level of analysis: state
head(part.level) # level of analysis: state
head(direct.contig) #level of analysis: state
head(direct.contig)
str(COWnatlTrade)
str(part.level)
str(direct.contig)
library(dplyr)
library(plyr)
names(direct.contig)[1] <- "ccode"
names(part.level)[7] <- "year"
statelevel <- merge(x = direct.contig, y = COWnatlTrade, by = c("ccode", "year"), all=TRUE)
statelevel <- merge(x = statelevel, y=part.level,  by = c("ccode", "year"), all=TRUE)
statelevel$midcheck <- ifelse(!is.na(statelevel$version),1,0)
table(statelevel$midcheck)
str(statelevel)

#FROM THE Text
##Exercise 1.2. Sinusitis and antibiotcs
###(a) What percent of patients in the treatment group experienced a significant
###improvement in symptons? What percent in the control group?
(66/85)*100 #percent
(65/81)*100 #percent
###(b) Based on your finding in part (a), which treatment appears to be more
###effective for sinusitis?
###The placebo seems more effective.
