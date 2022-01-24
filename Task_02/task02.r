setwd("~/Desktop/Evolution/Tasks/Task_02")
dir()
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote = F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
head(berenMilk)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
order(Data$age)
beren3 <- beren2[order(beren2$age) ,]
getOption("max.print")
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
setwd("~/Desktop/Evolution/Tasks/Task_02")
dir()
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors = F)
write.csv(Data, 'rawdata.csv', quote = F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
head(berenMilk)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
order(Data$age)
beren3 <- beren2[order(beren2$age) ,]
getOption("max.print")
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
#Question 1: Hypothesis I: "amount" is not measured in the data, only the ounces
#of milk are measuring. Hypothesis II: "There is a relationship" is not specific
#because there is a relationship between everything. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
head(avgFeed)
varFeed <-tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor.test(beren3$value[Feeds], beren3$age[Feeds])
cor(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)" )
?par
# las is the style of axis labels correlated to numbers meaning positions
# mar is numberical vector of the form c(bottom, left, top, right)
# mgp is the margin line
# tck is the length of tick marks
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
#Question 2: For the left graph, there are 3 different axis titles but only two 
#axis labeled. As for the right graph, there is no unit for nap time, and both 
#graphs look very squished together. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
unique(beren3$event)
#Hypothesis: Beren's mass increases with age. 
traitmass <- which(beren3$event == "trait_mass")
traitmass
avgtraitmass <- mean(beren3$value[traitmass])
avgtraitmass
cor.test(beren3$value[traitmass], beren3$age[traitmass])
aov(beren3$value[traitmass] ~ beren3$age[traitmass])
Naps <- which(beren3$event == 'nap')
beren4 <- beren3[Naps, ]
beren4
# 21.42 hours slept total
Starttime <- tapply(beren4$start_hour, beren4$start_minute)
time1 <- "2019-08-22 12:30"
time1E <- "2019-08-22 13:30"
Aug22nap <- difftime(time1E, time1, units = "hours")
Aug22nap
time2 <- "2019-08-23 8:55"
time2E <- "2019-08-23 9:36"
Aug23nap <- difftime(time2E, time2, units = "hours")
time3 <- "2019-08-23 11:52"
time3E <- "2019-08-23 12:46"
Aug23nap2 <- difftime(time3E, time3, units = "hours")
Aug23naptotal <- sum(Aug23nap, Aug23nap2)
Aug23naptotal
time4 <- "2019-08-26 8:40"
time4E <- "2019-08-26 10:10"
Aug26nap <- difftime(time4E, time4, units = "hours")
time5 <- "2019-08-26 12:45"
time5E <- "2019-08-26 13:20"
Aug26nap2 <- difftime(time5E, time5, units = "hours")
Aug26naptotal <- sum(Aug26nap, Aug26nap2)
Aug26naptotal
time6 <- "2019-08-27 11:10"
time6E <-"2019-08-27 12:40"
Aug27nap <- difftime(time6E, time6, units= "hours")
time7 <- "2019-08-27 14:10"
time7E <- "2019-08-27 14:45"
Aug27nap2 <- difftime(time7E, time7, units = "hours")
Aug27naptotal <- sum(Aug27nap, Aug27nap2)
Aug27naptotal
time8 <- "2019-08-30 7:30"
time8E <- "2019-08-30 8:42"
Aug30nap <- difftime(time8E, time8, units = "hours")
time9 <- "2019-08-30 10:21"
time9E <- "2019-08-30 11:34"
Aug30nap2 <- difftime(time9E, time9, units = "hours")
time10 <- "2019-08-30 14:27"
time10E <- "2019-08-30 14:37"
Aug30nap3 <- difftime(time10E, time10, units = "hours")
Aug30naptotal <- sum(Aug30nap, Aug30nap2, Aug30nap3)
Aug30naptotal
time11 <- "2019-09-04 8:04"
time11E <- "2019-09-04 9:19"
Sep4nap <- difftime(time11E, time11, units = "hours")
time12 <- "2019-09-04 11:07"
time12E <- "2019-09-04 12:56"
Sep4nap2 <- difftime(time12E, time12, units = "hours")
Sep4naptotal <- sum(Sep4nap, Sep4nap2)
Sep4naptotal
time13 <- "2019-09-05 8:00"
time13E <- "2019-09-05 9:19"
Sep5nap <- difftime(time13E, time13, units = "hours")
time14 <- "2019-09-05 12:15"
time14E <- "2019-09-05 12:33"
Sep5nap2 <- difftime(time14E, time14, units = "hours")
time15 <- "2019-09-05 12:55"
time15E <- "2019-09-05 14:17"
Sep5nap3 <- difftime(time15E, time15, units = "hours")
Sep5naptotal <- sum(Sep5nap, Sep5nap2, Sep5nap3)
Sep5naptotal
time16 <- "2019-09-06 8:48"
time16E <- "2019-09-06 9:32"
Sep6nap <- difftime(time16E, time16, units = "hours")
time17 <- "2019-09-06 11:34"
time17E <- "2019-09-06 12:10"
Sep6nap2 <- difftime(time17E, time17, units = "hours")
time18 <- "2019-09-06 14:10"
time18E <- "2019-09-06 14:40"
Sep6nap3 <- difftime(time18E, time18, units = "hours")
Sep6naptotal <- sum(Sep6nap, Sep6nap2, Sep6nap3)
Sep6naptotal
time19 <- "2019-09-09 8:46"
time19E <- "2019-09-09 9:43"
Sep9nap <- difftime(time19E, time19, units = "hours")
time20 <- "2019-09-09 12:38"
time20E <- "2019-09-09 13:23"
Sep9nap2 <- difftime(time20E, time20, units = "hours")
Sep9naptotal <- sum(Sep9nap, Sep9nap2)
Sep9naptotal
time21 <- "2019-09-10 9:00"
time21E <- "2019-09-10 9:40"
Sep10nap <- difftime(time21E, time21, units = "hours")
time22 <- "2019-09-10 12:00"
time22E <- "2019-09-10 12:35"
Sep10nap2 <- difftime(time22E, time22, units = "hours")
Sep10naptotal <- sum(Sep10nap, Sep10nap2)
Sep10naptotal
time23 <- "2019-09-11 9:49"
time23E <- "2019-09-11 10:36"
Sep11nap <- difftime(time23E, time23, units = "hours")
time24 <- "2019-09-11 11:49"
time24E <- "2019-09-11 12:05"
Sep11nap2 <- difftime(time24E, time24, units = "hours")
time25 <- "2019-09-11 14:16"
time25E <-"2019-09-11 14:45"
Sep11nap3 <- difftime(time25E, time25, units = "hours")
Sep11naptotal <- sum(Sep11nap, Sep11nap2, Sep11nap3)
Sep11naptotal
time26 <- "2019-09-12 9:08"
time26E<-"2019-09-12 9:46"
Sep12nap <- difftime(time26E, time26, units = "hours")
time27 <- "2019-09-12 12:07"
time27E <- "2019-09-12 13:05"
Sep12nap2 <- difftime(time27E, time27, units = "hours")
Sep12naptotal <- sum(Sep12nap, Sep12nap2)
Sep12naptotal
time28 <- "2019-09-13 8:22"
time28E <- "2019-09-13 9:17"
Sep13nap <- difftime(time28E, time28, units = "hours")
time29 <- "2019-09-13 11:45"
time29E <- "2019-09-13 12:15"
Sep13nap2 <- difftime(time29E, time29, units = "hours")
Sep13naptotal <- sum(Sep13nap, Sep13nap2)
Sep13naptotal
time30 <- "2019-09-16 12:15"
time30E <- "2019-09-16 12:53"
Sep16nap <- difftime(time30E, time30, units = "hours")
Sep16nap
time31 <- "2019-09-18 10:40"
time31E <- "2019-09-18 11:02"
Sep18nap <- difftime(time31E, time31, units = "hours")
time32 <- "2019-09-18 14:35"
time32E <- "2019-09-18 14:45"
Sep18nap2 <- difftime(time32E, time32, units = "hours")
Sep18naptotal <- sum(Sep18nap, Sep18nap2)
Sep18naptotal
time33 <- "2019-09-20 9:40"
time33E <- "2019-09-20 10:23"
Sep20nap <- difftime(time33E, time33, units = "hours")
time34 <- "2019-09-20 13:21"
time34E <- "2019-09-20 14:14"
Sep20nap2 <- difftime(time34E, time34, units = "hours")
Sep20naptotal <- sum(Sep20nap, Sep20nap2)
Sep20naptotal
time35 <- "2019-09-23 11:17"
time35E <- "2019-09-23 11:32"
Sep23nap <- difftime(time35E, time35, units = "hours")
Sep23nap
time36 <- "2019-09-24 10:35"
time36E <- "2019-09-24 11:20"
Sep24nap <- difftime(time36E, time36, units = "hours")
time37 <- "2019-09-24 14:21"
time37E <- "2019-09-24 15:03"
Sep24nap2 <- difftime(time37E, time37, units = "hours")
Sep24naptotal <- sum(Sep24nap, Sep24nap2)
Sep24naptotal
time38 <- "2019-09-25 8:40"
time38E <- "2019-09-25 9:30"
Sep25nap <- difftime(time38E, time38, units = "hours")
Sep25nap
time39 <- "2019-09-30 9:36"
time39E <- "2019-09-30 10:10"
Sep30nap <- difftime(time39E, time39, units = "hours")
Sep30nap
time40 <- "2019-10-02 8:57"
time40E <- "2019-10-02 10:14"
Oct2nap <- difftime(time40E, time40, units = "hours")
time41 <- "2019-10-02 13:23"
time41E <- "2019-10-02 14:00"
Oct2nap2 <- difftime(time41E, time41, units = "hours")
Oct2naptotal <- sum(Oct2nap, Oct2nap2)
Oct2naptotal
time42 <- "2019-10-03 9:10"
time42E <- "2019-10-03 10:25"
Oct3nap <- difftime(time42E, time42, units = "hours")
Oct3nap
time43 <- "2019-10-04 7:52"
time43E <- "2019-10-04 9:10"
Oct4nap <- difftime(time43E, time43, units = "hours")
time44 <- "2019-10-04 14:09"
time44E <- "2019-10-04 15:20"
Oct4nap2 <- difftime(time44E, time44, units = "hours")
Oct4naptotal <- sum(Oct4nap, Oct4nap2)
Oct4naptotal
time45 <- "2019-10-07 9:10"
time45E <- "2019-10-07 9:43"
Oct7nap <- difftime(time45E, time45, units = "hours")
time46 <- "2019-10-07 12:28"
time46E <- "2019-10-07 13:22"
Oct7nap2 <- difftime(time46E, time46, units = "hours")
Oct7naptotal <- sum(Oct7nap, Oct7nap2)
Oct7naptotal
time47 <- "2019-10-08 9:23"
time47E <- "2019-10-08 10:00"
Oct8nap <- difftime(time47E, time47, units = "hours")
time48 <- "2019-10-08 12:15"
time48E <- "2019-10-08 12:40"
Oct8nap2 <- difftime(time48E, time48, units = "hours")
time49 <- "2019-10-08 14:45"
time49E <- "2019-10-08 15:10"
Oct8nap3 <- difftime(time49E, time49, units = "hours")
Oct8naptotal <- sum(Oct8nap, Oct8nap2, Oct8nap3)
Oct8naptotal
time50 <- "2019-10-09 9:20"
time50E <- "2019-10-09 10:55"
Oct9nap <- difftime(time50E, time50, units = "hours")
Oct9nap
time51 <- "2019-10-10 9:18"
time51E <- "2019-10-10 9:40"
Oct10nap <- difftime(time51E, time51, units = "hours")
Oct10nap
time52 <- "2019-10-15 9:30"
time52E <- "2019-10-15 10:00"
Oct15nap <- difftime(time52E, time52, units = "hours")
time53 <- "2019-10-15 11:35"
time53E <- "2019-10-15 12:15"
Oct15nap2 <- difftime(time53E, time53, units = "hours")
Oct15naptotal <- sum(Oct15nap, Oct15nap2)
Oct15naptotal
time54 <- "2019-10-16 9:05"
time54E <- "2019-10-16 9:45"
Oct16nap <- difftime(time54E, time54, units = "hours")
time55 <- "2019-10-16 13:30"
time55E <- "2019-10-16 14:10"
Oct16nap2 <- difftime(time55E, time55, units = "hours")
Oct16naptotal <- sum(Oct16nap, Oct16nap2)
Oct16naptotal
time56 <- "2019-10-17 9:21"
time56E <- "2019-10-17 9:52"
Oct17nap <- difftime(time56E, time56, units = "hours")
time57 <- "2019-10-17 14:12"
time57E <- "2019-10-17 14:45"
Oct17nap2 <- difftime(time57E, time57, units = "hours")
Oct17naptotal <- sum(Oct17nap, Oct17nap2)
Oct17naptotal
time58 <- "2019-10-18 9:10"
time58E <- "2019-10-18 9:58"
Oct18nap <- difftime(time58E, time58, units = "hours")
time59 <- "2019-10-18 13:01"
time59E <- "2019-10-18 13:30"
Oct18nap2 <- difftime(time59E, time59, units = "hours")
Oct18naptotal <- sum(Oct18nap, Oct18nap2)
Oct18naptotal
time60 <- "2019-10-21 9:27"
time60E <- "2019-10-21 10:00"
Oct21nap <- difftime(time60E, time60, units = "hours")
time61 <- "2019-10-21 13:00"
time61E <- "2019-10-21 13:30"
Oct21nap2 <- difftime(time61E, time61, units = "hours")
Oct21naptotal <- sum(Oct21nap, Oct21nap2)
Oct21naptotal
time62 <- "2019-10-22 9:17"
time62E <- "2019-10-22 10:30"
Oct22nap <- difftime(time62E, time62, units = "hours")
time63 <- "2019-10-22 13:10"
time63E <- "2019-10-22 13:25"
Oct22nap2 <- difftime(time63E, time63, units = "hours")
Oct22naptotal <- sum(Oct22nap, Oct22nap2)
Oct22naptotal
time64 <- "2019-10-23 9:00"
time64E <- "2019-10-23 9:45"
Oct23nap <- difftime(time64E, time64, units = "hours")
time65 <- "2019-10-23 11:32"
time65E <- "2019-10-23 12:55"
Oct23nap2 <- difftime(time65E, time65, units = "hours")
Oct23naptotal <- sum(Oct23nap, Oct23nap2)
Oct23naptotal
time66 <- "2019-10-24 8:45"
time66E <- "2019-10-24 9:15"
Oct24nap <- difftime(time66E, time66, units = "hours")
time67 <- "2019-10-24 11:10"
time67E <- "2019-10-24 12:40"
Oct24nap2 <- difftime(time67E, time67, units = "hours")
Oct24naptotal <- sum(Oct24nap, Oct24nap2)
Oct24naptotal
time68 <- "2019-10-25 11:40"
time68E <- "2019-10-25 12:14"
Oct25nap <- difftime(time68E, time68, units = "hours")
Oct25nap
time69 <- "2019-10-28 10:53"
time69E <- "2019-10-28 11:45"
Oct28nap <- difftime(time69E, time69, units = "hours")
Oct28nap
?plot
13.30-12.30
beren4
getOption("max.print")
Start <- beren4$start_hour + beren4$start_minute/60
Start
End <- beren4$end_hour + beren4$end_minute/60
End
duration <- End-Start
duration
plot(beren4$age, duration)
cor.test(beren4$age, duration)
#Beren's nap time decreases as he ages, therefore this is a negative correlation.
beren3
##########

