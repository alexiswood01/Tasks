setwd("~/Desktop/Evolution/Tasks/final")
data1 <- read.csv("~/Desktop/Evolution/Tasks/final/Espeset2021_AnEntSoc_1.csv", header=TRUE, stringsAsFactors=FALSE)
data1
# hypothesis: There is significant evidence in the Cabbage White Butterfly to show that the 
# wing length of the males have a positive correlation to the size of the reproductive parts.

par(mar=c(4,4,1,1), las=1)
plot(data1$WingLength, data1$TestesWeight, pch=16, xlab="wing length", ylab="testes weight")
cor.test(data1$WingLength, data1$TestesWeight)