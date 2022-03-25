setwd('/Users/lexywood/Desktop/Evolution/Tasks/Task_09')
library(phytools)
tree<-force.ultrametric(read.tree("http://jonsmitchell.com/data/anolis.tre"))
plot(tree, type="fan")
Ntip(tree)
tree$edge.length
tree
#Q1 There are 82 tips, and there are branch lengths. 
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
data
#Q2 "data" is the different species of lizards that correspond with their SLV. 
svl <- setNames(data$svl, rownames(data))
svl
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
#Q3 (CI95) is the 95-percent confidence interval. estimated values are stored in the node. 
#Q4 Assumes the state compund for the root node = MLE. Assumes there is a 95% confidence interval
par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c(
  "Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", 
  "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilNodes <- c()
nodeN <- c()
head(fossilData)
for(i in 1:6) {
  Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes[i] <- fossilData[i, "svl"]
  nodeN[i]<- Node
}
i
names(fossilNodes)<- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
#Q7 Fossils increased the estimated ancestral time. 
#Q8-10 The ebFit model fits the data the best using evidence of the lowest AIC.
# fitContinuous compares continuous comparative data while fastAnc performs a 
# fast estimate of ML ancestral states. Both assume continuous traits and 95% CI.
install.packages("gieger")
library("gieger")
deltaFit <- fitContinuous(tree, data, model= "delta") #AIC= -6.106526
lambdaFit <- fitContinuous(tree, data, model= "lambda") #AIC= -4.512027
kappaFit <- fitContinuous(tree, data, model="kappa") #AIC= -4.512027
OuFit <- fitContinuous(tree, data, model="OU") #AIC= -4.512027
ebFit <- fitContinuous(tree, data, model="EB") #AIC= -7.235124
brownFit <- fitContinuous(tree, data) #AIC= -6.512027
meantrendfit <- fitContinuous(tree, data, model="mean_trend") #AIC= -4.512027
whiteFit <- fitContinuous(tree, data, model="white") #AIC= 91.391102
ratetrendfit <- fitContinuous(tree, data, model="rate_trend") #AIC= -6.981365









