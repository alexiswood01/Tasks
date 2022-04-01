setwd('/Users/lexywood/Desktop/Evolution/Tasks/Task_10')
library(phytools)
trees <- list()
births <- vector()
Fractions <- vector()
netdiversification <- vector()
speciationrate <- vector()
Avgbranchlength <- vector()
pbtree
for (i in 1:100) {
  births [i] <- runif(1, 0, 1)
  Fractions [i] <- runif(1, 0, 1)
  trees[[i]] <- pbtree(n=100, b=births [i], d=Fractions [i] * births[i])
  netdiversification[[i]] <- (births[i] - Fractions[i]*births[i])
  speciationrate[i] <- births[i]
  Avgbranchlength [[i]] <- mean(trees[[i]] $edge.length)
}
##Q4
netdiversification <- (births - Fractions* births)
totaltreetips <- log(sapply(trees, Ntip))
length(totaltreetips)
length(netdiversification)
plot(netdiversification, totaltreetips)
Q4plot <- plot(netdiversification, totaltreetips)
line <- lm(totaltreetips ~ netdiversification)
abline(line)
## They behave in a similar matter so as net diversification increases, the log of total tips do as well
##Q5
plot(speciationrate, Avgbranchlength)
## They behave in a similar manner. 
##Q6
cor(speciationrate, Avgbranchlength)
## There is no significant correlation.
##Q7
trees
trees[73]
Tree <- trees[[73]]
rates <- vector()
traits <- list()
plot(Tree) 
for (i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(Tree, sig2=rates[i])
} 
##Q8
head(traits)
length(traits) 
#100
mtraitsA <- sapply(traits, mean)
mtraitsratecor <- cor(mtraitsA, rates) 
plot(mtraitsA, rates)  
## There is no significant correlation of the mean between the two objects.
##Q9
vtraitsA <- sapply(traits, var)
vtraitsratecor <- cor(vtraitsA, rates)
plot(vtraitsA, rates)
## There is a positive correlation of variance between the two objects.
##Q10
element1<-sapply(traits, "[[",1)
element2<-sapply(traits, "[[",2)
traitMat <- cbind(element1, element2)
element1and2cor <- cor(element1, element2) 
plot(element1, element2)
## There is some correlation of element 1 and element 2 due to the
# relatedness in the phylogeny tree. This isn't significant since all traits
# are randomly generated and have positive correlation each time in some way.
# Therefore the positive correlation shown by element 1 and element 2 is 
# not proving anything significant other than their phylogeny tree.
## EC
Tree2 <- pbtree(n=100)
X <- fastBM(Tree2, nsim=2)
phylomorphospace(Tree2, X, xlab="element1", ylab="element2")

