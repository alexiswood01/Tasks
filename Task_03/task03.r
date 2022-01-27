trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
#Yes, the samples are different. The populations were also different because the values are different. 
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)
nrow(MatGrandma)
nrow(MatGrandpa)
nrow(PatGrandma)
nrow(PatGrandpa)
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Alan)
nrow(Alan)
head(Brenda)
nrow(Brenda)
Focus <- makeBaby(Brenda, Alan)
#The number should be 50% since baby gets half from mom and half from dad.
ToMom <- length(grep("mom", Focus))/length(Focus)
ToMom
#The number should be 25% since the baby gets a quarter from his grandparents, 
#because the parents got 50%
ToMomMom <- length(grep("grandma_mom", Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)
ToMomMom
ToMomDad
ToDadMom <- length(grep("grandma_da", Focus))/length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/length(Focus)
ToDadMom
ToDadDad
#Focus is not equally related to each grandparent, which is not what I expected.
#When adding his relatedness to grandparents and dividing by 4, the average 
#relatedness is 25%.
Sibling_01 <- makeBaby(Brenda, Alan)
#I expect the sibling to share 50% DNA with Focus.
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ToSib
#The amount actually shared is 60%, and I would expect it to be around this 
#number with each sibling. 
ManySibling <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
ManySibling
quantile(ManySibling)
mean(ManySibling)
plot(density(ManySibling), main="", xlab="proportion shared genes")
#This could be due to genetic recombination between the parents, and the variation
#of the combinations possible. The average is 50%, which is what was expected.
HWE <- function(p) {
  aa <- p^2
  ab <- 2*p*(1-p)
  bb <- (1-p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq,allele a", ylab="geno.freq")
p <- seq(from=0, to=1, by=0.1)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#As the allele a increases, the frequency of aa individuals increases. As the 
#allele a decreases, the frequency of aa individuals decreases until 0. The time
#is not shown on this plot, and neither is the geographic space. 
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#It doesn't exactly match, but it is very similar. With higher populations, the 
#values would be much closer together. With a lower population, the values would
#be very spread out. 
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#The values have a much wider variation across the trend line. This is because
#with a smaller sample number, the deviation inceases. 
install.packages("learnPopGen")
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x
x <- genetic.drift(Ne=600, nrep=5, pause=0.01)
x
x <- genetic.drift(Ne=50, nrep=5, pause=0.01)
x
#With Ne=200, the lines are a bit scattered but still in a reasonable range. 
#With Ne=600, the lines are much closer together with but still with a little deviation.
#With Ne=50, the lines are all over the place. This could mean that with a higher 
#value, there is more closely related lines. 
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
tExt
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt ~ Samples+0)
abline(Line2)
#Line is slightly above Line2, with adding +0, Line2 does not have an intercept. 
#With smaller population size, the points are closer together, and with a bigger
#population size, the points are further apart. This means that as population
#size increases, there is mor variation and outliers. 
install.packages("MASS")
library(MASS)
LineA <- rlm(tExt ~ Samples)
abline(LineA)
LineA$coef
summary(LineA)
#With adding this line, the slope decreased. 

