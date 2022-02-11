install.packages("coala")
library(coala)
install.packages("phytools")
library(phytools)
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
model <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) + feat_mutation(10) + feat_recombination(10) + sumstat_trees() + sumstat_nucleotide_div()
model
stats <- simulate(model, nsim=1)
Diversity <- stats$pi
Diversity
##The numbers are not the same. This could be an effect of the mutation and recombination rates.
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
##Each number possibly shares a common ancestor.
Age1 <- max(nodeHeights(t1))
Age1
t2 <- read.tree(text=stats$trees[[2]][1])
t2
plot(t2)
axisPhylo()
##The most recent common ancestor on this SNP looks barely higher than 0, while the first SNP was less than 0.1. 
##They do not match, but they are very close to each other.  
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1,t2)
##The red line is slightly behind the blue line. They are not directly on top of each other, so they aren't a match.
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1,t1_2)
##They do not match. The most recent ancestor for t1 is much older than t2, as it is at 3. 
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus==1 && n==1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
##Changing recombination rate to 100. By increasing the recombination, the diversity in the loci
##should also increase, and the furthest common ancestor would be younger. 
model_2 <- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2) + feat_mutation(10) + feat_recombination(100) + sumstat_trees() + sumstat_nucleotide_div()
model_2
stats <- simulate(model_2, nsim=1)
Diversity <- stats$pi
Diversity
Nloci <- length(stats$trees)
t3 <- read.tree(text=stats$trees[[1]][1])
plot(t3)
axisPhylo()
compare.chronograms(t1,t3)
##The furthest common ancestor is younger than in t1, which is what I predicted.
model3 <- coal_model(10, 50) + feat_mutation(par_prior("theta", sample.int(100, 1))) + sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(mean_pi, theta, type="p", xlab="mean_pi", ylab="theta")
LineA <- lm(mean_pi~theta)
abline(LineA)
library(learnPopGen)
?coalescent.plot
plot1 <- coalescent.plot(n=10, ngen=20, colors = NULL)
plot1
plot2 <- coalescent.plot(n=10, ngen=30, colors = NULL)
plot2
plot3 <- coalescent.plot(n=10, ngen=40, colors = NULL)
plot3
##Q1 10 alleles, which can be changed by the variable "n"
##Q2 After about 11 generations, one allele goes to fixation, based on plot 2 and 3. 
##Q3 1. Variance is 0.8.
##Q4 The more fit an animal with certain alleles is, the more that allele will progress in number for each generation.
##Q5 No.

  