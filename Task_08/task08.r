library(ape)
library(phytools)
text.string<-"(((((((cow, pig), whale), (bat,(lemur,human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
##Q1: shark and goldfish are more closely related
vert.tree
str(vert.tree)
##Q2: there are no branches
tree<-read.tree(text="(((A,B),(C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<-force.ultrametric(read.tree("http://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges<- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
##Q3 a tree with no tip labels
plot(AnolisTree, cex=0.25, show.tip.label = FALSE)
##Q4 a tree that is plotted as a circle
plot(AnolisTree, type="radial", cex=0.25)
##Q5 a tree with the tips colored red instead of black
plot(AnolisTree, cex=0.25, tip.color = "red")
##Q6-8 find which living, named species has the shortest edge length, then drop that
## tip from the tree, then plot the resulting tree.
which.min(AnolisTree$edge.length)
AnolisTree2<-drop.tip(AnolisTree, 82)
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
##Q9 It increases in an exponential fashion. It does not go down because the past lineages
## have not went extinct. The slope varies. There is increasing diversity and periods
## of time where the line remains flat until there is a new event to increase diversity.
##Q10 
fit.bd(AnolisTree, rho=0.2)
## 'b'=lambda-mu = 0.8031, 'd'=mu/lambda = 0
