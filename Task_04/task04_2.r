setwd("/Users/lexywood/Desktop/Evolution/Tasks/Task_04")
results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
results
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
counts
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgrounds
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288db", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
Chisqs
plotChis(counts)
##When the chi square is high, the bars very uneven, with one bar being extremely higher
##than the others. When it is lower, the bars seem to get closer to being even, and
##seem exactly even at 0. The plotChis function helps to interpret values by knowing
##the higher the value, the less even it is. 
Avg <- mean(Chisqs)
Avg
## The average Chi-squared is 61, therefore the bars are not very even.
## The observed value and expected value differ a lot, and the avg is larger than the critical.
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
## The Chi-squared does differ by background.
propSig <- length(which(Chisqs>11.70))/length(Chisqs)
percSig <- round(100*propSig)
propSig
percSig
##I think the there is more than just natural selection being the reason the number
##is so high.
par(las=1, mar=c(4,4,1,1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1)
hist(Chisqs, main="", xlab="chi-sqaured values", ylab="frequency")
par(las=1, mar=c(4,4,1,1), mgp=c(2, 0.5, 0), tck=-0.01, cex.axis=1)
plot(1 , 1, xlim=c(0,400), ylim=c(1,8.5), xlab="", ylab="", type="n",yaxt="n")
axis(2, at= 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] == i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1
}
abline(v=11.70, lty=2, lwd=2, col='black')
##Yes, only the first quarter of the trials are behind the black line.
Simulation <- simDraws(10000)
Simulation
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
#The selection free simulation found a significant result 95% of the time.
#There is no natural selection, so the observed is closer to the expected when compared
#to counts where selection did occur.
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4,w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel.sim.")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
##Most students do show evidence of strong selection, I would say
##the different evolutionary progresses across all the groups
##are mostly similar, with a few groups being outliers.
#INFERENCE
#Evolutionary processes done in the lab by humans include illustrating the process of natural
#selection by using inanimate objects. In more complex labs, they can use live animals
#such as mice, or even plants. Similar processes are done by computers, except the random
#simulations can include more processes such as genetic drift or mutation. 
#The graphs can tell us the relative strength simulated by lab students are very similar, and
#when compared to the graph simulated by computers, it is slightly similar. 
#Comparing the student numbers to simulated numbers tells me more about the processes.
#The chi-square value would increase because adding mutations change the values and make
#the graph more uneven.




