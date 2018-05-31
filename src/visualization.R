# probably it should be renamed to dataAnalysis or something similar
# we need to compute all matrixes, variables and other stuff to see some correlation etc.
# then use those matrixes and variables to generate interesting plots which will support our hypothesis
# probably ROC plot, coeffient matrix, correlation plot etc.

plot(finalBoruta, xlab = "", xaxt = "n")
k <-lapply(1:ncol(finalBoruta$ImpHistory),function(i)
  finalBoruta$ImpHistory[is.finite(finalBoruta$ImpHistory[,i]),i])
names(k) <- colnames(finalBoruta$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(finalBoruta$ImpHistory), cex.axis = 0.7)
