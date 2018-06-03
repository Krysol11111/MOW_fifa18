library(dplyr)
library(ROCR)
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


category = c(evaluation.pred.knn[[1]])
pred = rev(seq_along(category))
rocobject = multiclass.roc(category, pred)
auc(rocobject)
rs <- rocobject[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
#########
ggplot(data,aes(x = age,y = overall-potential)) +
  geom_point(aes(color = factor(age))) + 
  geom_smooth(method = "lm") +
  xlab("Player Age") + 
  ylab("Overall - Potential Rating") + 
  ggtitle("Player Age vs Overall - Potential Rating") + 
  theme(plot.title = element_text(hjust = 0.5))  

age_overall <- data %>%
  select(age, overall) %>%
  group_by(age, overall) %>%
  count()

p <- ggplot(age_overall, aes(x = age, y = overall)) +
  geom_point(aes(size = n, color = n)) +
  geom_smooth(method = "loess", color = "red") +
  theme(legend.position = "none") +
  ggtitle("Correlation between Age and Overall")
print(p)

# Age Density

ggplot(data,aes(x = age, fill = factor(age))) + 
  geom_bar() + 
  guides(fill = FALSE) + 
  xlab("Player Age") + 
  ylab("Number of Players") + 
  scale_x_continuous(breaks = c(16:47)) + 
  ggtitle("Player Age") + 
  theme(plot.title = element_text(hjust = 0.5))


######

plot(evaluation.model.rf[[1]]$finalModel, col=1:8, main = "Final model", log="y")
########
evaluation.model.knn[[1]]$results$model <- 't1'
evaluation.model.knn[[2]]$results$model <- 't2'
evaluation.model.knn[[3]]$results$model <- 't3'
evaluation.model.knn[[4]]$results$model <- 't4'
results <- union(evaluation.model.knn[[1]]$results, evaluation.model.knn[[2]]$results)
results <- union(results, evaluation.model.knn[[3]]$results)
results <- union(results, evaluation.model.knn[[4]]$results)
results$model <- as.factor(results$model)
knn_plot <- ggplot(data=results, aes(x=k, y=Accuracy, group=model, colour=model))+
  geom_line()+
  theme(legend.text=element_text(size=9),legend.position="bottom")+
  scale_x_continuous(breaks = seq(min(results$k), max(results$k), by=1))+
  geom_point()

print(knn_plot)
########
tab <- confusionMatrix(evaluation.model.knn[[1]])
tab <- tab$table / colSums(tab$table)
confusion <- as.data.frame(tab)

plot <- ggplot(confusion) + 
  geom_tile(aes(x=Reference,y=Prediction, fill = Freq)) + 
  geom_text(aes(x=Reference, y= Prediction, label = round(Freq,2))) +
  scale_x_discrete(name = "Actual Class")+ 
  scale_y_discrete(name = "Predicted class") + 
  scale_fill_gradient(breaks = seq(from=0,to=1,by=0.2)) +
  labs(fill="Normalized\nFrequency") +
  theme(legend.text=element_text(size=15), legend.position = "bottom", legend.key.width = unit(1.5,"cm"))

print(plot)
#ACCURACY
predict_data <- evaluation.pred.rf[[4]]
test_data <- evaluation.testLabels[[4]]
for (pos in unique(data$label))
{
  a <- length(predict_data[test_data == pos & predict_data == pos])
  b <- length(test_data[test_data == pos])
  c <- length(predict_data[predict_data == pos & test_data != pos])
  val <- (a/(b+c))
}


##############
tp <- tn <- fn <- fp <- c()
allTp <- allTn <- allFn <- allFp <- c()
accuracy <- errorRate <- precision <- recall <- c()
predict_data <- evaluation.pred.knn[[1]]
test_data <- evaluation.testLabels[[1]]
for( class in unique(data$label))
{
  classPredictions <- predict_data[test_data == class]
  otherPredictions <- predict_data[test_data != class]
  allTp[[class]] <- tp <- sum(classPredictions == class)
  allTn[[class]] <- tn <- sum(otherPredictions != class)
  allFn[[class]] <- fn <- sum(classPredictions != class)
  allFp[[class]] <- fp <- sum(otherPredictions == class)
  
  accuracy[[class]] <- (tp + tn) / (tp + tn + fn + fp)
  errorRate[[class]] <- (fp + fn) / (tp + tn + fn + fp)
  
  precision[[class]] <- tp /(tp + fp)
  recall[[class]] <- tp / (tp + fn)
}

l <- length(unique(data$label))
measures <- c()

measures$avgAccuracy <- sum(accuracy) / l
measures$avgError <- sum(errorRate) / l
a <- measures$avgPrecision <- sum(precision) / l
b <- measures$avgRecall <- sum(recall) / l
measures$avgFscore <- (2 * a * b) / (a + b)
