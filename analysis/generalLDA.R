################# Create a training set for LDA

# sample size for each group must be the same here (42)

trainingSize = min(count(myData,'site')$freq)
  
amph1 <- subset(myData, site=="delicias")
training1 <- amph1[sample(nrow(amph1), trainingSize),]
amph2 <- subset(myData, site=="malpica")
training2 <- amph2[sample(nrow(amph2), trainingSize),]
amph3 <- subset(myData, site=="belen")
training3 <- amph3[sample(nrow(amph3), trainingSize),]
amph4 <- subset(myData, site=="parlamento")
training4 <- amph4[sample(nrow(amph4), trainingSize),]
amph5 <- subset(myData, site=="villaseca")
training5 <- amph5[sample(nrow(amph5), trainingSize),]
  
training <- rbind(training1,training2)
training <- rbind(training, training3)
training <- rbind(training, training4)
training <- rbind(training, training5)

logTraining <- log(training[,1:8])
pcaTraining <- princomp(logTraining)

plot(pcaTraining)
trainingPc1 <- pcaTraining$scores[,1]
trainingPc2 <- pcaTraining$scores[,2]

trainingValues <- data.frame(pc1=trainingPc1, pc2=trainingPc2, site=training$site)
    
#######  Discriminant Analysis   ##########

# training
amphDA <- qda(site~pc1+pc2, data=trainingValues, prior=c(1,1,1,1,1)/5)
# testing with all dataset
predQda <- predict(amphDA, values)
    
values$probDelicias<- predQda$posterior[,"delicias"]
values$probMalpica <- predQda$posterior[,"malpica"]
values$probBelen <- predQda$posterior[,"belen"]
values$probParlamento <- predQda$posterior[,"parlamento"]
values$probVillaseca <- predQda$posterior[, "villaseca"]
values$class <- predQda$class


####  Confusion Matrix  #####

confusionMatrix(values$site, values$class)

correct <- subset(values, site==class)
incorrect <- subset(values, site!=class)
correct$predict <- "correct"
incorrect$predict <- "incorrect"

result <- rbind(correct, incorrect)

pdf('confusion.pdf', width=15, height=8)
ggplot(result, aes(x=pc2, y=pc1, col=predict)) + geom_point(alpha=0.5) + facet_wrap(~site, ncol=2) + theme_bw()
dev.off()
