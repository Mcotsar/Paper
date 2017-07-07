##################################    RESULTS FOR THE PAPER     ##############################################

library(ggplot2)
library(gridExtra)    
library(plyr)
library(MASS)
library(caret)
library(vegan)

getConfused <- function(allData, site1, site2)
{
    data1 <- subset(allData, site==site1)
    data2 <- subset(allData, site==site2)

    trainingSize <- min(nrow(data1), nrow(data2))

    training1 <- data1[sample(nrow(data1), trainingSize),]
    training2 <- data2[sample(nrow(data2), trainingSize),]

    training <- rbind(training1,training2)
    all <- rbind(data1, data2)

    # get 2 main PCs
    pcaTraining <- princomp(log(training[,1:8]))
    pcaAll <- princomp(log(all[,1:8]))

    valuesTraining <- data.frame(pc1=pcaTraining$scores[,1], pc2=pcaTraining$scores[,2],pc3=pcaTraining$scores[,3],pc4=pcaTraining$scores[,4],site=factor(training$site))
    values <-  data.frame(pc1=pcaAll$scores[,1], pc2=pcaAll$scores[,2],pc3=pcaAll$scores[,3],pc4=pcaAll$scores[,4],site=factor(all$site))

    # LDA
    DAmodel <- lda(values[,1:4], values$site, prior=c(1,1)/2)
    DAclass <- predict(DAmodel, values[,1:4])
    values$class <- DAclass$class

    cm <- confusionMatrix(values$class, values$site)

    # return percentage of site1 correctly classified
    return (cm$table[1,1]/sum(cm$table[,1]))

#    return(cm$overall['Accuracy'])
}


myData <- read.csv('drespaper.csv', header=T, sep=",")

##choose the type of Dressel to do the analyse
myData= subset(myData, type %in% c("Dressel C","Dressel D","Dressel E"))
myData <- myData[,5:13]
#to count the data
count(myData)

## exploratory elements with different variables 

ggplot(myData, aes(x=exterior_diam, y=rim_w, colour=site)) + geom_point()
ggplot(myData, aes(x=exterior_diam, fill=site)) + geom_bar() + facet_wrap(~site, ncol=1)
ggplot(myData, aes(x=rim_w_2, y=protruding_rim, colour=site)) + geom_point() + facet_wrap(~site)

################## PCA all dataset

logData <- log(myData[,1:8])
pcaData <- princomp(logData)

# plot to check the relevance of the first 2 PC'S
plot(pcaData)
# get the scores of the data
pc1 <- pcaData$scores[,1]
pc2 <- pcaData$scores[,2]
# put type in pcaValues
values <- data.frame(pc1=pc1, pc2=pc2, site=myData$site)

pdf("pca.pdf", width=6, height=9)
ggplot(values, aes(x=pc2, y=pc1, col=site)) + geom_point(alpha=0.5) + facet_wrap(~site,ncol=1) + xlab("PC1") + ylab("PC2") + theme_bw() + theme(legend.position="none")
dev.off()


##### Matrix distance #######

distMetrics <- matrix(0, nrow=5, ncol=5)
rownames(distMetrics) <- c('belen','delicias','malpica','parlamento','villaseca')
colnames(distMetrics) <- c('belen','delicias','malpica','parlamento','villaseca')

value <- getConfused(myData, 'parlamento', 'belen')
distMetrics['parlamento','belen'] <- value
distMetrics['belen','parlamento'] <- value

value <- getConfused(myData, 'parlamento', 'malpica')
distMetrics['parlamento','malpica'] <- value
distMetrics['malpica','parlamento'] <- value

value <- getConfused(myData, 'malpica', 'belen')
distMetrics['malpica','belen'] <- value
distMetrics['belen','malpica'] <- value

value <- getConfused(myData, 'parlamento', 'delicias')
distMetrics['parlamento','delicias'] <- value
distMetrics['delicias','parlamento'] <- value

value <- getConfused(myData, 'delicias', 'belen')
distMetrics['delicias','belen'] <- value
distMetrics['belen','delicias'] <- value

value <- getConfused(myData, 'delicias', 'malpica')
distMetrics['delicias','malpica'] <- value
distMetrics['malpica','delicias'] <- value

value <- getConfused(myData, 'villaseca', 'belen')
distMetrics['villaseca','belen'] <- value
distMetrics['belen','villaseca'] <- value

value <- getConfused(myData, 'villaseca', 'parlamento')
distMetrics['villaseca','parlamento'] <- value
distMetrics['parlamento','villaseca'] <- value

value <- getConfused(myData, 'villaseca', 'malpica')
distMetrics['villaseca','malpica'] <- value
distMetrics['malpica','villaseca'] <- value

value <- getConfused(myData, 'villaseca', 'delicias')
distMetrics['villaseca','delicias'] <- value
distMetrics['delicias','villaseca'] <- value

# normalize distMetrics
distAmphorae <- as.dist(distMetrics, diag=F)
# swap from similarity to dissimilarity
distAmphorae

# spatial distance
euclid <- read.csv('euclideanDistances.csv', header=T, sep=",")
distEuclid =as.dist(xtabs(euclid[, "spatial"] ~ euclid[, "from"] + euclid[, "to"]), diag=F)

mantel(distEuclid, distAmphorae)

