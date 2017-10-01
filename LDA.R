library(RTextTools)
library(lda)
library(tm)
data("NYTimes") 
data <- NYTimes[sample(1:3100,replace=FALSE,size = 1000),]

matrix= create_matrix(cbind(as.vector(data$Title),as.vector(data$Subject)), language="english",removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

getinspect(matrix[1:5,1:5])

dim(data)
head(data)



k <- length(unique(data$Topic.Code))

train <- matrix[1:700,]
test <- matrix[701:1000,]

train.lda <- LDA(train,k)

get_topics(train.lda,5)
get_terms(train.lda,5)


setwd("/Users/nitinranjansharma/Documents/Nitin/Files")

train.topic <- topics(train.lda)

test.topic <- posterior(train.lda,test)

test.topic$topics[1:10,1:5]
head(test.topic)

test.topics <- apply(test.topic$topics, 1, which.max)

test<-data[701:1000,]
final<-data.frame(Title=test$Title,Subject=test$Subject,Pred_topic=test.topics)
View(final)

View(final[final$Pred_topic==22,])

best.model <- lapply(seq(2,10, by=1), function(k){LDA(matrix,k)})

best_model<- as.data.frame(as.matrix(lapply(best.model, logLik)))
final_best_model <- data.frame(topics=c(seq(2,10, by=1)), 
                               log_likelihood=as.numeric(as.matrix(best_model)))

head(final_best_model)
library(ggplot2)
with(final_best_model,qplot(topics,log_likelihood,color="red"))

#Based on the graph, we can choose the best model
k=final_best_model[which.max(final_best_model$log_likelihood),1]

cat("Best topic number is k=",k)
