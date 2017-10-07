setwd('/Users/nitinranjansharma/Documents/Nitin/TestFiles/Sentiment Analysis/Data Set')
reviews <- read.csv('reviews.csv',stringsAsFactors = FALSE)
reviews<-reviews[,c(5,7,8)]


dim(reviews)
names(reviews)[2]<-"reviews"
library(tm)
tweets.corpus<-Corpus(VectorSource(reviews$reviews))
summary(tweets.corpus)
inspect(tweets.corpus[1:5])

tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'available') 
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)

tweets.text<-reviews$reviews
head(tweets.text)
pos[2007:2014]<-c("spectacular","everyday","better","top","thumbs","four","five",'thumbs up')
neg[4784:4789]<-c("one","two","careful","sync","Beware","suck")
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub('[[:punct:]]', '', sentence) #removes punctuations
    sentence = gsub('[[:cntrl:]]', '', sentence) #removes control characters
    sentence = gsub('\\d+', '', sentence) #removes digits

    sentence = tolower(sentence)

    word.list = str_split(sentence, '\\s+')

    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

analysis<-score.sentiment(tweets.text, pos, neg, .progress="text")
names(analysis)
View(analysis)
str(analysis)

table(analysis$score)
mean(analysis$score)
hist(analysis$score)

# training data


analysis$text<-as.character(analysis$text)
str(analysis)
analysis$sentiment<-ifelse(analysis$score>0,"positive",
                           ifelse(analysis$score<0,"negative","neutral"))
table(analysis$sentiment)
#Cleaning the data again
analysis$text = gsub('[[:punct:]]', '', analysis$text)
str(analysis)
head(analysis,5)

sampling <- sort(sample(nrow(analysis) * 0.7))

length(sampling)
head(sampling)
names(sampling)
train.tweets <- analysis[sampling,]
test.tweets<- analysis[-sampling,]

prop.table(table(train.tweets$sentiment))
prop.table(table(test.tweets$sentiment))

#classification

set.seed(2000)
install.packages("RTextTools")
library(RTextTools)

install.packages("e1071")
library("e1071")
dim(test.tweets)
dim(train.tweets)
names(train.tweets)
names(test.tweets)

test.tweets$type = "test"
train.tweets$type = "train"

tweets <- rbind(train.tweets,test.tweets)
head(tweets[,2])

matrix= create_matrix(tweets[,2], language="english",removeNumbers=TRUE, removePunctuation=TRUE, 
                      removeSparseTerms=0, 
                      removeStopwords=TRUE, stripWhitespace=TRUE, toLower=TRUE)

mat <- as.matrix(matrix)

container = create_container(mat, as.numeric(as.factor(tweets[,3])),
                             trainSize=1:244, testSize=245:349,virgin=FALSE)

models = train_models(container, algorithms=c("MAXENT", "SVM", "RF", "BAGGING", "TREE"))


results = classify_models(container, models)
class(results)
head(results)



table(as.numeric(as.factor(tweets[245:349,3])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(tweets[245:349,3])), results[,"MAXENTROPY_LABEL"])
table(as.numeric(as.factor(tweets[245:349,3])), results[,"TREE_LABEL"])
table(as.numeric(as.factor(tweets[245:349,3])), results[,"BAGGING_LABEL"])
table(as.numeric(as.factor(tweets[245:349,3])), results[,"SVM_LABEL"])

recall_accuracy(as.numeric(as.factor(tweets[245:349,3])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349,3])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349,3])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349,3])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349,3])), results[,"SVM_LABEL"])

analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)

N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")
cross_validate(container,N,"BAGGING")

results1<-results[,c(1,3,5,7,9)]

results1$majority=NA
for(i in 1:nrow(results1))
{
  #Getting the frequency distribution of the classifications 
  print(i)
  p<-data.frame(table(c(results1$MAXENTROPY_LABEL[i],results1$TREE_LABEL[i],results1$FORESTS_LABEL[i],
                        results1$SVM_LABEL[i],results1$BAGGING_LABEL[i])))
  #Choosing the classification that occurs maximum
  #Putting this value into the new column "majority"
  
  results1$majority[i]<-paste(p$Var1[p$Freq==max(p$Freq)])
  rm(p)
}
results1$majority<-as.numeric(results1$majority)
table(results1$majority)