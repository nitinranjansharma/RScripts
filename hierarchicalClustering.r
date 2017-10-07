# building tdm corpus from 

library(igraph)
setwd("/Users/nitinranjansharma/Documents/Nitin/TestFiles/Network Analysis of Tweets/Data Set")
obama1 <- read.csv("obama1.csv",h = FALSE)
obama2<-read.csv("obama2.csv",h=FALSE)
obama3<-read.csv("obama3.csv",h=FALSE)
obama4<-read.csv("obama4.csv",h=FALSE)

obama <- rbind(obama1,obama2,obama3,obama4)

str(obama)

names(obama)

tweets <- data.frame(obama$V2)
names(tweets) <- "TweetText"
str(tweets)
library(tm)

tweets.corpus <- Corpus(VectorSource(tweets$TweetText))
summary(tweets.corpus)
inspect(tweets.corpus[1:5])

tweets.corpus<-tm_map(tweets.corpus,tolower) #Converting to lower case
tweets.corpus<-tm_map(tweets.corpus,stripWhitespace) #Removing extra white space
tweets.corpus<-tm_map(tweets.corpus,removePunctuation) #Removing punctuations
tweets.corpus<-tm_map(tweets.corpus,removeNumbers) #Removing numbers
my_stopwords<-c(stopwords('english'),'http*') 
tweets.corpus<-tm_map(tweets.corpus,removeWords,my_stopwords)

tweets.tdm <- TermDocumentMatrix(tweets.corpus)
tweets.rs <- removeSparseTerms(tweets.tdm,0.97)
tweets.rsmatrix <- as.matrix(tweets.rs)

#creating distance matrix to feed it to clustering algorithm
distmatrix  <- dist(scale(tweets.rsmatrix),method = "euclidean")

#creating hierarchical matrix
tweets.hm <- hclust(distmatrix, method = "ward.D2")
k = cutree(tweets.hm,k=6)


#install.packages("ggdendro")
library(ggdendro)
#ggdendrogram( tweets.hm,cex = 0.9)
ggdendrogram(tweets.hm, theme_dendro = FALSE,color = "tomato")
