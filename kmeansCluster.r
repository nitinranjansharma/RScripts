setwd('/Users/nitinranjansharma/Documents/Nitin/TestFiles/Clustering/Data Set')
searchkeywords <- read.csv("searchkeywords.csv")
searchkeywords$keyword<-gsub("[[:punct:]]", " ",searchkeywords$keyword)

dtm <- create_matrix(searchkeywords$keyword , stemWords = TRUE, removeStopwords = FALSE, minWordLength = 1, removePunctuation = TRUE) 

distmatrix <- dist(scale(dtm), method = "euclidean")
kmeans5 <- kmeans(distmatrix ,5)

kw_with_cluster <- data.frame(cbind(searchkeywords, kmeans5$cluster))


kw_with_cluster <- data.frame(cbind(searchkeywords,kmeans5$cluster))
names(kw_with_cluster)[4] <- c("kmeans5")


kw_with_cluster$num=1
cluster1 <- kw_with_cluster[kw_with_cluster$kmeans5 == 1,]
cluster2 <- kw_with_cluster[kw_with_cluster$kmeans5 == 2,]
cluster3 <- kw_with_cluster[kw_with_cluster$kmeans5 == 3,]
cluster4 <- kw_with_cluster[kw_with_cluster$kmeans5 == 4,]
cluster5 <- kw_with_cluster[kw_with_cluster$kmeans5 == 5,]

library(wordcloud)
library(RColorBrewer)

word.cloud = function(cluster, max, min)
{
  doc.corpus <- Corpus(VectorSource(cluster[,'keyword']))
  doc.corpus <- tm_map(doc.corpus, tolower)
  doc.corpus <- tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  my_stopwords <- c(stopwords('english'),'http')
  doc.corpus <- tm_map(doc.corpus , removeWords, my_stopwords)
  
  pal2 <-  brewer.pal(8, "Dark2")
  wordcloud(doc.corpus, max.words = max, min.freq = min, random.order = T, colors = pal2)
  rm(doc.corpus)
  
  
  
}


word.cloud(cluster1, 5,100)

findAssocs(dtm,'content',0.1)

kw_with_cluster <- data.frame(cbind(searchkeywords, kmeans5$cluster))

aggregate(kw_with_cluster$number_searches, by = list(kmeans5$cluster),mean)

aggregate(kw_with_cluster$cpc, by = list(kmeans5$cluster),mean)


word.cloud(cluster4, 20,5)
head(kw_with_cluster)

# multiple clusters to choose the elbow
cluster_vs_cost <- data.frame()

for(i in 1:20)
{
  kmeans <- kmeans( x = dtm, centers = i, iter.max = 20)
  
  cluster_vs_cost <- rbind(cluster_vs_cost, cbind(i, kmeans$tot.withinss))
  
}

names(cluster_vs_cost) <- c("cluster","cost")
head(cluster_vs_cost)

library(ggplot)
ggplot(data=cluster_vs_cost, aes(x=cluster, y=cost)) + 
  theme_bw(base_family="Arial") + 
  geom_line(colour = "blue") +
  theme(text = element_text(size=10)) +
  ggtitle("Reduction In Cost For Values of 'k'") +
  xlab("Clusters") + 
  ylab("Within-Cluster Sum of Squares")

# optimal cluster is 14