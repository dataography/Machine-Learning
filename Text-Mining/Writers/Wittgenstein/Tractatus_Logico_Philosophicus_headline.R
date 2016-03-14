library(tm)
library(readr)
library(XML)
library(lsa)
library(SnowballC) 
library(ggplot2) 
library(wordcloud)
library(cluster)
library(fpc)  


## -------DATA LOADING --------
subtitleDoc <- read_file("~/Dropbox/R/Training/Text Mining/Tractatus Logico Philosophicus headlines.txt")
typeof(subtitleDoc)
subtitleDoc <- Corpus(VectorSource(subtitleDoc))

## -------DATA CLEANING------------

# before cleaning
writeLines(as.character(subtitleDoc))

#check the type of the doc
typeof(subtitleDoc)


subtitleDoc <- tm_map(subtitleDoc, tolower)  
subtitleDoc <- tm_map(subtitleDoc, removePunctuation, mc.cores=2)     
subtitleDoc <- tm_map(subtitleDoc, removeWords, stopwords("english"), mc.cores=2)  
subtitleDoc <- tm_map(subtitleDoc, stemDocument, language = "english", mc.cores=2)     
subtitleDoc <- tm_map(subtitleDoc, removeNumbers, mc.cores=2)     
subtitleDoc <- tm_map(subtitleDoc, stripWhitespace, mc.cores=2)  
#subtitleDoc <- tm_map(subtitleDoc, function(x) removeWords(x, stopwords("english")), mc.cores=2)
 




subtitleDoc <- tm_map(subtitleDoc, stemDocument, language = "english", mc.cores=2)   # *Removing common word endings* (e.g., "ing", "es")   
subtitleDoc <- tm_map(subtitleDoc, removeWords, stopwords("english"))   # *Removing "stopwords" 

# After cleaning
writeLines(as.character(subtitleDoc))
subtitleDoc <- tm_map(subtitleDoc, PlainTextDocument)
summary(subtitleDoc)

dtm <- DocumentTermMatrix(subtitleDoc, control=list(minWordLength=1))  
dtm
tdm <- TermDocumentMatrix(subtitleDoc, control=list(minWordLength=1)) 
tdm

# Convert dtm and tdm into matrices
dtm.as.matrix <- as.matrix(dtm)
tdm.as.matrix <- as.matrix(tdm)

# Lets look at frequencies of terms  
freq <- colSums(dtm.as.matrix) 
freq
summary(freq)
length(freq) 

#we can order freq table as below for more insight
ord <- order(freq)
ord

  
#  Start by removing sparse terms (if any) 
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.
# in our case, no sparse entry
# <<DocumentTermMatrix (documents: 1, terms: 8659)>>
#   Non-/sparse entries: 8659/0
# Sparsity           : 0%
# Maximal term length: 25
# Weighting          : term frequency (tf)



#Frequencies of each term frequencies

#head(table(freq), 20)   


#tail(table(freq), 20)   


  
# tune "50" according to your data.  
f<-findFreqTerms(dtm, lowfreq=1, highfreq = 1000)   
plot(tdm.as.matrix)
#

## ---- PLOTTING FREQUENCIES -----
  
wf <- data.frame(words=names(freq), frequency=freq)   
p <- ggplot(subset(wf, freq>=1), aes(words, frequency))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p <- p + ggtitle("Word frequencies for headlines")
p   

## ---- ASSOCIATION -----
# list the terms that is associated with 'a term' with 0.25 corr or above?
findAssocs(tdm, "a term", 0.25)



## ---- WORD CLOUD MAP ----- 
  
dark2 <- brewer.pal(8, "Dark2")   
# will create a cloud of max 100 words
wordcloud(names(freq), freq, max.words=200, rot.per=0.1, colors=dark2)    



## -----HIERARCHICAL CLUSTERING ------   

# Distance metric between words

d <- dist(t(dtm), method="euclidian")  
fit <- hclust(d=d, method="ward")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)     
rect.hclust(fit, k=5, border="red")   

# another way of clustering

distMatrix <- dist(scale(tdm.as.matrix))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6) 

## ------K MEANS CLUSTERING  ------

# k = number of cluster, distance is set to be  euclidian.
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=2)  




