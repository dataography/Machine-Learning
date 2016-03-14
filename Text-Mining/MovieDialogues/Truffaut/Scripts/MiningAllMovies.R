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

subtitleDoc <- read_file("~/your working directory/all.txt")
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

# We can order freq table as below for more insight
orderedFreq <- order(freq)
orderedFreq

  
# Remove sparse terms (if any) 
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.
# in our case, no sparse entry
# <<DocumentTermMatrix (documents: 1, terms: 8659)>>
#   Non-/sparse entries: 8659/0
# Sparsity           : 0%
# Maximal term length: 25
# Weighting          : term frequency (tf)



#Frequencies of each term frequencies

head(table(freq), 20)   
# freq
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
# 4500 1311  650  415  249  193  170  110   83   82   64   61   40   51   50   35   30   28   20   15 

tail(table(freq), 20)   
#freq
# 281 294 313 340 342 343 347 348 366 367 369 401 410 431 443 451 519 522 657 715 
# 1   1   1   1   1   1   1   1   1   1   1   2   1   1   1   1   1   1   1   1

  
# tune "50" according to your data.  
f<-findFreqTerms(dtm, lowfreq=50, highfreq = 750)   
plot(tdm.as.matrix)
#

## ---- PLOTTING FREQUENCIES -----
  
wf <- data.frame(words=names(freq), frequency=freq)   
p <- ggplot(subset(wf, freq>=1), aes(words, frequency))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p <- p + ggtitle("François Truffaut - all movies - words having frequency > 150")
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
rect.hclust(fit, k = 6) # cut tree into 6 clusters 

## ------K MEANS CLUSTERING  ------

# k = number of cluster, distance is set to be  euclidian.
d <- dist(t(dtm), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  



