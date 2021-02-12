#Using tm and pdf tools package, we can analyze data from pdf files

getwd()
library(tm)
library(SnowballC)
library(topicmodels)
library(wordcloud)
library(stringr)
library(reshape2)
library(sentimentr)
library(scales)
library(RCurl)
library(syuzhet)

#How to import the text
filenames=list.files(getwd(),pattern="*.txt")
filenames
class(filenames)
my_data=read.delim(file.choose())
#Read the data
#files=lapply("text_analytics.txt",readLines)
#files
class(my_data)
str(my_data)

#Create corpus form vector  (#tm package is required)
library(tm)
articles.corpus=Corpus(VectorSource(my_data))     
class(articles.corpus)

#Text Preprocessing (tm_map function does data cleaning)
#Make each letter lowercase
articles.corpus=tm_map(articles.corpus,tolower)
#Remove punctuation
articles.corpus=tm_map(articles.corpus,removePunctuation)
#Remove numbers
articles.corpus=tm_map(articles.corpus,removeNumbers)
#Remove generic and custom stopwords
stopwords()  #To check the number of stopwords

articles.corpus=tm_map(articles.corpus,removeWords,stopwords())   #To remove the stopwords

articles.corpus=tm_map(articles.corpus,removeWords,c("and"))  

#Visualization- Word Cloud

library(wordcloud)
wordcloud(articles.corpus,randomorder=F)   #random order=F means more significant words appear in the centre and bigger


###################
#Create TDM (Term document matrix)
#A term document matrix is a way of represnting the words in the text
tdm=TermDocumentMatrix((articles.corpus))
class(tdm)
tdmfreq=as.matrix(tdm)
class(tdmfreq)
termFreq=rowSums(as.matrix(tdm))
termFreq

#Subsetting TDM
termFreqsubset=subset(termFreq,termFreq>=3)
class(termFreqsubset)
termFreqsubset

#Creating a dataframe
tdmf=data.frame(term=names(termFreqsubset),freq=termFreqsubset)
View(tdmf)
row.names(tdmf)=NULL    #Remove the rownames
library(ggplot2)
#Create a barplot
tdmplot=ggplot(tdmf,aes(x=term,y=freq))+
  geom_bar(stat="identity")+xlab("Terms")+ylab("Count")+
  coord_flip()+
  theme(axis.text=element_text(size=6))
tdmplot

#Sentiment Analysis

library(sentimentr)
class(articles.corpus)
a=as.character(articles.corpus)
class(a)

#NRC is one of most popular dictionary for sentiment analysis
mysentiment=get_nrc_sentiment(a)
mysentiment
#Calculate the NRC scores
SentimentScores=data.frame(colSums(mysentiment[,]))
SentimentScores

#Give a name to data (since the variable names are not there)
#Give name to the scores column
names(SentimentScores)="Score"

#Give Row names
SentimentScores=cbind("sentiment"=rownames(SentimentScores),SentimentScores)
SentimentScores

#Removing Row names
rownames(SentimentScores)=NULL
SentimentScores

#Plotting the sentiment scores
ggplot(SentimentScores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat="identity")+
  xlab("Sentiment")+ylab("Score")+
  ggtitle("Total Sentiment Score")

##Topic Modeling
#latent dirichlet allocation (LDA) models are a widely used topic model
#Createt DTM
library(topicmodels)
articleDtm=DocumentTermMatrix(articles.corpus,
                              control=list(minwordLength=3))
k=4   #If we need 4 topics to list out
SEED=1234
article.lda=LDA(articleDtm,k,method="Gibbs",control=list(seed=SEED))

lda.topics=as.matrix(topics(article.lda))

lda.topics
lda.terms=terms(article.lda)
lda.terms
