#------------------------*****  *****------------------------#

# In the analysis we will be performing following seteps:
#   Data Wrangling
#   Exploratory Analysis (Univariate , Bivariate)
#   Sentiment Analysis
#   Topic Modelling
#     Perform LDA
#     Create Word cloud
#   Exploratory Analysis (Univariate , Bivariate)
  
#------------------------***** Read Data *****------------------------#

# read data
data<-read.csv('Twitter_Data.csv',header = T)
str(data)

# 'data.frame':	46395 obs. of  16 variables:
#   $ State          : Factor w/ 5 levels "IL","IN","MI",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Mentions....   : Factor w/ 673 levels "","[10]","[12]",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Mention.Count  : logi  NA NA NA NA NA NA ...
# $ Favorites      : Factor w/ 160 levels "","0","1","1.3K",..: 1 1 1 3 1 1 1 1 1 1 ...
# $ Likes          : int  NA NA NA 1 NA 1 1 3 NA NA ...
# $ Retweet.Count  : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Tag            : Factor w/ 2449 levels "","[1BillionSteps]",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Tag.Count      : logi  NA NA NA NA NA NA ...
# $ Date           : Factor w/ 18123 levels "1/1/13","1/1/14",..: 11646 11507 12831 12455 10529 10327 10319 10267 10288 10113 ...
# $ Time           : logi  NA NA NA NA NA NA ...
# $ Time.of.Day    : logi  NA NA NA NA NA NA ...
# $ Weekday.Weekend: logi  NA NA NA NA NA NA ...
# $ Tweet          : Factor w/ 44140 levels "   .... 4D s   Drain, Dress, DEET, Dusk/Dawn and the 4C s   Cover, Clean, Contain, & Consult Dr. - Please get your Flu Vaccine",..: 8765 37303 13840 37304 37315 37314 4944 18423 12304 21839 ...
# $ Tweet.Type     : logi  NA NA NA NA NA NA ...
# $ Username       : Factor w/ 959 levels "@_NowPow_","@21stShow",..: 467 467 467 467 467 467 467 467 467 467 ...
# $ Has_media      : int  NA NA NA NA NA NA NA NA NA NA ...

#------------------------***** Data Wrangling *****------------------------#

# We need to check for missing values.
apply(data,2,function(x) sum(is.na(x)))

# replacing blank spaces by 0
data$Favorites[data$Favorites=='']<-0

# replacing NA  by 0
data$Likes[is.na(data$Likes)]<-0
data$Retweet.Count[is.na(data$Retweet.Count)]<-0

# function to count words in a string based on 
countWords <- function( s) {
  s2 <- gsub(',',"",s)
  return ((nchar(s) - nchar(s2))+1)
}

# Updating mention count
data$Mentions....<-as.character(data$Mentions....)
data$Mention.Count<- ifelse(data$Mentions....=='',0 ,countWords(data$Mentions....))
table(data$Mention.Count)

# handling tag count column
data$Tag<-as.character(data$Tag)
data$Tag.Count<- ifelse(data$Tag=='',0 ,countWords(data$Tag))
table(data$Tag.Count)

# splitting date and time into seperate columns
data$Time <- ifelse(data$State=='IL',(format(as.POSIXct(strptime(data$Date[data$State=='IL'],
                                                                 "%m/%d/%y %H:%M",tz="")) ,format = "%H.%M")), 00.00)

data$Date <- ifelse(data$State=='IL',(format(as.POSIXct(strptime(data$Date[data$State=='IL'],
                                                                 "%m/%d/%y %H:%M",tz="")) ,format = "%m/%d/%Y")),
                    format(as.POSIXct(strptime(data$Date[data$State!='IL'],"%m/%d/%y",tz="")) ,format = "%m/%d/%Y"))

# Adding new column time of the day
data$Time<-as.numeric(data$Time)
data$Time.of.Day <-ifelse(data$Time>00.01 & data$Time <=04.59 , 'LateNight',
                          ifelse(data$Time >04.59 & data$Time <=11.59, 'Morning',
                                 ifelse(data$Time>11.59 & data$Time <=16.00,'Afternoon', 
                                        ifelse(data$Time>16.00 & data$Time <=18.59 , 'Evening',
                                               ifelse(data$Time>18.59 & data$Time <=23.59 , 'Night' ,'Unknown')))))


table(data$Time.of.Day)

data$Time<-as.character(data$Time)
data$Time<-ifelse(data$Time != 0, gsub('[.]',':',data$Time), 0:00)

# Export data  
write.csv(data,file='data.csv')

#------------------------***** Sentiment Analysis *****------------------------#

# load in the libraries we'll need
library(tidyverse)
install.packages('tokenizers')
install.packages('tidytext')
library(tidytext)
library(glue)
library(stringr)
library('ROAuth')
library('RCurl')
library(twitteR)
library(purrr)
library(dplyr)
library(plyr)


# sentiment score function
score.sentiment<- function(sentences,pos.words,neg.words){
  require(plyr)
  require(stringr)
  scores<-laply(sentences,function(sentence,pos.words,neg.words){
    sentence<-gsub('[[:punct:]]',"",sentence)
    sentence<-gsub('[[:cntrl:]]',"",sentence)
    sentence<-gsub('\\d+',"",sentence)
    
    word.list<-str_split(sentence,'\\s+')
    words<-unlist(word.list)
    pos.matches<-match(words,pos.words)
    neg.matches<-match(words,neg.words)
    pos.matches<-!is.na(pos.matches)
    neg.matches<-!is.na(neg.matches)
    score<-sum(pos.matches)-sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress = .progress)
  scores.df<-data.frame(score=scores,text=sentences)
  return(scores.df)
}

# reading positive and negative words
pos.words<-scan('positive.txt', what = 'character', comment.char = '')
neg.words<-scan('negative.txt', what = 'character', comment.char = '')

test.score<-score.sentiment(data$Tweet,pos.words,neg.words)

# adding score column to the data
data2<-cbind(dat,score=test.score$score)
write.csv(data2,file='senti.csv')

#------------------------***** Topic Modeling *****------------------------#

data.tm<-data
colnames(data.tm) # column names of dataset
text.data<-data.tm[27000:36000,17] # selecting few row and the tweet column

install.packages('topicmodels')
library(topicmodels)
library(tm)

# reading stop words from personal list
stopList <- read.csv( 'stopWord.csv',header = T ) 

# converting text into vector
Corpus <-VCorpus(VectorSource(text.data))
Corpus <-tm_map(Corpus,content_transformer(tolower))
Corpus <-tm_map(Corpus,removeNumbers)
Corpus <-tm_map(Corpus,removePunctuation)
Corpus <-tm_map(Corpus,removeWords,stopList$List)
#Corpus <-tm_map(Corpus,removeWords,stopwords())
Corpus <-tm_map(Corpus,stemDocument)
Corpus <-tm_map(Corpus,stripWhitespace)
dtm<- DocumentTermMatrix(Corpus)
dtm<-removeSparseTerms(dtm,0.99) # omitting less occuring word
ds<-as.data.frame(as.matrix(dtm))

frequent_terms <- freq_terms(ds, 30)
plot(frequent_terms)

tt<-ds[!(rowSums(ds)==0),]
tt
#collapse matrix by summing over columns
freq <- colSums(ds)
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
fr<-freq[ord]
fr

library(topicmodels)
ap_lda <- LDA(tt, k = 10, control = list(seed = 1234))
ap_lda

install.packages('tidytext')
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Enhance
install.packages('wordcloud')
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(321)
#limit words by specifying min frequency
wordcloud(names(fr),fr, min.freq=95,colors=brewer.pal(5,"Dark2"))

#------------------------***** Exploratory Analysis *****------------------------#

library(ggplot2)
library(dplyr)
library(readxl)

g.data<-data

###-------

#----- variable tweet type
table(g.data$type)


colnames(g.data)[colnames(g.data)=='type']<-'Tweet.Category'

# univariate
ggplot(data=subset(g.data,!is.na(Tweet.Category)), aes(x = Tweet.Category)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge',fill="steelblue")+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Distribution of Tweet Category')

ggsave(filename =  'ttu.png',device = 'png')

# bivariate
ggplot(data=subset(g.data,!is.na(Tweet.Category)), aes(x = Tweet.Category, fill = State)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Distribution of Tweet Category by State')

ggsave(filename =  'ttb1.png',device = 'png')

ggplot(data=subset(g.data,!is.na(Tweet.Category)), aes(x = Tweet.Category,fill=Tweet.Category)) +  
  geom_bar(aes(y = 100*(..count..)/sum(..count..)))+facet_wrap(~State)+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Distribution of Tweet Category by State')

ggsave(filename =  'ttb2.png',scale = 2.5)

#----variable mention

table(g.data$Mention.Count)
sum(is.na(g.data$Mention.Count))
men.g.data<-g.data[!is.na(g.data$Mention.Count),]  # Only IL  have Mentions

# univariate
ggplot(data=subset(g.data,!is.na(Mention.Count)), aes(x = Mention.Count)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge',fill="steelblue")+
  ylab('Count % age ')+xlab('Mention Count')+ggtitle('Mention Count distribution')

ggsave(filename =  'ttb8.png',scale = 2)


# bivariate

ggplot(data=subset(g.data,!is.na(State)), aes(x = Mention.Count, fill = State)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('Mention.Count')+ggtitle('Mention Count by Tweet Category for IL')

ggsave(filename =  'ttb9.png',scale = 2)

ggplot(data=subset(g.data,!is.na(Tweet.Category)), aes(x = Mention.Count, fill = Tweet.Category)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('Mention.Count')+ggtitle('Mention Count by Tweet Category for IL')

ggsave(filename =  'ttb10.png',scale = 2)


#---- variable Favourite

table(g.data$Favorites)
sum(is.na(g.data$Favorites))

# create favourite category
g.data<-mutate(g.data,favCategory=ifelse(Favorites==1, '1',
                                         ifelse(Favorites>1 & Favorites<=20, '2 to 20',
                                                ifelse(Favorites>20,'Greater than 20' ,'0'))))
table(g.data$favCategory)
fv.data<-g.data[g.data$favCategory!='0',]
table(fv.data$favCategory)

# univariate
ggplot(data=subset(fv.data,!is.na(Favorites)), aes(x = favCategory)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge',fill="steelblue")+
  ylab('Count % age ')+xlab('Favorites Count')+ggtitle('Favorites distribution')

ggsave(filename =  'ttb11.png',scale = 2)

# bivariate
ggplot(data=subset(fv.data,!is.na(Tweet.Category)), aes(x = Tweet.Category, fill = favCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+facet_wrap(~State)+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Tweet Category by favorites')

ggsave(filename =  'ttb12.png',scale = 2.5)

ggplot(data=subset(fv.data,!is.na(Tweet.Category)), aes(x = State, fill = favCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('State')+ggtitle('Distribution of favorites by State')

ggsave(filename =  'ttb13.png',scale = 2)

ggplot(data=subset(fv.data,!is.na(Tweet.Category)), aes(x = Time.of.Day, fill = favCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+facet_wrap(~State)+
  ylab('Count % age ')+xlab('Time.of.Day')+ggtitle('Distribution of favorites by Time of Day')

ggsave(filename =  'ttb14.png',scale = 2)

#---- variable Likes

table(g.data$Likes)
sum(is.na(g.data$Likes))

g.data<-mutate(g.data,LikesCategory=ifelse(Likes==1, '1',
                                           ifelse(Likes>1 & Likes<=20, '2 to 20',
                                                  ifelse(Likes>20,'Greater than 20' ,'0'))))
table(g.data$LikesCategory)
like.data<-g.data[g.data$LikesCategory!='0',]
table(like.data$LikesCategory)

# univariate
ggplot(data=subset(like.data,!is.na(LikesCategory)), aes(x =LikesCategory)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge',fill="steelblue")+
  ylab('Count % age ')+xlab('Likes Count')+ggtitle('Likes distribution')

ggsave(filename =  'ttb15.png',scale = 2)

# bivariate
ggplot(data=subset(like.data,!is.na(Tweet.Category)), aes(x = Tweet.Category, fill = LikesCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Distribution of Tweet Category by Likes')

ggsave(filename =  'ttb16.png',scale = 2)

#- only IL has likes
ggplot(data=subset(like.data,!is.na(Tweet.Category)), aes(x = State, fill = LikesCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('State')+ggtitle('Distribution of Likes by State')

ggsave(filename =  'ttb17.png',scale = 2)

ggplot(data=subset(like.data,!is.na(Tweet.Category)), aes(x = Time.of.Day, fill = LikesCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+facet_wrap(~State)+
  ylab('Count % age ')+xlab('Time.of.Day')+ggtitle('Distribution of Time of Day  by Likes')

ggsave(filename =  'ttb18.png',scale = 2)

#---- variable retweet

table(g.data$Retweet.Count)
sum(is.na(g.data$Retweet.Count))

g.data<-mutate(g.data,RetweetCategory=ifelse(Retweet.Count==1, '1',
                                             ifelse(Retweet.Count>1 & Retweet.Count<=20, '2 to 20',
                                                    ifelse(Retweet.Count>20,'Greater than 20' ,'0'))))
table(g.data$RetweetCategory)
rt.data<-g.data[g.data$RetweetCategory!='0',]
table(rt.data$RetweetCategory)

# univariate
ggplot(data=subset(rt.data,!is.na(RetweetCategory)), aes(x =RetweetCategory)) +  
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge',fill="steelblue")+
  ylab('Count % age ')+xlab('Retweet Count')+ggtitle('Retweet count distribution')

ggsave(filename =  'ttb23.png',scale = 2)

# bivariate
ggplot(data=subset(rt.data,!is.na(Tweet.Category)), aes(x = Tweet.Category, fill = RetweetCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+ facet_wrap(~State)+
  ylab('Count % age ')+xlab('Tweet Category')+ggtitle('Distribution of Tweet Category by Retweet count')

ggsave(filename =  'ttb24.png',scale = 2.5)

ggplot(data=subset(rt.data,!is.na(Tweet.Category)), aes(x = State, fill = RetweetCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+
  ylab('Count % age ')+xlab('State')+ggtitle('Distribution of Retweet count by State')

ggsave(filename =  'ttb25.png',scale = 2)

ggplot(data=subset(rt.data,!is.na(Tweet.Category)), aes(x = Time.of.Day, fill = RetweetCategory)) + 
  geom_bar(aes(y = 100*((..count..)/sum(..count..))),position = 'dodge')+facet_wrap(~State)+
  ylab('Count % age ')+xlab('Time.of.Day')+ggtitle('Distribution Time of Day by Retweet count')#+ scale_fill_grey()


ggsave(filename =  'ttb26.png',scale = 2)

#------------------------*******------------------------#
