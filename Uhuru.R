---
title: "Uhuru Kenyatta State of the Union Addresses 2014-2019"
author: "Herman Wandabwa"
date: " 25th April 2019"
output: html_document
runtime: shiny
---
  
library(dplyr)
library(tm)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
theme_set(theme_minimal())
library(wordcloud2)
library(plotly)

## Close all connections/Lists/trigger garbage collection/set working directory

closeAllConnections()
rm(list=ls())


##garbage collection - manual trigger
gc()

#Set the working directory
setwd(" Put the  path to your data  folder")

length(dir()) ##Number of files in the folder

docs <- VCorpus(DirSource())

wordToRemove = c('the','mister','honourable','also','will','speaker')

docs

docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs,removeWords,wordToRemove)
docs <- tm_map(docs, stripWhitespace)

docs = tm_map(docs, PlainTextDocument)

dtm = DocumentTermMatrix(docs)

dim(dtm)
dtm <- removeSparseTerms(dtm, 0.75)
dim(dtm)
rownames(dtm) <- c("2014", "2015", "2016", "2017", "2018","2019")


inspect(dtm[1:6, 1:5])

#-----------
# Distribution of terms over time (Terms decay over time). 


#Word frequency and topic models

freq <- colSums(as.matrix(dtm))
ord <- order(-freq)
freq[head(ord)]
freq[tail(ord)]


#What these tables show is the number of words with that specific frequency. 
#So 524 whead(table(freq))
tail(table(freq))


#review_bigrams <- moma.df %>%
 # unnest_tokens(bigram, review_body, token = "ngrams", n = 2)

findFreqTerms(dtm, 100) # we can see which words occurred at least 100 timesords occurred two times; and one word ---- occured --- times




#Words associated with the top 100 words

findAssocs(dtm, "kenyans", corlimit = 0.85) #Words associated with "youth" with 85% as the correlation cutoff

#A few wordloud variants. The fancier the better

#Create Palette


#redPalette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
#redPalette


#plots
#wordcloud2(hmtTable, size=1.6, figPath = handmaiden, color=rep_len( redPalette, nrow(hmtTable) ) )
#wordcloud2(wf, size=1.6, figPath = kenya, color=rep_len( redPalette, nrow(wf) ) )
#wordcloud2(wf, size=1.6, figPath = kenya, color="#B20000")
#wf
#wordcloud2(wf, size=2.6, figPath = kenya)
DF <- tidy(dtm)
terms_freq <- termFreq(DF$term, control = list())

df_terms_freq  <- tidy(terms_freq)
#--------------------------------

freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf <- data.frame(word = names(freq), freq = freq)

wf1 <-data.frame(word = names(freq), freq = freq)

wf <- wf[1:6, ]
barplot(wf$freq, names = wf$word, main = "Word Frequency",
        xlab = "Words", ylab = "Counts", ylim = c(0, 250))

#wordcloud(names(freq), freq, min.freq = 30, scale = c(3, .5),  
#          colors = brewer.pal(6, "Dark2"))
wordcloud2(wf)

#------------
#Topic Modeling

# Determine optimal clusters using factorextra::fviz_nbclust() method
library("factoextra")


fviz_nbclust(wf, kmeans, method= "gap_stat")

library(topicmodels)

set.seed(123)
lda3 <- LDA(dtm, k = 3, method = "Gibbs")
topics(lda3)
terms(lda3, 20)

#A quick look into each of the speeches. we will need into turn the text into data frames, perform sentence splitting, and then combine 
#them to one data frame with a variable created that specifies the year of the speech. 

library(qdap)

#2014
speech_2014 <- paste(readLines("SOU2014.txt"), collapse=" ")
speech_2014 <- iconv(speech_2014, "latin1", "ASCII", "")
prep14 <- qprep(speech_2014)
prep14 <- replace_contraction(prep14)
prep14 <- rm_stopwords(prep14, Top100Words, separate = F)
prep14 <- strip(prep14, char.keep = c("?", "."))
#Split the speech into sentences
sent14 <- data.frame(speech = prep14)
sent14 <- sentSplit(sent14, "speech")
sent14$year <- "2014"

#2015
speech_2015 <- paste(readLines("SOU2015.txt"), collapse=" ")
speech_2015 <- iconv(speech_2015, "latin1", "ASCII", "")
prep15 <- qprep(speech_2015)
prep15 <- replace_contraction(prep15)
prep15 <- rm_stopwords(prep15, Top100Words, separate = F)
prep15 <- strip(prep15, char.keep = c("?", "."))
#Split the speech into sentences
sent15 <- data.frame(speech = prep15)
sent15 <- sentSplit(sent15, "speech")
sent15$year <- "2015"

#2016
speech_2016 <- paste(readLines("SOU2016.txt"), collapse=" ")
speech_2016 <- iconv(speech_2016, "latin1", "ASCII", "")
prep16 <- qprep(speech_2016)
prep16 <- replace_contraction(prep16)
prep16 <- rm_stopwords(prep16, Top100Words, separate = F)
prep16 <- strip(prep16, char.keep = c("?", "."))
#Split the speech into sentences
sent16 <- data.frame(speech = prep16)
sent16 <- sentSplit(sent16, "speech")
sent16$year <- "2016"

#2017
speech_2017 <- paste(readLines("SOU2017.txt"), collapse=" ")
speech_2017 <- iconv(speech_2017, "latin1", "ASCII", "")
prep17 <- qprep(speech_2017)
prep17 <- replace_contraction(prep17)
prep17 <- rm_stopwords(prep17, Top100Words, separate = F)
prep17 <- strip(prep17, char.keep = c("?", "."))
#Split the speech into sentences
sent17 <- data.frame(speech = prep17)
sent17 <- sentSplit(sent17, "speech")
sent17$year <- "2017"

#2018
speech_2018 <- paste(readLines("SOU2018.txt"), collapse=" ")
speech_2018 <- iconv(speech_2018, "latin1", "ASCII", "")
prep18 <- qprep(speech_2018)
prep18 <- replace_contraction(prep18)
prep18 <- rm_stopwords(prep18, Top100Words, separate = F)
prep18 <- strip(prep18, char.keep = c("?", "."))
#Split the speech into sentences
sent18 <- data.frame(speech = prep18)
sent18 <- sentSplit(sent18, "speech")
sent18$year <- "2018"

#2019
speech_2019 <- paste(readLines("SOU2019.txt"), collapse=" ")
speech_2019 <- iconv(speech_2019, "latin1", "ASCII", "")
speech_2019 <- gsub("(Honourable)", "", speech_2019)
prep19 <- qprep(speech_2019)
prep19 <- replace_contraction(prep19)
prep19 <- rm_stopwords(prep19, Top100Words, separate = F)
prep19 <- strip(prep19, char.keep = c("?", "."))
sent19 <- data.frame(speech = prep19)
sent19 <- sentSplit(sent19, "speech")
sent19$year <- "2019"


sentences <- data.frame(rbind(sent14, sent15, sent16, sent17, sent18,sent19)) # concatenate sentences

plot(freq_terms(sentences$speech)) #Plots frequency of words

wordMat <- wfm(sentences$speech, sentences$year) #Provides freq of words per speech per year 
head(wordMat[order(wordMat[, 1], wordMat[, 2],decreasing = TRUE),])

#Complete word statistics
ws <- word_stats(sentences$speech, sentences$year, rm.incomplete = T)
plot(ws, label = T, lab.digits = 2)



#Visualize in word networks:

speech_subject <- sentences %>% 
  unnest_tokens(word, speech) %>% 
  anti_join(stop_words)

my_stopwords <- data_frame(word = c(as.character(1:10)))

speech_subject <- speech_subject %>% 
  anti_join(my_stopwords)

title_word_pairs <- speech_subject %>% 
  pairwise_count(word, year, sort = TRUE, upper = FALSE)
set.seed(1234) #for reproducibility

title_word_pairs %>%
  filter(n > 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "blue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Word network in  the Speeches from 2014-2019')
  theme_void()



plot(freq_terms(sentences$speech))

wordMat <- wfm(sentences$speech, sentences$year) # word frequency matrix that provides the counts for each word by speech
head(wordMat[order(wordMat[, 1], wordMat[, 2],decreasing = TRUE),])

trans_cloud(sentences$speech, sentences$year, min.freq = 10) # View word clouds for the speeches

ws <- word_stats(sentences$speech, sentences$year, rm.incomplete = T) #statistics of the speeches
plot(ws, label = T, lab.digits = 2)


pol = polarity(sentences$speech, sentences$year) #polarity of the sentences.
#stan.mean.polarity value represents the standardized mean polarity, which is the average polarity divided by the standard deviation.
pol # 2019 was more positive i.e. 0.482

plot(pol) #2014 speech was more negative overall

pol.df <- pol$all
which.min(pol.df$polarity) #min polarity
which.max(pol.df$polarity)
pol.df$text.var[685] #most negative
pol.df$text.var[269] #most positive 

readability_speeches <- automated_readability_index(sentences$speech, sentences$year) #readability
readability_speeches$Readability #https://en.wikipedia.org/wiki/Automated_readability_index


#Measure diversity in the speeches
diversity_speeches <- diversity(sentences$speech, sentences$year)
diversity_speeches
plot(diversity_speeches)


#Dispersion of a word in the text

dispersion_plot(sentences$speech,
                rm.vars = sentences$year,
                c("security", "jobs", "economy","youth","women","corruption"),
                color = "black", bg.color = "white")
