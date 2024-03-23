#Anna Feldman INFO 640
#Final R Project
#Quantitative text analysis in R - structured data, topic modeling, and sentiment analysis

library(pdftools)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(tidytext) 
library(tm) 
library(stringr)
library(topicmodels) 
library(tidyverse)
library(textdata) #you may need to install this for the get_sentiments function later
library(data.table) #call before dplyr
library(dplyr) 
library(reshape2) 
library("tidyverse")
library("sys")
library(xml2)
library(rvest)
library(tibble)
library(stringr)

#--------Topic modeling unstructured text with five enacted state laws----# 
#----CA, CO, UT, CT, and VA ----#

#create topic model using function
print("ok")
get_LDA_topics_terms_by_topic <- function(input_corpus, plot = TRUE, number_of_topics = 6, number_of_words = 5, 
                                          path="/Users/annafeldman/Desktop/new_topics_")
{ 
  my_dtm <- DocumentTermMatrix(input_corpus)
  unique_indexes <- unique(my_dtm$i) 
  my_dtm <- my_dtm[unique_indexes,]
  my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234)) 
  my_topics <- tidy(my_lda, matrix="beta")
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics <- as.matrix(my_lda_words) 
  write.csv(my_lda_topics,file=paste(path,number_of_topics,".csv"))
  my_top_terms <- my_topics %>% group_by(topic) %>% top_n(number_of_words, beta) %>% ungroup() %>% arrange(topic, -beta)
  if(plot==TRUE){ my_top_terms %>%
      mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta, fill=factor(topic))) + geom_col(show.legend=FALSE) +
      facet_wrap(~ topic, scales = "free")+ coord_flip()
  }
  else
  {
    return(my_top_terms)
  }
}

#--------California CCPA----#

#reading in CA laws from PDF using pdftools R package
ccpa_law <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/CCPA full text .pdf")
ccpa_law

#create corpus      
ccpa_source <- VectorSource(ccpa_law) 
ccpa_corpus <- VCorpus(ccpa_source)
print(ccpa_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
ccpa_cleaned <- tm_map(ccpa_corpus, removeNumbers) #remove numbers
ccpa_cleaned <- tm_map(ccpa_cleaned, content_transformer(tolower)) #make everything lowercase
ccpa_stops = c(stopwords("en"), "provision", "chapter","shall","means", "§", 
               "pursuant", "purposes", "general", "may", "data", "subsection", "sections",
               "section", "act", "sec", "title", "code", "including","proposition", "consumer’s",
               "categories","december", "consumer", "subdivision", "use") 
#did not remove information on purpose re: defining data
ccpa_cleaned = tm_map(ccpa_cleaned, removeWords, ccpa_stops) #remove stop words
ccpa_cleaned <- tm_map(ccpa_cleaned, removePunctuation) #remove punctuation
ccpa_cleaned <- tm_map(ccpa_cleaned, stripWhitespace) #remove white space

#get topics for CCPA
get_LDA_topics_terms_by_topic(ccpa_cleaned, number_of_topics = 4, number_of_words = 7)


#--------California CPRA Proposition/Ballot Measure----#

ca_prop <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/CPRA full text –  Prop.pdf")
ca_prop

#create corpus      
prop_source <- VectorSource(ca_prop) 
prop_corpus <- VCorpus(prop_source)
print(prop_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
prop_cleaned <- tm_map(prop_corpus, removeNumbers) #remove numbers
prop_cleaned <- tm_map(prop_cleaned, content_transformer(tolower)) #make everything lowercase
prop_stops = c(stopwords("en"), "provision", "chapter","shall","means", "§", "pursuant", "purposes", 
               "general", "may", "data", "subsection", "sections","section", "act", "title", 
               "subdivision", "consumer’s","use") #add more stop words
#did not remove information on purpose re: defining data
prop_cleaned = tm_map(prop_cleaned, removeWords, prop_stops) #remove stop words
prop_cleaned <- tm_map(prop_cleaned, removePunctuation) #remove punctuation
prop_cleaned <- tm_map(prop_cleaned, stripWhitespace) #remove white space

get_LDA_topics_terms_by_topic(prop_cleaned, number_of_topics = 4, number_of_words = 7) 


#--------Connecticut----#
#reading in CT law from PDF using pdftools R package
ct_law <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/Connecticut-2022-SB 6-Chaptered.pdf")
#ct_law

#create corpus      
ct_source <- VectorSource(ct_law) 
ct_corpus <- VCorpus(ct_source)
#print(ct_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
ct_cleaned <- tm_map(ct_corpus, removeNumbers) #remove numbers
ct_cleaned <- tm_map(ct_cleaned, content_transformer(tolower)) #make everything lowercase
ct_stops = c(stopwords("en"), "regulation", "provision", "chapter","shall","means", "§", "pursuant", "purposes", 
             "general", "may", "data", "subsection", "sections","section", "act", "usc", "seq","include","inclusive") #add more stop words
#did not remove information on purpose re: defining data
ct_cleaned = tm_map(ct_cleaned, removeWords, ct_stops) #remove stop words
ct_cleaned <- tm_map(ct_cleaned, removePunctuation) #remove punctuation
ct_cleaned <- tm_map(ct_cleaned, stripWhitespace) #remove white space

#get topics for CT
get_LDA_topics_terms_by_topic(ct_cleaned, number_of_topics = 4, number_of_words = 7)


#---------Virginia
#reading in VA law from csv
va_law <- read.csv("/Users/annafeldman/Desktop/FA22/Data Analysis 640/VA Privacy Law.csv")
#va_law

#create corpus      
va_source <- VectorSource(va_law) 
va_corpus <- VCorpus(va_source)
#print(va_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
va_cleaned <- tm_map(va_corpus, removeNumbers) #remove numbers
va_cleaned <- tm_map(va_cleaned, content_transformer(tolower)) #make everything lowercase
va_stops = c(stopwords("en"), "regulation", "provision", "chapter","shall","means", "§", "pursuant", "purposes", 
             "general", "may", "data", "subsection") #add more stop words
va_cleaned = tm_map(va_cleaned, removeWords, va_stops) #remove stop words
va_cleaned <- tm_map(va_cleaned, removePunctuation) #remove punctuation
va_cleaned <- tm_map(va_cleaned, stripWhitespace) #remove white space

#get topics - completed
get_LDA_topics_terms_by_topic(va_cleaned, number_of_topics = 4, number_of_words = 7)


#-----Colorado 
#completed but want to maybe add cfr back to stops
co_law <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/CO Privacy 2021a_190_signed.pdf")
#co_law

#create corpus      
co_source <- VectorSource(co_law) 
co_corpus <- VCorpus(co_source)
#print(co_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
co_cleaned <- tm_map(co_corpus, removeNumbers) #remove numbers
co_cleaned <- tm_map(co_cleaned, content_transformer(tolower)) #make everything lowercase
co_stops = c(stopwords("en"), "regulation", "provision", "chapter","shall","means", "§", 
             "pursuant", "purposes", "general", "may", "data", "subsection", "section", 
             "sec","part","defined", "cfr", "usc", "act", "article", "consumers") #add more stop words
co_cleaned = tm_map(co_cleaned, removeWords, co_stops) #remove stop words
co_cleaned <- tm_map(co_cleaned, removePunctuation) #remove punctuation
co_cleaned <- tm_map(co_cleaned, stripWhitespace) #remove white space

#topics
get_LDA_topics_terms_by_topic(co_cleaned, number_of_topics = 4, number_of_words = 7)


#--------Utah
#reading in UT law from PDF using pdftools R package - completed
ut_law <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/Utah Privacy SB0227S02.pdf")
#ut_law

#create corpus      
ut_source <- VectorSource(ut_law) 
ut_corpus <- VCorpus(ut_source)
#print(ut_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
ut_cleaned <- tm_map(ut_corpus, removeNumbers) #remove numbers
ut_cleaned <- tm_map(ut_cleaned, content_transformer(tolower)) #make everything lowercase
ut_stops = c(stopwords("en"), "regulation", "provision", "chapter", "shall", "means", "pursuant", "purposes", 
             "general", "may", "data", "subsection", "section", "sec", "seq", "code", 
             "annotated", "usc", "act", "defined", "consumers", "cfr") #add more stop words, let CFR back in because interesting!
ut_cleaned = tm_map(ut_cleaned, removeWords, ut_stops) #remove stop words
ut_cleaned <- tm_map(ut_cleaned, removePunctuation) #remove punctuation
ut_cleaned <- tm_map(ut_cleaned, stripWhitespace) #remove white space

#topics
get_LDA_topics_terms_by_topic(ut_cleaned, number_of_topics = 4, number_of_words = 7)

#--------Federal kids privacy laws-----#
#-----COPPA
coppa <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/COPPA-pdf.pdf")
coppa

#create corpus      
coppa_source <- VectorSource(coppa) 
coppa_corpus <- VCorpus(coppa_source)
print(coppa_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
coppa_cleaned <- tm_map(coppa_corpus, removeNumbers) #remove numbers
coppa_cleaned <- tm_map(coppa_cleaned, content_transformer(tolower)) #make everything lowercase
coppa_stops = c(stopwords("en"), "regulation", "provision", "§", "operator", "operators", "means", "childs", "child",
                "shall", "online", "web", "site") #add more stop words
coppa_cleaned = tm_map(coppa_cleaned, removeWords, coppa_stops) #remove stop words
coppa_cleaned <- tm_map(coppa_cleaned, removePunctuation) #remove punctuation
coppa_cleaned <- tm_map(coppa_cleaned, stripWhitespace) #remove white space

get_LDA_topics_terms_by_topic(coppa_cleaned)
#get_LDA_topics_terms_by_topic(coppa_cleaned, number_of_topics = 7, number_of_words = 7)

#------Amendment proposed to COPPA
amend <- read.csv("/Users/annafeldman/Desktop/FA22/Data Analysis 640/COPPA Amend 1.csv")
amend

#Create corpus      
amend_source <- VectorSource(amend) 
amend_corpus <- VCorpus(amend_source)
print(amend_corpus) 

#Clean and prepare the text
amend_cleaned <- tm_map(amend_corpus, removeNumbers)
amend_cleaned <- tm_map(amend_cleaned, content_transformer(tolower))
amend_stops = c(stopwords("en"), "sec", "term", "operator", "general", "paragraph", 
                "act", "online", "section", "shall") 
amend_cleaned = tm_map(amend_cleaned, removeWords, amend_stops) 
amend_cleaned <- tm_map(amend_cleaned, removePunctuation) 
amend_cleaned <- tm_map(amend_cleaned, stripWhitespace) 

get_LDA_topics_terms_by_topic(amend_cleaned)

#-----FERPA Family Educational Rights and Privacy Act Regulations----#
ferpa <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/FERPA full text.pdf")
ferpa

#create corpus      
ferpa_source <- VectorSource(ferpa) 
ferpa_corpus <- VCorpus(ferpa_source)

#Clean and prepare the text for topic modeling: 
ferpa_cleaned <- tm_map(ferpa_corpus, removeNumbers) #remove numbers
ferpa_cleaned <- tm_map(ferpa_cleaned, content_transformer(tolower)) #make everything lowercase
ferpa_stops = c(stopwords("en"), "regulation", "provision", "chapter", "shall", "section", 
                "paragraph", "office", "cfr", "part", "means", "another", "will") #add more stop words
ferpa_cleaned = tm_map(ferpa_cleaned, removeWords, ferpa_stops) #remove stop words
ferpa_cleaned <- tm_map(ferpa_cleaned, removePunctuation) #remove punctuation
ferpa_cleaned <- tm_map(ferpa_cleaned, stripWhitespace) #remove white space

#topics
get_LDA_topics_terms_by_topic(ferpa_cleaned)

#-----GDPR-----#

gdpr <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/GDPR Full Text.pdf")

#create corpus      
gdpr_source <- VectorSource(gdpr) 
gdpr_corpus <- VCorpus(gdpr_source)
print(gdpr_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
gdpr_cleaned <- tm_map(gdpr_corpus, removeNumbers) #remove numbers
gdpr_cleaned <- tm_map(gdpr_cleaned, content_transformer(tolower)) #make everything lowercase
gdpr_stops = c(stopwords("en"), "regulation", "provision", "chapter","shall","means", "§", 
             "pursuant", "purposes", "general", "may", "data", "subsection", "section", 
             "sec","part","defined", "subject", "article", "european", "union") #add more stop words
gdpr_cleaned = tm_map(gdpr_cleaned, removeWords, gdpr_stops) #remove stop words
gdpr_cleaned <- tm_map(gdpr_cleaned, removePunctuation) #remove punctuation
gdpr_cleaned <- tm_map(gdpr_cleaned, stripWhitespace) #remove white space

#topics
get_LDA_topics_terms_by_topic(gdpr_cleaned, number_of_topics = 4, number_of_words = 7)

#-----APPDA Proposed-----#

adppa <- pdf_text("/Users/annafeldman/Desktop/State Law PDFs/ADPPA.pdf")

#create corpus      
adppa_source <- VectorSource(adppa) 
adppa_corpus <- VCorpus(adppa_source)
print(adppa_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
adppa_cleaned <- tm_map(adppa_corpus, removeNumbers) #remove numbers
adppa_cleaned <- tm_map(adppa_cleaned, content_transformer(tolower)) #make everything lowercase
adppa_stops = c(stopwords("en"), "regulation", "provision", "chapter","shall","means", 
             "pursuant", "purposes", "general", "may", "subsection", "section", "fmt", "sfmt",
             "sec","part","defined", "cfr", "usc", "act", "article", "jul", "july", "nov",  
             "covered", "verdate", "paragraph", "data", "gvggxml", "gcmteeccpftc", "hansfcxml", 
             "large", "entity", "jkt", "usc", "title") #add more stop words
adppa_cleaned = tm_map(adppa_cleaned, removeWords, adppa_stops) #remove stop words
adppa_cleaned <- tm_map(adppa_cleaned, removePunctuation) #remove punctuation
adppa_cleaned <- tm_map(adppa_cleaned, stripWhitespace) #remove white space

#topics
get_LDA_topics_terms_by_topic(adppa_cleaned, number_of_topics = 4, number_of_words = 7)


#-----topic model and sentiment analysis of media coverage-----#

#reading in BBC articles
bbc <- read.csv("/Users/annafeldman/Desktop/FA22/Data Analysis 640/Media Coverage Data Privacy.csv")
#bbc

#create corpus      
bbc_source <- VectorSource(bbc) 
bbc_corpus <- VCorpus(bbc_source)
print(bbc_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
bbc_cleaned <- tm_map(bbc_corpus, removeNumbers) #remove numbers
bbc_cleaned <- tm_map(bbc_cleaned, content_transformer(tolower)) #make everything lowercase
bbc_stops = c(stopwords("en"), "data", "said", "privacy", "one", "will", "told", "says",
              "also","can","now","many","make","used","use", "much", "caption", "added") #add more stop words
#did not remove information on purpose re: defining data
bbc_cleaned = tm_map(bbc_cleaned, removeWords, bbc_stops) #remove stop words
bbc_cleaned <- tm_map(bbc_cleaned, removePunctuation) #remove punctuation
bbc_cleaned <- tm_map(bbc_cleaned, stripWhitespace) #remove white space

#get topics - completed
get_LDA_topics_terms_by_topic(bbc_cleaned)
get_LDA_topics_terms_by_topic(bbc_cleaned, number_of_topics = 4, number_of_words = 7)


#-----sentiment analysis-----#

sentiments
get_sentiments("afinn") #gives you a score between -3 and +3
get_sentiments("nrc") #gives you qualitative labels (fear, trust, etc.)
get_sentiments("bing") #gives you positive/negative

#download articles
bbc_sentiment <- read.csv("/Users/annafeldman/Desktop/FA22/Data Analysis 640/Media Coverage Data Privacy.csv")
summary(bbc_sentiment)

#tidy up - tidy_bbc is a long list of every word in the csv
tidy_bbc <- bbc_sentiment %>% ungroup() %>% unnest_tokens(word, headline)
summary(tidy_bbc) 
head(tidy_bbc)

#use nrc sentiment data set to assess the different sentiments 
#that are represented across the articles -- shows a stronger negative presence than positive.
tidy_bbc %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE) #success

bbc_sentiment <- tidy_bbc %>% inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment) %>% spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
head(bbc_sentiment) #success

#calculate a sentiment score by getting average from positive-negative
#Overall score of how positive or negative 
bbc_total_sentiment <- mean(bbc_sentiment$sentiment) 
bbc_total_sentiment

#filter the nrc sentiment dictionary for fear terms
#hypothesize that this will show some themes reflecting legislative efforts
nrc_sent <- get_sentiments("nrc") %>% filter(sentiment == "fear")

#Perform a join on the “word” column for both the sentiment and the tidy articles 
#Then count the number of times each word appears & sort the list
tidy_bbc %>% inner_join(nrc_sent) %>% dplyr::count(word, sort = TRUE)

#visualize articles and sentiment
ggplot(bbc_sentiment, aes(negative, positive, color = word)) + geom_jitter(show.legend = FALSE)


#---------
#Web scraping just the article titles from BBC World News articles on data privacy (search = data privacy)
#Sentiment analysis, LDA Topics
#Storing url of first page of the table with data from bbc 
bbc_page <- "https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl&page=%s"

#creating empty dataframe
bbc_titles <- data.frame()
#iterator
i<-1
#it loops through 291 times so as to extract and then store and then combine about 290 articles 
while (i<30) {
  new_webpage <- read_html(sprintf(bbc_page,i)) %>% #read each page of articles after first pg
    html_nodes(".ssrcss-1ynlzyd-PromoLink") %>% #Get the css tag for the listed articles links
    html_text() #Extract the article text from the css tag
  bbc_new_webpage <-data.frame(new_webpage) #create dataframe
  bbc_titles <- rbind(bbc_titles, bbc_new_webpage) #add to dataframe
  i=i+1
} #success!

#LDA

#create corpus      
titles_source <- VectorSource(bbc_titles) 
titles_corpus <- VCorpus(titles_source)
print(titles_corpus) #about the corpus

#Clean and prepare the text for topic modeling: 
bbc_titles_cleaned <- tm_map(titles_corpus, removeNumbers) #remove numbers
bbc_titles_cleaned <- tm_map(bbc_titles_cleaned, content_transformer(tolower)) #make everything lowercase
titles_stops = c(stopwords("en"), "data", "said", "privacy", "one", "will", "told", "says",
              "also","can","now","many","make","used","use", "much", "caption", "added", "bbc") #add more stop words
#did not remove information on purpose re: defining data
bbc_titles_cleaned = tm_map(bbc_titles_cleaned, removeWords, titles_stops) #remove stop words
bbc_titles_cleaned <- tm_map(bbc_titles_cleaned, removePunctuation) #remove punctuation
bbc_titles_cleaned<- tm_map(bbc_titles_cleaned, stripWhitespace) #remove white space

#get topics - completed
get_LDA_topics_terms_by_topic(bbc_titles_cleaned)
get_LDA_topics_terms_by_topic(bbc_titles_cleaned, number_of_topics = 4, number_of_words = 7)


#SENTIMENT
#tidy up - tidy_bbc is a long list of every word in the csv
tidy_bbc_titles <- bbc_titles %>% ungroup() %>% unnest_tokens(word, new_webpage)
summary(tidy_bbc_titles ) 
head(tidy_bbc_titles )

#use nrc sentiment data set to assess the different sentiments 
#that are represented across the articles -- shows a stronger negative presence than positive.
tidy_bbc_titles %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE) #success

bbc_titles <- tidy_bbc_titles %>% inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment) %>% spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
head(bbc_titles) #success

#calculate a sentiment score by getting average from positive-negative
#Overall score of how positive or negative 
bbc_titles_sentiment <- mean(bbc_titles$sentiment) 
bbc_titles_sentiment

nrc_sent <- get_sentiments("nrc") %>% filter(sentiment == "fear")

#Perform a join on the “word” column for both the sentiment and the tidy articles 
#Then count the number of times each word appears & sort the list
tidy_bbc_titles %>% inner_join(nrc_sent) %>% dplyr::count(word, sort = TRUE)



#-----not done-------#

#Web scraping just the article titles from CNN articles on data privacy (search = data privacy)
#Storing url of first page of the table with data from bbc - success
cnn_page <- "https://www.cnn.com/search?q=data+privacy&from=0&size=10&page=%s"
#separate and concatenate

#creating empty dataframe
cnn_titles <- data.frame()
#iterator
i<-1
#it loops through 50 times so as to extract and then store and then combine about  articles 
while (i<5) {
  cnn_loop <- sprintf(cnn_page,i)
  cnn_loop <- paste(cnn_loop, "&sort=newest&types=all&section=")
  cnn_loop <- gsub(" ", "", cnn_loop)
  #print(cnn_loop)
  new_webpage <- read_html(cnn_loop) %>% #read each page of articles after first pg
    html_nodes("headline") %>% #Get the css tag for the listed articles links
    html_text() #Extract the article text from the css tag
  cnn_new_webpage <-data.frame(new_webpage) #create dataframe
 cnn_titles <- rbind(cnn_titles, cnn_new_webpage) #add to dataframe
  i=i+1
} 
#------------#

# probabilistic coherence, a measure of topic quality
# this measure can be used with any topic model, not just probabilistic ones
summary(ccpa_corpus$coherence)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.04014 0.13035 0.19317 0.21366 0.24673 0.57733
ccpa_cleaned$top_terms <- GetTopTerms(phi = ccpa_cleaned$phi, M = 5)
hist(ccpa_cleaned$coherence, 
     col= "blue", 
     main = "Histogram of probabilistic coherence")
