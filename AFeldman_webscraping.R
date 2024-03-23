#Anna Feldman
#Web scraping from BBC World News articles on data privacy (search = data privacy)

library("tidyverse")
library("sys")
library(xml2)
library(rvest)
library(tibble)
library(stringr)

#Storing url of first page of the table with data from bbc - success
bbc_datapriv <- "https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl"
bbc_new_urls <- "https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl&page=%s"

#creating empty dataframe
df <- data.frame()
#iterator
i<-1
#it loops through 291 times so as to extract and then store and then combine about 290 articles 
while (i<30) {
  new_webpage <- read_html(sprintf(bbc_new_urls,i)) %>% #read each page of articles after first pg
    html_nodes(".ssrcss-1ynlzyd-PromoLink") %>% #Get the css tag for the listed articles links
    html_text() #Extract the article text from the css tag
  df_new_webpage <-data.frame(new_webpage) #create dataframe
  df <- rbind(df, df_new_webpage) #add to dataframe
  i=i+1
} #success!


#---other

#Scraping html content from the stored url - success
bbc_base_webpage <- read_html(bbc_datapriv)
#see that is does print out the entire source code of this page
bbc_base_webpage

#Read the central url page containing articles' links - success
bbc_pg_url <- read_html( "https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl") %>% 
  html_nodes(".ssrcss-1ynlzyd-PromoLink") %>% #Get the css tag for the listed articles titles
  html_attr("href")  #Extract the url of each page from the css tag
bbc_pg_url 
#this is a list - make it into a dataframe
#create data frame from 10 articles on first page - success
df_bbc_pg_url <-data.frame(bbc_pg_url)

#it loops through 291 times so as to extract and then store and then combine about 290 articles 
while (i<30) {
  new_webpage <- read_html(sprintf(bbc_new_urls,i)) %>% #read each page of articles after first pg
    html_nodes(".ssrcss-1ynlzyd-PromoLink") %>% #Get the css tag for the listed articles links
    html_attr("href") #Extract the url of each page from the css tag
  df_new_webpage <-data.frame(new_webpage) #create dataframe
  df <- rbind(df, df_new_webpage) #add to dataframe
  i=i+1
} #success!




#test -- look at ten articles titles on first page - success
bbc_pg_title <- read_html("https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl") %>% 
  html_nodes(".ssrcss-1ynlzyd-PromoLink") %>% 
  html_text()
bbc_pg_title
df_bbc_pg_title <-data.frame(bbc_pg_title)

#Creating dataframe of the next set/pages of articles:
#from Armanda link: store 29 pages worth of articles - concatenate page #s 
#for strings use "%s" for pages

#----did this manually instead----#

#look at articles content on first page - success
df_bbc_pg_url[1,] #test
bbc_pg_article <- read_html(df_bbc_pg_url[1,]) %>% 
  html_nodes(".ssrcss-pv1rh6-ArticleWrapper") %>% 
  html_text()
bbc_pg_article

#create new empty df
df_articles <- data.frame()
#iterator
a<-1
#read articles content on first page 
while (a<11) {
bbc_pg_article <- read_html(df_bbc_pg_url[a,]) %>% 
  html_nodes("#main-content") %>% 
  html_text()
df_new_articles <-data.frame(bbc_pg_article) #create dataframe
df_articles <- rbind(df_articles, df_new_articles) #add to existing dataframe
a=a+1
} #doesn't read in correctly


#merge url tables - fail
#rename(df, links = new_webpage)
df <- rbind(df, df_new_webpage)
df_bbc_pg_url <- rbind(df_bbc_pg_url, df) #fail


#repeat but for .ssrcss-1q0x1qg-Paragraph to get short content desc

#repeat everything but for different base url search page

#-------not used-----#

#from lab - Custom function to read in lyrics according to the tag!
#use this for the content - fail
read_articles <- function(url) {
  tibble(
    articles = 
      url %>%
      read_html() %>% 
      html_nodes(".ssrcss-pv1rh6-ArticleWrapper") %>%
      html_text()  
  )}

#Pull the content at the specified links - fail
bbc_pg_url[1:10] %>% #We select just nine urls already obtained as trial 
  set_names(1:10) %>%  #A column that indicates the postion of each url 
  map_dfr(read_articles, .id = ".ssrcss-pv1rh6-ArticleWrapper")  %>% 
 bind_cols(title = bbc_pg_title[1:10]) #Bind the title of the first ten articles


#other code, unused
#-------
#get all the pages into one huge list iteration -- then have another loop thru this list
#find html marker for body of the article, repeat through each page, instead of title, 
#look for the body tag - will dump into a list - then put it into a dataframe - then text analysis
#loop within a loop?
#make a loop for the however many 29 pages - for loop
#https://www.bbc.co.uk/search?q=data+privacy&d=news_gnl&page=
#concatanate the page# - loop in however many pages
#variable is always this up until the "=" , create a list variable of 1-29
#loop = go to this list, concat. on element by element to the url
