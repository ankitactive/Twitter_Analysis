
rm(list = ls())

library(twitteR)

library(curl)

library(tm)

library(ggplot2)



#Read in the Sentiment Dictionary which we will use later.

dict <- read.csv("sentimentdictionary.csv",stringsAsFactors = F)



#The following keys are unique to each user, you can get yours by signing up for an app "https://apps.twitter.com/".

consumer_key <- "ZTCrD2k1ueKk3gAoZ4MbbAKUV"

consumer_secret <- "vPnqw3ay7Ppx9Vv4iet8U9BoVc23pkyCMnq4GpiCouEISH9lSx"

access_token <- "66080966-8LG9fKoXLZqsa0mlb22z3lKvGbSkx7SZnzUm50fbv"

access_secret <- "ybqYb8MN4mwGhmyQ4IlHNv7IBsOJezDP3kAouAFzYjWvm"



#Create a connection with the Twitter API. 

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)



sentiment <- function(word,tweets_num=500) {
  
  
  
  #Fetches tweets from twitter and stores them in a data frame.
  
  string <- paste0(word,"-filter:retweets")
  
  tweets <- searchTwitter(string,n=tweets_num, lang="en",resultType = "recent")
  
  tweetsdf <- twListToDF(tweets)
  
  tweetsdf <- data.frame(text=tweetsdf$text)
  
  
  
  #Text processing and clean up. Remove unnecessary punctuation and characters. 
  
  tweetsdf$text <- gsub("@[[:alnum:]]+ *","",tweetsdf$text)
  
  tweetsdf$text <- gsub("http[[:alnum:][:punct:]]+ *","",tweetsdf$text)
  
  tweetsdf$text <- gsub("[^[:alpha:][:space:]]*","",tweetsdf$text)
  
  tweetsdf$text <- gsub("[[:punct:]]","",tweetsdf$text)
  
  tweetsdf$text <- gsub("\n","",tweetsdf$text)
  
  tweetsdf$text <- tolower(tweetsdf$text)
  
  
  
  #Matches the tweets with words in the Sentiment Dictionary and assigns the appropriate score to each tweet.
  
  sentiscores <- unlist(lapply(tweetsdf$text,function(x){
    
    sum(as.numeric(dict$Score[match(unlist(strsplit(x," ")),dict$Word)]),na.rm=T)
    
  }))
  
  
  
  sentiscores[is.na(sentiscores)] <- as.numeric(0)
  
  sentiscoresdf <- data.frame(Scores=sentiscores)
  
  sentiscoresdf$color <- ifelse(sentiscoresdf$Scores > 0,"positive",ifelse(sentiscoresdf$Scores == 0,"neutral","negative"))
  
  
  
  #Writes the resulting data frame to a file in the current working irectory and displays a bar plot of the Sentiment Score. 
  
  list(write.csv(cbind(tweetsdf,Scores=sentiscores),paste0(word,"scores.csv"),row.names = F),
       
       ggplot(data=sentiscoresdf,aes(x=Scores))+geom_bar(aes(fill=color))+
         
         ylab("No. Of Tweets")+xlab("Sentiment Score")+theme_minimal()+geom_text(stat="count",aes(label=..count..),vjust=-.5)
       
       +scale_x_continuous(breaks=c(-7:7))+scale_fill_manual(values = c(positive="steelblue",negative="firebrick1",neutral="yellowgreen")))
  
  
  
}





#After loading the script call the function "sentiment" which has two arguments, "word" which is the search string nd "tweets_num" which is the number of tweets to be fetched.

