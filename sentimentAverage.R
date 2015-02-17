# the dictionary source is here:
#'http://crr.ugent.be/archives/1003
#'==========================================================
#'http://analyzecore.com/2014/05/11/twitter-sentiment-analysis-based-on-affective-lexicons-in-r/
#'======================================================
#'Note, I use average rating of evaluated words which I find in tweet. For 
#'example, if we found “good” (4 points) and “perfect” (6 points) in one tweet,
#' it would be evaluated as (4+6)/2=5. This is better than use total sum in 
#' case of all words in dictionary have positive rating. For example, one 
#' “good” (4 points) should be better than three “bad” (1,5 points each). 
#' This solves via average value.


#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)

#connect to API
download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'
consumerKey <- '____________' #put the Consumer Key from Twitter Application
consumerSecret <- '______________'  #put the Consumer Secret from Twitter Application
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=reqURL,
                         accessURL=accessURL,
                         authURL=authURL)
Cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')) #There is URL in Console. You need to go to it, get code and enter it on Console

save(Cred, file='twitter authentication.Rdata')
load('twitter authentication.Rdata') #Once you launch the code first time, you can start from this line in the future (libraries should be connected)
registerTwitterOAuth(Cred)

#the function of tweets accessing and analyzing
search <- function(searchterm)
{
    #access tweets and create cumulative file
    list <- searchTwitter(searchterm, cainfo='cacert.pem', n=1500)
    df <- twListToDF(list)
    df <- df[, order(names(df))]
    df$created <- strftime(df$created, '%Y-%m-%d')
    if (file.exists(paste(searchterm, '_stack_val.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack_val.csv'), row.names=F)
    
    #merge last access with cumulative file and remove duplicates
    stack <- read.csv(file=paste(searchterm, '_stack_val.csv'))
    stack <- rbind(stack, df)
    stack <- subset(stack, !duplicated(stack$text))
    write.csv(stack, file=paste(searchterm, '_stack_val.csv'), row.names=F)
    
    #evaluation tweets function
    score.sentiment <- function(sentences, valence, .progress='none')
    {
        require(plyr)
        require(stringr)
        scores <- laply(sentences, function(sentence, valence){
            sentence <- gsub('[[:punct:]]', '', sentence) #cleaning tweets
            sentence <- gsub('[[:cntrl:]]', '', sentence) #cleaning tweets
            sentence <- gsub('\\d+', '', sentence) #cleaning tweets
            sentence <- tolower(sentence) #cleaning tweets
            word.list <- str_split(sentence, '\\s+') #separating words
            words <- unlist(word.list)
            val.matches <- match(words, valence$Word) #find words from tweet in "Word" column of dictionary
            val.match <- valence$Rating[val.matches] #evaluating words which were found (suppose rating is in "Rating" column of dictionary).
            val.match <- na.omit(val.match)
            val.match <- as.numeric(val.match)
            score <- sum(val.match)/length(val.match) #rating of tweet (average value of evaluated words)
            return(score)
        }, valence, .progress=.progress)
        scores.df <- data.frame(score=scores, text=sentences) #save results to the data frame
        return(scores.df)
    }
    
    valence <- read.csv('dictionary.csv', sep=',' , header=TRUE) #load dictionary from .csv file
    
    Dataset <- stack
    Dataset$text <- as.factor(Dataset$text)
    scores <- score.sentiment(Dataset$text, valence, .progress='text') #start score function
    write.csv(scores, file=paste(searchterm, '_scores_val.csv'), row.names=TRUE) #save evaluation results into the file
    
    #modify evaluation
    stat <- scores
    stat$created <- stack$created
    stat$created <- as.Date(stat$created)
    stat <- na.omit(stat) #delete unvalued tweets
    write.csv(stat, file=paste(searchterm, '_opin_val.csv'), row.names=TRUE)
    
    #create chart
    ggplot(stat, aes(created, score)) + geom_point(size=1) +
        stat_summary(fun.data = 'mean_cl_normal', mult = 1, geom = 'smooth') +
        ggtitle(searchterm)
    
    ggsave(file=paste(searchterm, '_plot_val.jpeg'))
}

search("______") #enter keyword