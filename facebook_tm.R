library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(syuzhet)

cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', # Consumer Key (API Key)
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Access Token Secret
setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  #Access Token Secret

Tweets <- userTimeline('facebook', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
write.csv(TweetsDF, "Tweets.csv",row.names = F)
getwd()

facebook <- read.csv(file.choose())
str(facebook)
View(facebook)

library(tm)
# Corpus and DTM/TDM
corpus <- facebook$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Cleaning 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('facebook','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term Document Matrix 
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot 
w <- rowSums(tdm)  
w <- subset(w, w>= 25) 
barplot(w, las = 2, col = rainbow(50)

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)
wordcloud(words = names(w), freq = w, max.words = 250,random.order = F,min.freq =  3,colors = brewer.pal(8, 'Dark2'),scale = c(5,0.3),rot.per = 0.6)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')

# Sentiment Analysis for tweets
fbdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(fbdata$text)
class(tweets)

#Sentiment scores 
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('pretending')
get_nrc_sentiment('can learn') 

# barplot 
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Facebook Tweets')

