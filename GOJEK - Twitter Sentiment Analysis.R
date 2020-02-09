# http://hakim-azizul.com/bermain-twitter-dengan-r-part-2-finding-trending-topics/
# https://www.nurandi.id/
install.packages('twitteR')
install.packages('devtools')
library(twitteR)
library(tm)
library(dplyr)
library(stringr)
library(katadasaR)
library(ggplot2)
#install.packages("devtools")
library(devtools)
#install_github("nurandi/katadasaR")

# ===== CRAWLING DTA FROM TWITTER

api_key<- "qyvCWVkyalBMk3P0DWsgvSiRp"
api_secret<- "kk8g44RUOY3S4hF5IG06sFoyD0API7oOtsi6ZSWfpuzMaBla1M"
access_token<- "83317394-4G9NubR29WOK41fog9vSLKWWg3q07CW7nFE4Llhke"
access_token_secret<- "IsQjX7u4EC5GufA8phS136o28GcNg20xfZ2NDmeIvK8IP"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
datagojekya = searchTwitter(searchString = '#gojekID')
# ===== CONVERT DATA TO DATA FRAME
data.gojek.df = twListToDF(data.jokowi)
View(data.gojek.df)

# ===== SAVE DATA
write.csv(x = dfGid,file = 'D:/S2/big data/percobaan eka/Datasets/DataGojek.csv',row.names = FALSE)
# Load Data from Computer
data.gojek.df = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/DataGojek.csv',header = TRUE,sep = ',')
data.gojek.df=gojek.df %>% distinct

# ===== DATA CLEANSING
# Work with Corpus
tweet.corpus = VCorpus(VectorSource(data.gojek.df$text))
inspect(tweet.corpus[[1]])
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
# TRANSFORM TO LOWER CASE
tweet.corpus = tm_map(tweet.corpus, content_transformer(tolower))
# FILTERING - CUSTOM CLEANSING FUNCTIONS
tweet.corpus = tm_map(tweet.corpus, content_transformer(removeURL))
tweet.corpus = tm_map(tweet.corpus, content_transformer(unescapeHTML))
tweet.corpus = tm_map(tweet.corpus, content_transformer(removeMention))
tweet.corpus = tm_map(tweet.corpus, content_transformer(removeCarriage))
tweet.corpus = tm_map(tweet.corpus, content_transformer(removeInvoice))
# Remove additional symbols to white space
tweet.corpus = tm_map(tweet.corpus, toSpace, "[[:punct:]]") # punctuation
tweet.corpus = tm_map(tweet.corpus, toSpace, "[[:digit:]]") # numbers
# Eliminate extra white spaces
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
# Check the final result
inspect(tweet.corpus[[1]])
# SPELLING NORMALIZATION
spell.lex = read.csv(file = 'D:/S2/big data/percobaan eka/Helpers/Data Helper/colloquial-indonesian-lexicon.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
tweet.corpus = tm_map(tweet.corpus, spell.correction, spell.lex)

# STEMMING WORDS
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
tweet.corpus = tm_map(tweet.corpus, content_transformer(stemming))

# ===== LEXICON-BASED SENTIMENT SCORING
pos = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/s-pos.txt',warn=FALSE)
neg = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/s-neg.txt',warn=FALSE)
# Negation Handling
negasi = scan('D:/S2/big data/percobaan eka/Helpers/Data Helper/negatingword.txt',
              what = 'character')
senti = read.csv('D:/S2/big data/percobaan eka/Helpers/Data Helper/sentiwords_id.txt',
                 sep = ':', 
                 header = FALSE) %>% 
  mutate(words = as.character(V1),
         score = as.numeric(V2)) %>% 
  select(c('words','score'))
booster = read.csv('D:/S2/big data/percobaan eka/Helpers/Data Helper/boosterwords_id.txt',
                   sep = ":", 
                   header = FALSE) %>%
  mutate(words = as.character(V1),
         score = as.numeric(V2)) %>% 
  select(c('words','score'))
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Lexicon-Based Scoring Analysis.R')
install.packages('plyr')
library(plyr)
library(dplyr)
results = scores.sentiment(df.tweet$text, senti, negasi )

set.seed(2211)
random.tweet = sample(x = df.tweet$text,size = 10)
head(scores.sentiment(random.tweet, senti, negasi),5)
# Convert score to sentiment classes
results$class = as.factor(ifelse(results$score < 0, 'Negative',ifelse(results$score == 0, 'Neutral','Positive')))
# ===== SAVE DATA RESULTS
write.csv(x = results,file = 'D:/S2/big data/percobaan eka/Datasets/Sentiment Data Gojek.csv',row.names = FALSE)

# ===== FILTERING STOPWORDS
rm.stopword = VCorpus(VectorSource(results$text))
# Using edited stopword list
stopwords.id = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/stopwords-id.txt',warn=FALSE)
rm.stopword = tm_map(rm.stopword, removeWords, stopwords.id)
# Using manually added stopword list
stopwords.id = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/stopwords-manual.txt',warn=FALSE)
rm.stopword = tm_map(rm.stopword, removeWords, stopwords.id)
# Check the final result
inspect(rm.stopword[[1]])
# Eliminate extra white spaces
rm.stopword = tm_map(rm.stopword, stripWhitespace)

# TOKENIZATION - FREQUENT WORDS
tdm = TermDocumentMatrix(rm.stopword,control = list(wordLengths = c(1, Inf)))
freq.terms = findFreqTerms(tdm,lowfreq = 20)
freq.terms[1:50]

# ===== WORDCLOUD
wordcloud(rm.stopword,
         max.words = 100,
          min.freq = 25,
          random.order = FALSE,
         colors = brewer.pal(8, "Dark2"))

# ===== BARPLOT OF FREQUENT WORDS
term.freq = rowSums(as.matrix(tdm))
term.freq = subset(term.freq, term.freq >= 100)
df = data.frame(term = names(term.freq), 
                freq = term.freq)
ggplot(df)+ 
  geom_bar(aes(x = reorder(term,
                           freq),
               y = freq),
           stat = 'identity',
           fill = I('red'),
           alpha = 0.6)+
  labs(title = 'Frequent Words',
       subtitle = 'GOJEK ',
       caption = 'Twitter Crawling gojekID')+
  xlab('Terms')+ 
  ylab('Count')+
  coord_flip()+
  theme(axis.text = element_text(size = 9))+
  theme_bw()


