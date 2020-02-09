
library(twitteR)
library(tm)
library(dplyr)
library(stringr)
library(katadasaR)
library(ggplot2)
library(lubridate)

# ===== CRAWLING DTA FROM TWITTER
api_key<- "qyvCWVkyalBMk3P0DWsgvSiRp"
api_secret<- "kk8g44RUOY3S4hF5IG06sFoyD0API7oOtsi6ZSWfpuzMaBla1M"
access_token<- "83317394-4G9NubR29WOK41fog9vSLKWWg3q07CW7nFE4Llhke"
access_token_secret<- "IsQjX7u4EC5GufA8phS136o28GcNg20xfZ2NDmeIvK8IP"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
datagrab = searchTwitter(searchString = '#grabID')
# ===== CONVERT DATA TO DATA FRAME
data.grab.df = twListToDF(datagrab)
View(data.grab.df)

# ===== SAVE DATA
write.csv(x = dfGr,
          file = 'D:/S2/big data/percobaan eka/Datasets/DataGrab.csv',
          row.names = FALSE)
# Load Data from Computer
data.grab.df = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/DataGrab.csv',
                           header = TRUE,
                           sep = ',')

# ===== DATA CLEANSING
# Work with Corpus
tweet.corpus.grab = VCorpus(VectorSource(data.grab.df$text))
inspect(tweet.corpus.grab[[1]])
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
# TRANSFORM TO LOWER CASE
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(tolower))
# FILTERING - CUSTOM CLEANSING FUNCTIONS
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(removeURL))
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(unescapeHTML))
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(removeMention))
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(removeCarriage))
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(removeInvoice))
# Remove additional symbols to white space
tweet.corpus.grab = tm_map(tweet.corpus.grab, toSpace, "[[:punct:]]") # punctuation
tweet.corpus.grab = tm_map(tweet.corpus.grab, toSpace, "[[:digit:]]") # numbers
# Eliminate extra white spaces
tweet.corpus.grab = tm_map(tweet.corpus.grab, stripWhitespace)
# Check the final result
inspect(tweet.corpus.grab[[1]])
# SPELLING NORMALIZATION
spell.lex = read.csv(file = 'D:/S2/big data/percobaan eka/Helpers/Data Helper/colloquial-indonesian-lexicon.txt',
                     header = TRUE,
                     sep = ',',
                     stringsAsFactors = FALSE)
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
tweet.corpus.grab = tm_map(tweet.corpus.grab, spell.correction, spell.lex)
# STEMMING WORDS
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Cleansing.R')
tweet.corpus.grab = tm_map(tweet.corpus.grab, content_transformer(stemming))

# ===== LEXICON-BASED SENTIMENT SCORING
pos = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/s-pos.txt',warn=FALSE)
neg = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/s-neg.txt',warn=FALSE)
df.tweet.grab = data.frame(text = sapply(tweet.corpus.grab, as.character),
                      stringsAsFactors = FALSE)
# Negation Handling
negasi= scan('D:/S2/big data/percobaan eka/Helpers/Data Helper/negatingword.txt',
              what = 'character')
senti = read.csv('D:/S2/big data/percobaan eka/Helpers/Data Helper/sentiwords_id.txt',
                 sep = ':', 
                 header = FALSE) %>% 
  mutate(words = as.character(V1),
         score = as.numeric(V2)) %>% 
  select(c('words','score'))
booster.grab = read.csv('D:/S2/big data/percobaan eka/Helpers/Data Helper/boosterwords_id.txt',
                   sep = ":", 
                   header = FALSE) %>%
  mutate(words = as.character(V1),
         score = as.numeric(V2)) %>% 
  select(c('words','score'))
# Import external function
source(file = 'D:/S2/big data/percobaan eka/Helpers/Function Helper/Lexicon-Based Scoring Analysis.R')
results.grab = scores.sentiment(df.tweet.grab$text, senti, negasi)

set.seed(2211)
random.tweet.grab = sample(x = df.tweet.grab$text, 
                      size = 10)
head(scores.sentiment(random.tweet.grab, senti, negasi),5)
# Convert score to sentiment classes
results.grab$class = as.factor(ifelse(results.grab$score < 0, 'Negative',
                                 ifelse(results.grab$score == 0, 'Neutral','Positive')))
# ===== SAVE DATA RESULTS
write.csv(x = results,
          file = 'D:/S2/big data/percobaan eka/Datasets/Sentiment Data Grab.csv',
          row.names = FALSE)

# ===== FILTERING STOPWORDS
rm.stopword.grab = VCorpus(VectorSource(results.grab$text))
# Using edited stopword list
stopwords.id.grab = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/stopwords-id.txt',warn=FALSE)
rm.stopword.grab = tm_map(rm.stopword.grab, removeWords, stopwords.id.grab)
# Using manually added stopword list
stopwords.id.grab = readLines('D:/S2/big data/percobaan eka/Helpers/Data Helper/stopwords-manual.txt',warn=FALSE)
rm.stopword.grab= tm_map(rm.stopword.grab, removeWords, stopwords.id.grab)
# Check the final result
inspect(rm.stopword.grab[[2]])
# Eliminate extra white spaces
rm.stopword.grab = tm_map(rm.stopword.grab, stripWhitespace)

# TOKENIZATION - FREQUENT WORDS
tdm.grab = TermDocumentMatrix(rm.stopword.grab, 
                         control = list(wordLengths = c(1, Inf)))
freq.terms.grab = findFreqTerms(tdm.grab, 
                           lowfreq = 20)
freq.terms.grab[1:50]

# ===== BARPLOT OF FREQUENT WORDS
term.freq.grab = rowSums(as.matrix(tdm.grab))
term.freq.grab = subset(term.freq.grab, term.freq.grab >= 100)
df.grab = data.frame(term = names(term.freq.grab), 
                freq = term.freq.grab)
ggplot(df.grab)+ 
  geom_bar(aes(x = reorder(term,freq),
               y = freq),
           stat = 'identity',
           fill = I('red'),
           alpha = 0.6)+
  labs(title = 'Frequent Words',
       subtitle = 'Prabowo Subianto',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Terms')+ 
  ylab('Count')+
  coord_flip()+
  theme(axis.text = element_text(size = 9))+
  theme_bw()
