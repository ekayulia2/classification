
#implementasikan bayes classifier untuk sentiment analysis:
library(sentiment)
library(ggplot2)
library(lubridate)

# ===== LOAD THE DATA
data.grab.df = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/DataGrab.csv',
                         header = TRUE,
                         sep = ',')
results.grab = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/Sentiment Data Grab.csv',
                   header = TRUE,
                   sep = ',')
grabTweetsClassEmotico = classify_emotion(data.grab.df, algorithm="bayes", prior=1.0)
head(grabTweetsClassEmotico, 20)
grabEmotion = grabTweetsClassEmotico[,7]
grabEmotion[is.na(grabEmotion)] = "unknown"
head(grabEmotion, 20)
grabTweetsClassPol = classify_polarity(data.grab.df, algorithm = "bayes")
head(grabTweetsClassPol, 20)
grabPol = grabTweetsClassPol[,4]
grabSentimenDataFrame = data.frame(text=data.grab.df, emotion=grabEmotion, polarity=grabPol, stringAsFactors=FALSE)
grabSentimenDataFrame = within(grabSentimenDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
head(grabSentimenDataFrame, 20)
#function for plotting
plotSentiments3 <- function(sentiment_dataframe, title) 
{
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) + 
    geom_bar(aes(y=..count.., fill=emotion)) + 
    scale_fill_brewer(palette="Dark2") + 
    ggtitle(title) + 
    theme(legend.position="right") + 
    ylab("Number of Tweets") + 
    xlab("Emotion Categories")
}
plotSentiments3(grabSentimenDataFrame, "Sentiment Analysis of Tweets on Twitter about GRAB")

# ===== HISTOGRAM OF SENTIMENT SRORE
df.score.grab = subset(results.grab,
                  class == c('Negative','Positive'))
colnames(df.score.grab) = c('Score',
                       'Text',
                       'Sentiment')
ggplot(df.score.grab)+
  geom_density(aes(x = Score,
                   fill = Sentiment),
               alpha = 0.75)+
  xlim(c(-11,11))+
  labs(title = 'Density Plot of Sentiment Scores',
       subtitle = 'Transportasion Online -GRAB',
       caption = '')+
  xlab('Score')+ 
  ylab('Density')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# ===== BARPLOT OF SENTIMENT
df.sentiment.grab = as.data.frame(table(results.grab$class))
colnames(df.sentiment.grab) = c('Sentiment',
                           'Freq')
ggplot(df.sentiment.grab)+
  geom_bar(aes(x = Sentiment,
               y = Freq,
               fill = I('red')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  labs(title = 'Polarity of Sentiments',
       subtitle = 'GRAB',
       caption = '')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')


# ===== BARPLOT OF SENTIMENT SCORE
df.sentiment.score.grab = data.frame(table(results.grab$score))
colnames(df.sentiment.score.grab) = c('Score',
                                 'Freq')
df.sentiment.score.grab$Score = as.character(df.sentiment.score.grab$Score)
df.sentiment.score.grab$Score = as.numeric(df.sentiment.score.grab$Score)
Score1.grab = df.sentiment.score.grab$Score
sign(df.sentiment.score.grab[1,1])
for (i in 1:nrow(df.sentiment.score.grab)) {
  sign.row = sign(df.sentiment.score.grab[i,'Score'])
  for (j in 1:ncol(df.sentiment.score.grab)) {
    df.sentiment.score.grab[i,j] = df.sentiment.score.grab[i,j] * sign.row
  }
}
df.sentiment.score.grab$Label = c(letters[1:nrow(df.sentiment.score.grab)])
df.sentiment.score.grab$Sentiment = ifelse(df.sentiment.score.grab$Freq < 0,
                                      'Negative','Positive')
df.sentiment.score.grab$Score1 = Score1.grab

ggplot(df.sentiment.score.grab)+
  geom_bar(aes(x = Label,
               y = Freq,
               fill = Sentiment),
           stat = 'identity',
           show.legend = FALSE)+
  # Positive Sentiment
  geom_hline(yintercept = mean(abs(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Positive'),'Freq'])),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Positive'),'Freq'])))),
                x = 10,
                y = mean(abs(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Positive'),'Freq']))+30),
            hjust = 'right',
            size = 4)+
  # Negative Sentiment
  geom_hline(yintercept = mean(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Negative'),'Freq']),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Negative'),'Freq'])))),
                x = 5,
                y = mean(df.sentiment.score.grab[which(df.sentiment.score.grab$Sentiment == 'Negative'),'Freq'])-15),
            hjust = 'left',
            size = 4)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'GRAB ',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+
  scale_x_discrete(limits = df.sentiment.score.grab$Label,
                   labels = df.sentiment.score.grab$Score1)+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

# ===== BARPLOT OF FREQUENT WORDS
term.freq.grab= rowSums(as.matrix(tdm.grab))
term.freq.grab= subset(term.freq.grab, term.freq.grab >= 100)
df.grab = data.frame(term = names(term.freq.grab), 
                freq = term.freq.grab)
ggplot(df.grab)+ 
  geom_bar(aes(x = reorder(term,
                           freq),
               y = freq),
           stat = 'identity',
           fill = I('red'),
           alpha = 0.6)+
  labs(title = 'Frequent Words',
       subtitle = 'GRAB ',
       caption = 'gojekID')+
  xlab('Terms')+ 
  ylab('Count')+
  coord_flip()+
  theme(axis.text = element_text(size = 9))+
  theme_bw()  
