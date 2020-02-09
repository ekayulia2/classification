
#implementasikan bayes classifier untuk sentiment analysis:
library(sentiment)
library(ggplot2)
library(lubridate)

# ===== LOAD THE DATA
data.gojek.df = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/DataGojek.csv',
                          header = TRUE,
                          sep = ',')
results = read.csv(file = 'D:/S2/big data/percobaan eka/Datasets/Sentiment Data Gojek.csv',
                   header = TRUE,
                   sep = ',')
gojekTweetsClassEmotico = classify_emotion(data.gojek.df, algorithm="bayes", prior=1.0)
head(gojekTweetsClassEmotico, 20)
gojekEmotion = gojekTweetsClassEmotico[,7]
gojekEmotion[is.na(gojekEmotion)] = "unknown"
head(gojekEmotion, 20)
gojekTweetsClassPol = classify_polarity(data.gojek.df, algorithm = "bayes")
head(gojekTweetsClassPol, 20)
gojekPol = gojekTweetsClassPol[,4]
gojekSentimenDataFrame = data.frame(text=data.gojek.df, emotion=gojekEmotion, polarity=gojekPol, stringAsFactors=FALSE)
gojekSentimenDataFrame = within(gojekSentimenDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
head(gojekSentimenDataFrame, 20)
#function for plotting
plotSentiments1 <- function(sentiment_dataframe, title) 
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
plotSentiments1(gojekSentimenDataFrame, "Sentiment Analysis of Tweets on Twitter about Go-Jek")

# ===== HISTOGRAM OF SENTIMENT SRORE
df.score = subset(results,
                  class == c('Negative','Positive'))
colnames(df.score) = c('Score',
                       'Text',
                       'Sentiment')
ggplot(df.score)+
  geom_density(aes(x = Score,
                   fill = Sentiment),
               alpha = 0.75)+
  xlim(c(-11,11))+
  labs(title = 'Density Plot of Sentiment Scores',
       subtitle = 'Transportasion Online -GOJEK',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+ 
  ylab('Density')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# ===== BARPLOT OF SENTIMENT
df.sentiment = as.data.frame(table(results$class))
colnames(df.sentiment) = c('Sentiment',
                           'Freq')
ggplot(df.sentiment)+
  geom_bar(aes(x = Sentiment,
               y = Freq,
               fill = I('red')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  labs(title = 'Polarity of Sentiments',
       subtitle = 'Gojek',
       caption = '')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')
# PIECHART OF SENTIMENT
df.pie = df.sentiment
df.pie$Prop = df.pie$Freq/sum(df.pie$Freq)
# Manipulate the Data
df.pie = df.pie %>%
  arrange(desc(Sentiment)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

ggplot(df.pie,
       aes(x = 2,
           y = Prop,
           fill = Sentiment))+
  geom_bar(stat = 'identity',
           col = 'white',
           alpha = 0.75,
           show.legend = TRUE)+
  coord_polar(theta = 'y', 
              start = 0)+
  geom_text(aes(y = lab.ypos,
                label = Prop),
            color = 'white',
            fontface = 'italic',
            size = 4)+
  labs(title = 'Piechart of Sentiments',
       subtitle = 'GOJEK',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlim(c(0.5,2.5))+
  theme_void()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(legend.title = element_blank(),
        legend.position = 'right')

# ===== BARPLOT OF SENTIMENT SCORE
df.sentiment.score = data.frame(table(results$score))
colnames(df.sentiment.score) = c('Score',
                                 'Freq')
df.sentiment.score$Score = as.character(df.sentiment.score$Score)
df.sentiment.score$Score = as.numeric(df.sentiment.score$Score)
Score1 = df.sentiment.score$Score
sign(df.sentiment.score[1,1])
for (i in 1:nrow(df.sentiment.score)) {
  sign.row = sign(df.sentiment.score[i,'Score'])
  for (j in 1:ncol(df.sentiment.score)) {
    df.sentiment.score[i,j] = df.sentiment.score[i,j] * sign.row
  }
}
df.sentiment.score$Label = c(letters[1:nrow(df.sentiment.score)])
df.sentiment.score$Sentiment = ifelse(df.sentiment.score$Freq < 0,
                                      'Negative','Positive')
df.sentiment.score$Score1 = Score1

ggplot(df.sentiment.score)+
  geom_bar(aes(x = Label,
               y = Freq,
               fill = Sentiment),
           stat = 'identity',
           show.legend = FALSE)+
  # Positive Sentiment
  geom_hline(yintercept = mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq'])),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq'])))),
                x = 10,
                y = mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq']))+30),
            hjust = 'right',
            size = 4)+
  # Negative Sentiment
  geom_hline(yintercept = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq']),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])))),
                x = 5,
                y = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])-15),
            hjust = 'left',
            size = 4)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'Joko Widodo',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+
  scale_x_discrete(limits = df.sentiment.score$Label,
                   labels = df.sentiment.score$Score1)+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

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

