library(tm)
install.packages('Text')
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
install.packages('doMC')
library(doMC)
registerDoMC(cores=detectCores())



df.grab<- read.csv('D:/S2/big data/percobaan eka/Datasets/Sentiment Data Grab.csv',
              header = TRUE,
              sep = ',')
glimpse(df.grab)
set.seed(1)
df.grab <- df.grab[sample(nrow(df.grab)), ]
df.grab <- df.grab[sample(nrow(df.grab)), ]
glimpse(df.grab)
df.grab$class <- as.factor(df.grab$class)
corpus.grab <- Corpus(VectorSource(df.grab$text))
corpus.grab
inspect(corpus.grab[1:3])

corpus.clean.grab=corpus.grab
dtm.grab <- DocumentTermMatrix(corpus.clean.grab)
inspect(dtm[40:50, 10:15])

df.train.grab <- df.grab[1:700,]
df.test.grab <- df.grab[701:1000,]

dtm.train.grab <- dtm.grab[1:700,]
dtm.test.grab <- dtm.grab[701:1000,]

corpus.clean.train.grab <- corpus.clean.grab[1:700]
corpus.clean.test.grab <- corpus.clean.grab[701:1000]
dim(dtm.train.grab)

fivefreq.grab <- findFreqTerms(dtm.train.grab, 5)
length((fivefreq.grab))

dtm.train.nb.grab <- DocumentTermMatrix(corpus.clean.train.grab, control=list(dictionary = fivefreq.grab))
dim(dtm.train.nb.grab)

dtm.test.nb.grab<- DocumentTermMatrix(corpus.clean.test.grab, control=list(dictionary = fivefreq.grab))
dim(dtm.test.nb.grab)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("Negative", "Positive"))
  y
}

trainNB.grab <- apply(dtm.train.nb.grab, 2, convert_count)
testNB.grab <- apply(dtm.test.nb.grab, 2, convert_count)
ctrl = trainControl(method = 'cv',
                    number = 10)

system.time( classifier.grab <- naiveBayes(trainNB.grab, df.train$class,trControl = ctrl) )
system.time( pred.grab <- predict(classifier.grab, newdata=testNB.grab) )

table("Predictions"= pred.grab,  "Actual" = df.test.grab$class )

conf.mat.grab <- confusionMatrix(pred.grab, df.test.grab$class)
conf.mat.grab$overall['Accuracy']
