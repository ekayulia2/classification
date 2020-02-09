library(tm)
install.packages('Text')
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
install.packages('doMC')
library(doMC)
registerDoMC(cores=detectCores())



df<- read.csv('D:/S2/big data/percobaan eka/Datasets/Sentiment Data Gojek.csv',
              header = TRUE,
              sep = ',')
glimpse(df)
set.seed(1)
df <- df[sample(nrow(df)), ]
df <- df[sample(nrow(df)), ]
glimpse(df)
df$class <- as.factor(df$class)
corpus <- Corpus(VectorSource(df$text))
corpus
inspect(corpus[1:3])

corpus.clean=corpus
dtm <- DocumentTermMatrix(corpus.clean)
inspect(dtm[40:50, 10:15])

df.train <- df[1:700,]
df.test <- df[701:1000,]

dtm.train <- dtm[1:700,]
dtm.test <- dtm[701:1000,]

corpus.clean.train <- corpus.clean[1:700]
corpus.clean.test <- corpus.clean[701:1000]
dim(dtm.train)

fivefreq <- findFreqTerms(dtm.train, 5)
length((fivefreq))

dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
dim(dtm.train.nb)

dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
dim(dtm.test.nb)

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("Negative", "Positive"))
  y
}

trainNB <- apply(dtm.train.nb, 2, convert_count)
testNB <- apply(dtm.test.nb, 2, convert_count)
ctrl = trainControl(method = 'cv',
                    number = 10)

system.time( classifier <- naiveBayes(trainNB, df.train$class,trControl = ctrl) )
system.time( pred <- predict(classifier, newdata=testNB) )

table("Predictions"= pred,  "Actual" = df.test$class )

conf.mat <- confusionMatrix(pred, df.test$class)
conf.mat$overall['Accuracy']


