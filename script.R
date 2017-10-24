library(caret)

train<-read.csv('train.csv')
test<-read.csv('test.csv')
train1<-train
train<-train1

# remove empty columns
train <- train[,colSums(is.na(train))<nrow(train)]
# Missing value persentage for each col
apply(train,2,function(col)sum(is.na(col))/length(col))

nearZeroVar(train)

if (length(nearZeroVar(train)) > 0) {
  train <- train[, -nearZeroVar(train)] 
}

# after history run

train2 <- train[lapply(train, function(x) sum(is.na(x)) / length(x) ) < 0.9 ]
train3<-rbind(train2,train$)
names(train)
names(test)
table(train)