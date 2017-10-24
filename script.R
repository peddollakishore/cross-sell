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
train3<-cbind(train2,train1$RESPONDERS)
train3$target<-ifelse(train3$`train1$RESPONDERS`=='N',0,1)
train3$target<-as.factor(train3$target)
summary(train3)
names(train3)
prop.table(table(train3$target))

train3$`train1$RESPONDERS`[train3$`train1$RESPONDERS`=='N']<-'0'
str(train3)
library(caret)
set.seed(1234)
splitIndex <- createDataPartition(train3$target, p = .70,
                                  list = FALSE,
                                  times = 1)
trainSplit <- train3[ splitIndex,]
testSplit <- train3[-splitIndex,]
str(trainSplit)
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(target ~ ., data = trainSplit[-1], method = "treebag",
                 trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'target']
pred <- predict(tbmodel$finalModel, testSplit[,predictors])








