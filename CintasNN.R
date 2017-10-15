cat("\014") 
rm(list = ls())

library(neuralnet)
library(caret)
library(e1071)
library(nnet)

dat <- read.csv("/Users/Roshan/Desktop/Machine Learning/Cintas Data/Cintas1.csv", header = TRUE)

set.seed(42)

## Features slected after the feature selection
vars <- c("X50.calendar.day.moving.average","assetnew", "betanew", "CTAS.Enterprise.Value..In.Millions..Basically.the.first.value.is.11.5.B.",
          "EBITDA_mn" , "Open..Nasdaq.", "Price.of.Oil",
          "returnnew", "X50.Day.Simple.Moving.Average..S.P.500.", "tobin_mn",
          "PE.Ratio", "Volume","Indicator")
Ycolumn <- "Indicator"

alldata <- dat[, vars];
y <- alldata$Indicator;

##remove NA and data cleaning
alldata_new <- na.omit(alldata);
data<- alldata_new
rm(alldata_new)

## Factor levels

data$assetnew<- as.numeric(as.factor(data$assetnew))
data$EBITDA_mn<- as.numeric(as.factor(data$EBITDA_mn))
#data$reinvestednew<- as.numeric(as.factor(data$reinvestednew))
data$returnnew<- as.numeric(as.factor(data$returnnew))
data$tobin_mn<- as.numeric(as.factor(data$tobin_mn))
#data$zminew<- as.numeric(as.factor(data$zminew))
data$Volume<-as.numeric(as.factor(data$Volume))
data$Indicator<- as.numeric(as.factor(data$Indicator))


##

tab=as.table(matrix(0, nrow=2, ncol=2))
colnames(tab) <- c("Increase", "Decrease")
rownames(tab) <- c("Increase", "Decrease")

for (i in 1:10){
  ##splitting the data into training and testing
  index <- sample(1:nrow(data),round(0.90*nrow(data)))
  train <- data[index,]
  test <- data[-index,]
  
  # train_indi <- train$Indicator
  # train$Indicator <- NULL
  
  formula <- as.formula(paste(Ycolumn,paste(vars[1:12],collapse=' + '),sep=' ~ '))
  
  
  # obj <- tune.nnet( formula,data=train, size = 1:5,
  #                   tunecontrol = tune.control(nrepeat = 5)
  #           )
  
  ## Grid search in Neural net and picking best parameters
  ## After doing this, we got size =5 and decay as 0.1 as best, so picking that values 
  
  model <- train(formula,data=train, method='nnet', linout=TRUE, trace = FALSE,
                 #Grid of tuning parameters to try:
                 tuneGrid=expand.grid(.size=5,.decay=0.1))
  
  
  #plot(model)
  ps <- predict(model, test[,1:12])
  
  pr.nn.results <- round(ps,0)
  tab1 <- table(pr.nn.results,test$Indicator)
  tab <- tab +tab1
  print ("Table")
  print (tab1)
}

confusionMatrix(tab)

