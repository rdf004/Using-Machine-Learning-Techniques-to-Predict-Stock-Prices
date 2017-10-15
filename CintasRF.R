cat("\014") 
rm(list = ls())
library(caret)

library(randomForest)

library(e1071)


dat <- read.csv("/Users/Roshan/Desktop/Machine Learning/Cintas Data/Cintas1.csv", header = TRUE)

set.seed(42)

## Features slected after the feature selection
vars <- c("X50.calendar.day.moving.average","assetnew", "betanew", "CTAS.Enterprise.Value..In.Millions..Basically.the.first.value.is.11.5.B.",
          "EBITDA_mn" , "Close..Nasdaq.", "Open..Nasdaq.", "Price.of.Oil",
          "returnnew", "X50.Day.Simple.Moving.Average..S.P.500.", "tobin_mn",
          "PE.Ratio","Indicator")
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
  
  
  
  ######################################################################
  #################### Random Forest classifier
  model.forest <- randomForest(formula, 
                               data = train, ntree = 500)
  
  # View the forest results.
  print(model.forest) 
  
  # Importance of each predictor.
  # print(importance(fit,type = 2)) 
  
  #plot(model)
  ps <- predict(model.forest, test[,1:12])
  
  pr.rf.results <- round(ps,0)
  tab1 <- table(pr.rf.results,test$Indicator)
  tab <- tab +tab1
  print ("Table")
  print (tab1)
}

confusionMatrix(tab)
