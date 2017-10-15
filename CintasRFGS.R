### Random Forest on Cintas1 with grid search
#Line 71 without GS line 73 with GS

cat("\014") 
rm(list = ls())

library(randomForest)

library(e1071)
library(caret)

dat <- read.csv("/Users/Roshan/Documents/Machine Learning/Stocks/Oil/Chevron_3days4.csv", header = TRUE)
newData <- read.csv("/Users/Roshan/Documents/Machine Learning/Stocks/Tech/Apple_4days5.csv", header = TRUE)

dat <- dat[nrow(dat):1,]
newData <- newData[nrow(newData):1,]

set.seed(30)

## Features selected after the feature selection
# Generating a list with the name of all features
vars = colnames(dat)
Ycolumn <- "Indicator"
# Removing the Y column from the features
vars <- setdiff(vars, Ycolumn)

# Here you are slicing the columns
# alldata <- dat[, vars];
alldata <- dat
y <- alldata[Ycolumn];

# ALternative
#y <- dat$Indicator;

##remove NA and data cleaning #FLAG
alldata_new <- na.omit(alldata);
data<- alldata_new
rm(alldata_new)

aux1 <- na.omit(newData);
cleanNewData <- aux1
rm(aux1)

## Factor levels

## Factor levels
# Loop through each feature
for(i in colnames(data)){
  data[, i] <- as.numeric(as.factor(data[, i]))
  cleanNewData[, i] <- as.numeric(as.factor(cleanNewData[, i]))
}

## OLD WAY
#data$Indicator2 <- ifelse(data$Indicator2 ==1, 'Decrease', 'Increase')
# Transforming the indicator feature into a factor so the Random Forest algo 
# can understand we are doing a classification problem
#data$Indicator <- as.factor(data$Indicator)

## NEW WAY
data[Ycolumn] <- ifelse(data[Ycolumn] ==1, 'Decrease', 'Increase')
# Transforming the indicator feature into a factor so the Random Forest algo 
# can understand we are doing a classification problem
data[Ycolumn] <- as.factor(data[,Ycolumn])

tab=as.table(matrix(0, nrow=2, ncol=2))
colnames(tab) <- c("Increase", "Decrease")
rownames(tab) <- c("Increase", "Decrease")

newTab=as.table(matrix(0, nrow=2, ncol=2))
colnames(newTab) <- c("Increase", "Decrease")
rownames(newTab) <- c("Increase", "Decrease")

for (i in 1:10){
  #splitting the data into training and testing
  index <- sample(1:nrow(data),round(0.10*nrow(data)))
  test <- data[index,]
  train <- data[-index,]
  
  #sample = sample.split(dat, SplitRatio = .75)
  #train = subset(dat, sample == TRUE)
  #test = subset(dat, sample == FALSE)
  
  # train_indi <- train[Ycolumn]
  # train$Indicator <- NULL
  
  formula <- as.formula(paste(Ycolumn,paste(vars,collapse=' + '),sep=' ~ '))
  
  #####################Grid Search
  #control <- trainControl(method="repeatedcv", number=3, repeats=1, search="grid")
  #set.seed(21)
  #tunegrid <- expand.grid(.mtry=c(1:10))
  #rf_gridsearch <- train(formula, data=train, method="rf", tuneGrid=tunegrid, trControl=control)
  #print(rf_gridsearch)
  #plot(rf_gridsearch)
  
  ######################################################################
  #################### Random Forest classifier
  model.forest <- randomForest(formula, data = train, ntree = 500)
  # With the GS parameters
  #model.forest <- randomForest(formula, data = train, ntree = 500, mtry=rf_gridsearch$bestTune$mtry)
  
  # View the forest results.
  print(model.forest) 
  
  # Importance of each predictor.
  # print(importance(fit,type = 2)) 
  
  
  #plot(model)
  ps <- predict(model.forest, test[,vars])
  
  tab1 <- table(ps,test[,Ycolumn])
  tab <- tab +tab1
  print ("Table")
  print (tab1)
  
  
  newPs <- predict(model.forest, cleanNewData[,vars])
  
  newTab1 <- table(newPs,cleanNewData[,Ycolumn])
  newTab <- newTab +newTab1
  print ("Table")
  print (newTab)
  
}

print("Test Confusion Matrix")
confusionMatrix(tab)

print("New Data Confusion Matrix")

confusionMatrix(newTab)

