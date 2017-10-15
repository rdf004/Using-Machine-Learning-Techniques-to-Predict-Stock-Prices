library("e1071")
library(caret)

dat <- read.csv("/Users/Roshan/Desktop/Machine Learning/Cintas Data/Cintas Stock Trailing Replaced Indicators.csv", header = TRUE)

## Features slected after the feature selection
vars <- c("X50.calendar.day.moving.average","assetnew", "betanew", "CTAS.Enterprise.Value..In.Millions..Basically.the.first.value.is.11.5.B.",
          "EBITDA_mn" , "reinvestednew" , "Close..Nasdaq.", "Open..Nasdaq.", "Price.of.Oil",
          "returnnew", "X50.Day.Simple.Moving.Average..S.P.500.", "tobin_mn","zminew",
          "PE.Ratio","Indicator")
Ycolumn <- "Indicator"

alldata <- dat[, vars];
y <- alldata$Indicator;

#remove NA and data cleaning
alldata_new <- na.omit(alldata);
data<- alldata_new
rm(alldata_new)

## Factor levels

data$asset_matched<- as.numeric(as.factor(data$assetnew))
data$EBITDA_matched<- as.numeric(as.factor(data$EBITDA_mn))
data$invested_matched<- as.numeric(as.factor(data$reinvestednew))
data$return_matched<- as.numeric(as.factor(data$returnnew))
data$tobin_matched<- as.numeric(as.factor(data$tobin_mn))
data$zmi_matched<- as.numeric(as.factor(data$zminew))
data$Indicator<- as.numeric(as.factor(data$Indicator))
##
##splitting the data into training and testing
index <- sample(1:nrow(data),round(0.90*nrow(data)))
train <- data[index,]
test <- data[-index,]

# train_indi <- train$Indicator
# train$Indicator <- NULL

formula <- as.formula(paste(Ycolumn,paste(vars[1:14],collapse=' + '),sep=' ~ '))


## tune `svm' for classification with RBF-kernel (default in svm),
## using one split for training/validation set
## GRID SEARCH
obj <- tune.svm(as.formula(formula), data=train, gamma = 2^(-1:1), cost = 2^(2:4))
summary(obj)
plot(obj)


## Choosing parameter after the parameter tuning(Grid Search )
svm_model <- svm(as.formula(formula), data=train, gamma = 0.5, cost = 16)
summary(svm_model)

#--------------------------------------------------------------------
# Confusion matrix
#--------------------------------------------------------------------

test1 <- test[,vars[1:14]]
# na.omit(test1) 
svm.pred <- predict(svm_model, newdata = test1)
svm.pred <- round(svm.pred,0)
tab <- table(svm.pred,test$Indicator)
confusionMatrix(tab)

