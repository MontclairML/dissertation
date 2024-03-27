library(tidyverse)
data<-read.csv("2022_OPM_FEVS_PRDF.csv")

data<-data%>%select(1:106, 115)
data<-data%>%select(-2)

data[data[1:106]=="X"]<-NA
data[data[1:106]=="Y"]<-NA
data[data[1:106]=="A"]<-1
data[data[1:106]=="B"]<-0
data[data[1:106]=="C"]<-0
data[data[1:106]=="D"]<-0

data[2:106]<- mutate_all(data[2:106], function(x) as.numeric(as.character(x)))


# Identify numeric columns
numeric_columns <- sapply(data, is.numeric)


# Median imputation for each numeric column
for (col_name in names(numeric_columns)[numeric_columns]) {
  # Compute the median of the column, ignoring NA values
  median_value <- median(data[[col_name]], na.rm = TRUE)
  
  # Replace NA values in the column with the median
  data[[col_name]][is.na(data[[col_name]])] <- median_value
}

data$DLEAVING<-as.factor(as.character(data$DLEAVING))
data<- data%>%select(1:Q14, Q16:DLEAVING)
data$workXP<-data%>%select(Q1:Q13)%>%rowMeans()
data$workUnit<-data%>%select(Q14:Q24)%>%rowMeans()
data$workOffice<-data%>%select(Q25:Q34)%>%rowMeans()
data$workOrg<-data%>%select(Q35:Q44)%>%rowMeans()
data$workSuper<-data%>%select(Q45:Q54)%>%rowMeans()
data$leadership<-data%>%select(Q55:Q64)%>%rowMeans()
data$workSat<-data%>%select(Q65:Q70)%>%rowMeans()
data$DEI<-data%>%select(Q71:Q84)%>%rowMeans()
data$employeeXP<-data%>%select(Q85:Q89)%>%rowMeans()
data$flexibility<-data%>%select(Q90:Q99)%>%rowMeans()

data2<-data%>%select(DLEAVING:flexibility) 
write.csv(data2, "data_cleaned.csv", row.names=FALSE)


#####################################################################################



library(caret)
train.control <- trainControl(method = "cv", 
                              number = 5,verboseIter = TRUE)
metric="Accuracy"
train_data<-data%>%select(DLEAVING:flexibility)

# Split the data into training and testing sets
# set.seed(123) # For reproducibility
# data_split <- initial_split(data, prop = 0.8)
# train_data <- training(data_split)
# test_data <- testing(data_split)
# Train the model
set.seed(9)
model.lm <- train(DLEAVING ~., data = train_data, method = "lm", preProcess = c("medianImpute","center","scale"), metric=metric,
                  trControl = train.control)
summary(model.lm)

set.seed(7)
model.nn<-train(DLEAVING~.,data=train_data,tuneGrid=expand.grid(alpha=0:1,lambda=seq(0.0001,1,length=20)),
                    method="glmnet", metric=metric, trControl=train.control)

set.seed(6)
model.rf<-train(DLEAVING ~., tuneLength=10, data = train_data, method = "ranger", metric=metric,
                trControl = train.control)
summary(model.rf)
set.seed(4)
model.svm<-train(DLEAVE ~., data = train_data, method = "svmRadial", metric=metric,
                 trControl = train.control)
summary(model.svm)
set.seed(2)
model.gbm<-train(DLEAVE ~., data = train_data, method = "gbm", metric=metric,
                 trControl = train.control)
summary(model.gbm)
set.seed(8)
model.xgboost<-train(DLEAVE ~., data = train_data, method = "xgbLinear", metric=metric,
                     trControl = train.control)

# Summarize the results
results <- resamples(list(lm=model.lm,glmnet=model.glmnet,svm=model.svm,rf=model.rf,gbm=model.gbm,xgboost=model.xgboost))
summary(results)
summary(model)