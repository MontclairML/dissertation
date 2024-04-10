library(haven)
library(tidyverse)
data <- read_sav("Diego - Dissertation Survey_April 5, 2024_13.26.sav")
data$TI[data$TI==21]<-0
data$TI[data$TI==22]<-1

data<-data%>%filter(JIMS_10>4)
data<-data%>%filter(ACS_9==1)


data$JIMS_5<-abs((data$JIMS_5)-6)
data$JIMS_7<-abs((data$JIMS_7)-6)
data$JIMS_9<-abs((data$JIMS_9)-6)
data$SPOS_2<-abs((data$SPOS_2)-6)
data$SPOS_3<-abs((data$SPOS_3)-6)
data$SPOS_5<-abs((data$SPOS_5)-6)
data$BFI_1<-abs((data$BFI_1)-6)
data$BFI_2<-abs((data$BFI_2)-6)
data$BFI_6<-abs((data$BFI_6)-6)
data$ACS_4<-abs((data$ACS_4)-6)
data$ACS_5<-abs((data$ACS_5)-6)
data$ACS_6<-abs((data$ACS_6)-6)
data$NCS_2<-abs((data$NCS_2)-6)
data$NCS_3<-abs((data$NCS_3)-6)
data$NCS_8<-abs((data$NCS_8)-6)
data$MOAQ_1<-abs((data$MOAQ_1)-6)

data$EES<-data%>%select(EES_1:EES_3)%>%rowMeans()
data$JIMS<-data%>%select(JIMS_1:JIMS_10)%>%rowMeans()
data$SPOS<-data%>%select(SPOS_1:SPOS_6)%>%rowMeans()
data$BFI<-data%>%select(BFI_1:BFI_6)%>%rowMeans()
data$ACS<-data%>%select(ACS_1:ACS_9)%>%rowMeans()
data$NCS<-data%>%select(NCS_1:NCS_8)%>%rowMeans()
data$MOAQ<-data%>%select(MOAQ_1:MOAQ_3)%>%rowMeans()
data$BIO<-data%>%select(BIO1:BIO13)%>%rowMeans()

data_train<-data%>%select(TI, EES, JIMS, SPOS, BFI, ACS, NCS, MOAQ, BIO, GSOI)
# Identify numeric columns
numeric_columns <- sapply(data_train, is.numeric)


# Median imputation for each numeric column
for (col_name in names(numeric_columns)[numeric_columns]) {
  # Compute the median of the column, ignoring NA values
  median_value <- median(data_train[[col_name]], na.rm = TRUE)
  
  # Replace NA values in the column with the median
  data_train[[col_name]][is.na(data_train[[col_name]])] <- median_value
}


######################### ml ########################################################

library(caret)
train.control <- trainControl(method = "cv", 
                              number = 5,verboseIter = TRUE, classProbs = TRUE)
metric="ROC"


# Split the data into training and testing sets
# set.seed(123) # For reproducibility
# data_split <- initial_split(data, prop = 0.8)
# train_data <- training(data_split)
# test_data <- testing(data_split)
# Train the model

data_train$TI<-as.numeric(data_train$TI)
data_train$GSOI<-as.numeric(data_train$GSOI)
data_train$TI<-as.factor(as.character(data_train$TI))
write.csv(data_train, "data_study2.csv", row.names=FALSE)

set.seed(45)
model.lr<-glm(TI~., data=data_train)
summary(model.lr)

set.seed(7)
model.nn<-train(TI~.,data=data_train,tuneGrid=expand.grid(alpha=0:1,lambda=seq(0.0001,1,length=20)),
                method="glmnet", metric=metric, trControl=train.control)

set.seed(6)
model.rf<-train(TI ~., tuneLength=10, data = data_train, method = "ranger", metric=metric,
                trControl = train.control)
summary(model.rf)
set.seed(4)
model.svm<-train(TI ~., data = data_train, method = "svmRadial", metric=metric,
                 trControl = train.control)
summary(model.svm)
set.seed(2)
model.gbm<-train(TI ~., data = data_train, method = "gbm", metric=metric,
                 trControl = train.control)
summary(model.gbm)
set.seed(8)
model.xgboost<-train(TI ~., data = data_train, method = "xgbLinear", metric=metric,
                     trControl = train.control)

# Summarize the results
results <- resamples(list(lm=model.lm,glmnet=model.glmnet,svm=model.svm,rf=model.rf,gbm=model.gbm,xgboost=model.xgboost))
summary(results)