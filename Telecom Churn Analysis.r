library(plyr)  
library(rpart.plot) 
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(ggplot2)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

churn <- read.csv("TelcoChurnData.csv")
glimpse(churn)

sapply(churn, function(x) sum(is.na(x)))

sum(is.na(churn$TotalCharges))/nrow(churn)

churn_clean <- churn[complete.cases(churn),]

churn_clean$SeniorCitizen <- as.factor(mapvalues(churn_clean$SeniorCitizen,from=c("0","1"),to=c("No", "Yes")))

churn_clean$MultipleLines <- as.factor(mapvalues(churn_clean$MultipleLines, from=c("No phone service"), to=c("No")))

for(i in 10:15){
  churn_clean[,i] <- as.factor(mapvalues(churn_clean[,i],from= c("No internet service"), to= c("No")))
}
churn_clean$customerID <- NULL


#Gender plot
p1 <- ggplot(churn_clean, aes(x = gender)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Senior citizen plot
p2 <- ggplot(churn_clean, aes(x = SeniorCitizen)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Partner plot
p3 <- ggplot(churn_clean, aes(x = Partner)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Dependents plot
p4 <- ggplot(churn_clean, aes(x = Dependents)) +geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), 
  stat = 'count', position = position_dodge(.1), size = 3)

#Plot demographic data within a grid
grid.arrange(p1, p2, p3, p4, ncol=2)


# Question1 ->

#How many females are there who are Senior citizen
df2 <- filter(churn_clean,SeniorCitizen=="Yes" & gender=="Female")
print(nrow(df2))

#How many females are dependent and Senior Citizen
df2 <- filter(churn_clean,SeniorCitizen=="Yes" & gender=="Female" & Dependents=="Yes")
print(nrow(df2))

#How many males are dependent and Senior Citizen
df2 <- filter(churn_clean,SeniorCitizen=="Yes" & gender=="Male" & Dependents=="Yes")
print(nrow(df2))

#How many Senior Citizens are dependent
df2 <- filter(churn_clean,SeniorCitizen=="No" & Dependents=="Yes")
print(nrow(df2))

df2 <- filter(churn_clean,gender=="Female" & Partner=="Yes")
print(nrow(df2))

#What is The churn rate of Female Senior Citizens
d1<- filter(churn_clean,SeniorCitizen=="Yes" & gender=="Female")
N1 <- nrow(d1)
d2 <- filter(d1,Churn == "Yes")
N2 <- nrow(d2)
print(N2/N1)

#Question2 ->

#Ist Part
# H0: The Churn is independent of gender
# H1: The Churn is dependent on gender
print(chisq.test(table(churn_clean$gender,churn_clean$Churn)))

# Hence proved there is no difference in churn in different genders

#2nd Part
# H0: The Senior Citizen is independent of Senior Citizen
# H1: The Senior Citizen is dependent on Senior Citizen

print(chisq.test(table(churn_clean$SeniorCitizen,churn_clean$Churn)))
# Hence proved there is a difference in churn in different Senior Citizens

#3rd Part

d1 <- filter(churn_clean,Dependents=="Yes")
d2 <- filter(churn_clean,Dependents=="No")

d3 <- filter(d1,Churn=="No")
d4 <- filter(d2,Churn=="No")

prop.test(c(nrow(d3),nrow(d4)),c(nrow(d1),nrow(d2)),alternative="greater")

#Second Part
#Phone service plot
p5 <- ggplot(churn_clean, aes(x = PhoneService)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Multiple phone lines plot
p6 <- ggplot(churn_clean, aes(x = MultipleLines)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Internet service plot
p7 <- ggplot(churn_clean, aes(x = InternetService)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online security service plot
p8 <- ggplot(churn_clean, aes(x = OnlineSecurity)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online backup service plot
p9 <- ggplot(churn_clean, aes(x = OnlineBackup)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Device Protection service plot
p10 <- ggplot(churn_clean, aes(x = DeviceProtection)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Tech Support service plot
p11 <- ggplot(churn_clean, aes(x = TechSupport)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Streaming TV service plot
p12 <- ggplot(churn_clean, aes(x = StreamingTV)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Streaming Movies service plot
p13 <- ggplot(churn_clean, aes(x = StreamingMovies)) +geom_bar(aes(fill = Churn)) +geom_text(aes(y = ..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot service data within a grid
grid.arrange(p5, p6, p7,p8, p9, p10,p11, p12, p13,ncol=3)

#Question 3->

# How many People have both Phone Service and Internet Service
df<-filter(churn_clean,InternetService=="DSL" | InternetService=="Fiber optic" & PhoneService=="Yes")
print(nrow(df))

# Customer opted for only one service

df<-filter(churn_clean,InternetService=="No" & PhoneService=="Yes")
print(nrow(df))

df<-filter(churn_clean,InternetService=="DSL" | InternetService=="Fiber optic" & PhoneService=="No")
print(nrow(df))

# How much People have who Opted for DSL service
print(nrow(filter(churn_clean,InternetService=="DSL")))

# How much People pay who Opted for Fiber optics
print(nrow(filter(churn_clean,InternetService=="Fiber optic")))

# Is Churning rate of People with DSL Service < Churning rate of People with Fiber optic
cr1 <- (nrow(filter(churn_clean,InternetService=="Fiber optic" & Churn=="Yes")))/nrow(filter(churn_clean,InternetService=="Fiber optic"))
cr2 <- (nrow(filter(churn_clean,InternetService=="DSL" & Churn=="Yes")))/nrow(filter(churn_clean,InternetService=="DSL"))
print(cr2 < cr1)


#Question 4->

# H0: Is churning rate of people with multiple Lines = Churning rate of people with no multiple lines
# H1: Is churning rate of people with multiple Lines < Churning rate of people with no multiple lines

t1 <- table(churn_clean$Churn,churn_clean$MultipleLines)
tab_values <- as.numeric(t1)    # Extract values

N1 <- tab_values[2]+tab_values[1]
N2 <- tab_values[3]+tab_values[4]

prop.test(c(tab_values[4],tab_values[1]),c(N2,N1),alternative="less")


#Third Part
#Contract status plot
p14 <- ggplot(churn_clean, aes(x = Contract)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Paperless billing plot
p15 <- ggplot(churn_clean, aes(x = PaperlessBilling)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Payment method plot
p16 <- ggplot(churn_clean, aes(x = PaymentMethod)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot contract data within a grid
grid.arrange(p14, p15, p16, ncol=1)

# Question 5->
# Is paperless billing dependent on Payment Method ?

#H0: Paperless billing is independent of Payment method
#H1: PaperLess billing is dependent on Payment Method

print(chisq.test(table(churn_clean$PaymentMethod,churn_clean$PaperlessBilling)))

c1 <- nrow(filter(churn_clean,PaymentMethod == 'Credit card (automatic)' & Churn=="Yes"))/nrow(filter(churn_clean,PaymentMethod == 'Credit card (automatic)'))
c2 <- nrow(filter(churn_clean,PaymentMethod == 'Bank transfer (automatic)' & Churn=="Yes"))/nrow(filter(churn_clean,PaymentMethod == 'Bank transfer (automatic)'))
c3 <- nrow(filter(churn_clean,PaymentMethod == 'Electronic check' & Churn=="Yes"))/nrow(filter(churn_clean,PaymentMethod == 'Electronic check'))
c4 <- nrow(filter(churn_clean,PaymentMethod == 'Mailed check' & Churn=="Yes"))/nrow(filter(churn_clean,PaymentMethod == 'Mailed check'))

maxrate <- which.max(c(c1,c2,c3,c4))
print(maxrate)

#Tenure histogram
p17 <- ggplot(data = churn_clean, aes(tenure, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Monthly charges histogram
p18 <- ggplot(data = churn_clean, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Total charges histogram
p19 <- ggplot(data = churn_clean, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)

#Plot quantitative data within a grid
grid.arrange(p17, p18, p19, ncol=1)


#Question 6 -> 

#Part1: Is tenure really dependent on the contract ?
ptc<-ggplot(churn_clean, aes(x = tenure, fill = Contract)) +geom_density(alpha = .3)
ptc

bartlett.test(tenure~Contract, data=churn_clean)

ggplot(data=churn_clean, mapping = aes(x = Contract, y = tenure)) + geom_boxplot() +theme_bw()

oneway.test(tenure~Churn,data=churn_clean, var.equal = FALSE)

result <- aov(tenure ~ Contract,data=churn_clean)
summary(result)

# from the test  we can conclude that
# The larger the F value, the more 
# likely it is that the variation caused by the 
# independent variable is real and not due to chance.
# Similarly in our case the f value is quite high and p value is low which implies that the groups have unequal mean and is highly dependent on the 
# contract otherwise f this was not true then they may have same means

qqnorm(churn_clean$MonthlyCharges, col="blue")
qqline(churn_clean$MonthlyCharges, col="red")

qqnorm(churn_clean$TotalCharges, col="blue")
qqline(churn_clean$TotalCharges, col="red")

#Is total Charges dependent on the Payment Method ?
bartlett.test(TotalCharges~PaymentMethod, data=churn_clean)

oneway.test(TotalCharges~PaymentMethod,data=churn_clean, var.equal = FALSE)

ptc<-ggplot(churn_clean, aes(x = TotalCharges, fill = PaymentMethod)) +geom_density(alpha = .3)
ptc

#Is Churn dependent on the Monthly Charges ?
var.test(MonthlyCharges ~ Churn, churn_clean, alternative = "two.sided")
result <- t.test(MonthlyCharges~Churn,data=churn_clean)
print(result)


#Question 7-> Are the tenure, TotalCharges and MonthyCharges related?
p20 <- ggplot(churn_clean, aes(x = Churn)) +
  geom_bar(aes(fill = Churn)) +
  geom_text(aes(y = ..count.. -200, 
  label = paste0(round(prop.table(..count..),4) * 100, '%')), 
  stat = 'count', position = position_dodge(.1), size = 3)
p20


churn_clean %>%  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%cor() %>%corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)

set.seed(56)
split_train_test <- createDataPartition(churn_clean$Churn,p=0.7,list=FALSE)
dtrain<- churn_clean[split_train_test,]
dtest<-  churn_clean[-split_train_test,]

# Remove Total Charges from the training dataset

dtrain <- dtrain[,-19]
dtest <- dtest[,-19]

# Question 8 - Decision Tree on our dataset and check accuracy ?

tr_fit <- rpart(Churn ~., data = dtrain, method="class")
rpart.plot(tr_fit)
summary(tr_fit)

printcp(tr_fit)
plotcp(tr_fit)

tr_prob1 <- predict(tr_fit, dtest)
tr_pred1 <- ifelse(tr_prob1[,2] > 0.5,"Yes","No")
table(Predicted = tr_pred1, Actual = dtest$Churn)

tr_prob2 <- predict(tr_fit, dtrain)
tr_pred2 <- ifelse(tr_prob2[,2] > 0.5,"Yes","No")
tr_tab1 <- table(Predicted = tr_pred2, Actual = dtrain$Churn)
tr_tab2 <- table(Predicted = tr_pred1, Actual = dtest$Churn)

# Train
confusionMatrix(
  as.factor(tr_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(tr_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

tr_acc <- sum(diag(tr_tab2))/sum(tr_tab2)
tr_acc


# Question 9 - Random Forest on our dataset and check accuracy ?
#Set control parameters for random forest model selection

#ctrl <- trainControl(method = "cv", number=5, classProbs = TRUE, summaryFunction = twoClassSummary)

#Exploratory random forest model selection
# rf_fit1 <- train(Churn ~., data = dtrain,
#                  method = "rf",
#                  ntree = 75,
#                  tuneLength = 5,
#                  metric = "ROC",
#                  trControl = ctrl)


rf_fit2 <- randomForest(as.factor(Churn) ~., data = dtrain, ntree = 75, mtry = 2, importance = TRUE, proximity = TRUE)

importance(rf_fit2)
varImpPlot(rf_fit2)

#Display variable importance from random tree
varImpPlot(rf_fit2, sort=T, n.var = 10, main = 'Top 10 important variables')

rf_pred1 <- predict(rf_fit2, dtest)
rf_tab2 <- table(Predicted = rf_pred1, Actual = dtest$Churn)


# Test
confusionMatrix(
  as.factor(rf_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

rf_acc <- sum(diag(rf_tab2))/sum(rf_tab2)
rf_acc

pred1=predict(rf_fit2,dtest,type = "prob")
library(ROCR)
perf = prediction(pred1[,2], dtest$Churn)
auc = performance(perf, "auc")
pred3 = performance(perf, "tpr","fpr")
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# Question 10 - Logistic Regression on our dataset and check accuracy ?

lr_fit <- glm(as.factor(Churn) ~., data = dtrain,family=binomial(link='logit'))
summary(lr_fit)

lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = dtest$Churn)

lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$Churn)

confusionMatrix(
  as.factor(lr_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(lr_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

lr_acc <- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc


library(pROC)
# Compute roc
res.roc <- roc(dtest$Churn, lr_prob1)
plot.roc(res.roc, print.auc = TRUE)
# Comparing different models
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart <- train(Churn~., data=dtrain, method="rpart", trControl=control)
# LOR
set.seed(7)
fit.lor <- train(Churn~., data=dtrain, method="glm", trControl=control)
#RF
set.seed(7)
fit.rf <- train(Churn~., data=dtrain, method="rf", trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Churn~., data=dtrain, method="knn", trControl=control)

# collect resamples
results <- resamples(list(CART=fit.cart,RF=fit.rf, LOR=fit.lor, KNN=fit.knn))

scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")
summary(results)

