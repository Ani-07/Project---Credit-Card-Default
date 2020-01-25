# FE 582 - Project R Code - Data Analysis

# Project Title - Prediction of Credit Card Default

# Import Data

data <- read.csv("UCI_Credit_Card.csv")

summary(data)

# Data Cleaning Process

# Change to Categorical Variables to Factors

data$SEX <- as.factor(data$SEX)
data$default.payment.next.month <- as.factor(data$default.payment.next.month)

# Education

# Categories 0, 5 and 6 under Education 5 and 6 do not add any additional value 
# and may be merged into Category 4.

summary(data$EDUCATION)

data$EDUCATION[data$EDUCATION==0] <- 4
data$EDUCATION[data$EDUCATION==5] <- 4
data$EDUCATION[data$EDUCATION==6] <- 4     

data$EDUCATION <- as.factor(data$EDUCATION)

summary(data$EDUCATION)

# Marriage

# Similar to education, 0 category is not defined for marriage and will be transferred
# 3

summary(data$MARRIAGE)

data$MARRIAGE[data$MARRIAGE == 0] <- 3

data$MARRIAGE <- as.factor(data$MARRIAGE)

summary(data$MARRIAGE)

# Age 

# Age as a contiunous variable may not add a lot of value and therefore, we
# can convert age into a categorical variable based on age categories.

breaks <- c(0, 22, 25, 30, 35, 40, 60, 108)
labels <- c("<22", "22-24", "25-29", "30-34", "35-40", "40-60", ">60")

data$age_group<-cut(data$AGE,
                 breaks = breaks,
                 labels = labels,
                 right=F,
                 ordered_result = T)

summary(data$age_group)

# Data Transformation Process

# We shall create a new variable - payment proportion being the ratio of
# the amount paid versus the bill statement

# We would need to ensure that the proportion is compared between the 
# right months. Thus Pay 1 will be compared to Bill 2 and so on.

# We write a function to create the proportion

prop.creat <- function(dataframe,x,y,z){
  prop <- NULL
  for (i in 1:nrow(dataframe)){
    if(dataframe[i,x] == 0){
      tmp <- 1
    }else if(dataframe[i,y] == 0){
      tmp <- 0
    }else{
      tmp <- round(dataframe[i,x]/dataframe[i,y], 2)
    }
    prop <- c(prop,tmp)
  }
  dataframe <- cbind(dataframe,prop)
  colnames(dataframe)[(ncol(dataframe))] <- paste("prop",z, sep = ".")
  return(dataframe)
}

names(data)

data <- prop.creat(data,14,19,1)
data <- prop.creat(data,15,20,2)
data <- prop.creat(data,16,21,3)
data <- prop.creat(data,17,22,4)
data <- prop.creat(data,18,23,5)

# We may now observe that we have created 5 new variables based on proportion

names(data)

# We shall now categorize customers based on trends in payment proportion.

names(data)

for (j in 1:(nrow(data))) {
  diff.vec <- vector()
  for (i in 27:31) {
    diff.tmp <- as.numeric(data[j,i]) - as.numeric(data[j,(i+1)])
    diff.vec <- c(diff.vec,diff.tmp)
  }
  if (all(diff.vec > 0)) {
    data$pay.trend.p[j] <- 5
  }else if (all(diff.vec < 0)){
    data$pay.trend.p[j] <- 1
  }else if (all(diff.vec == 0)){
    data$pay.trend.p[j] <- 0
  }else if (sum(diff.vec > 0) > sum(diff.vec < 0)){
    data$pay.trend.p[j] <- 4
  }else if (sum(diff.vec > 0) == sum(diff.vec < 0)){
    data$pay.trend.p[j] <- 3
  }else if (sum(diff.vec > 0) < sum(diff.vec < 0)){
    data$pay.trend.p[j] <- 2
  }
}

# We thus create a new variable called 'pay.trend.p'

data$pay.trend.p <- as.factor(data$pay.trend.p)
summary(data$pay.trend.p)

# We can also observe that we have default status of previous months
# as well. Using this we can again classify customers based on 
# trends in default

for (j in 1:(nrow(data))) {
  diff.vec <- vector()
  for (i in 7:11) {
    diff.tmp <- as.numeric(data[j,i] - data[j,(i+1)])
    diff.vec <- c(diff.vec,diff.tmp)
  }
  if (all(diff.vec > 0)) {
    data$def.trend[j] <- 5
  }else if (all(diff.vec < 0)){
    data$def.trend[j] <- 1
  }else if (all(diff.vec == 0)){
    data$def.trend[j] <- 0
  }else if (sum(diff.vec > 0) > sum(diff.vec < 0)){
    data$def.trend[j] <- 4
  }else if (sum(diff.vec > 0) == sum(diff.vec < 0)){
    data$def.trend[j] <- 3
  }else if (sum(diff.vec > 0) < sum(diff.vec < 0)){
    data$def.trend[j] <- 2
  }
}

summary(data$def.trend)

data$def.trend <- as.factor(data$def.trend)
summary(data$def.trend)

# We shall now save the transformed data as a separate file for easy access 

save(data, file = "Credit Card Data (Transformed)")

#load("Credit Card (Transformed)")

# Now that we have cleaned and transformed our data. We shall go ahead with the data analysis.

# Plotting the relationship of various variables with default

# LIMIT BAL

library(ggplot2)

ggplot(data, aes(x = data$default.payment.next.month, y = data$LIMIT_BAL, fill = "green"))+geom_boxplot() + 
  xlab("Default Status") + ylab("Limit Balance")

# For plotting the other categorical variables we shall compare the percentage of default under each
# category

# Function for calculation of percentage

per.cal <- function(dataframe, xcol, ycol){
  tmp <- table(dataframe[,xcol],dataframe[,ycol])
  out.list <- vector()
  for(i in 1:(length(tmp)/2)){
    percen <- tmp[(i+(length(tmp)/2))]/(tmp[i]+tmp[(i+(length(tmp)/2))])
    out.list <- c(out.list,percen)
  }
  return(out.list)
}

#Sex

sex.percent <- per.cal(data, 3, 25)
names(sex.percent) <- c("male", "female")
par <- colorRampPalette(colors = c("lightblue", "blue"))
barplot(sex.percent,  xlab = "sex", ylab = "default payment", col = c("grey", "grey"))

# Education

education.percent <- per.cal(data, 4, 25)
names(education.percent) <- c("1", "2", "3", "4")
barplot(education.percent,  xlab = "education_group", ylab = "default payment", col = c("lightblue"))

# Marriage

marriage.percent <- per.cal(data, 5, 25)
names(marriage.percent) <- c("Married", "Single", "Others")
barplot(marriage.percent,  xlab = "Marital Status", ylab = "default payment", col = c("darkgreen"))

barplot(marriage.percent,  xlab = "Marital Status", ylab = "Default Percentage", col = c("lightblue"), ylim = c(0,0.3))


# Age group

age.percent <- per.cal(data, 26, 25)
names(age.percent) <- labels
barplot(age.percent,  xlab = "age_group", ylab = "default payment", col = c("grey"))

# LIMIT BAL AND SEX

ggplot(data,aes(x = as.factor(default.payment.next.month), y = LIMIT_BAL, fill = SEX))+geom_boxplot()+
  xlab("Default Status")+ ylab("Limit Balance")


# Default Trend
summary(data$def.trend)

def.trend.percent <- per.cal(data, 33, 25)
names(def.trend.percent) <- c("No Trend","Majorly Positive","Mixed","Majorly Negative","Negative")
barplot(def.trend.percent,  xlab = "Default Trend", ylab = "default payment", col = c("lightgreen"))

# Payment Proportion Trend
names(data)
summary(data$pay.trend.p)

table(data$pay.trend.p,data$default.payment.next.month)

pay.trend.percent <- per.cal(data, 32, 25)
names(pay.trend.percent) <- c(1,2,3,4,5)
barplot(pay.trend.percent,  xlab = "Payment Proportion Category", ylab = "default payment", col = c("lightblue"))

# We can observe from the above plots that the variables other than payment trend seem
# to have an impact on default. We will use logistic regression to identify which
# of these variables are the most significant

# Logistic Regression

names(data)

# First we take all the demographic variables

model1 <- glm(default.payment.next.month ~ SEX + MARRIAGE + EDUCATION + age_group + def.trend, 
              data = data, family = binomial)

summary(model1)

# Marriage, Education and Defualt trend seem to have very important variable

# Test all proportion factors to see if they are important

model2 <- glm(default.payment.next.month ~ prop.1 + prop.2 + prop.3 + prop.4 + prop.5, 
              data = data, family = binomial)

summary(model2)

# Proportion 2 and proportion 1 seem to have the statistically significant variables

model3 <- glm(default.payment.next.month ~ PAY_0 + PAY_2 + PAY_3 + PAY_4, 
              data = data, family = binomial)

summary(model3)

# Default status as of 0,1,2,3 seem to be the most important variables

# Model Training

# We shall first split the data into training and testing

data.train <- data[c(1:(.8*nrow(data))),]

data.test <- data[c(((.8*nrow(data))+1):30000),]

# Now let us undertake a step wise model with all variables

null.model <- glm(default.payment.next.month ~ 1, data = data.train, family = binomial)

full.model.form <- default.payment.next.month ~ LIMIT_BAL + EDUCATION + SEX + MARRIAGE +
  age_group + def.trend + pay.trend.p + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + 
  prop.1 + prop.2 + prop.3 + prop.4 + prop.5 + BILL_AMT1+   + BILL_AMT2  + BILL_AMT3 +
  BILL_AMT4  + BILL_AMT5  + BILL_AMT6 + PAY_AMT1 + + PAY_AMT2 + PAY_AMT3 + + PAY_AMT4 +
  PAY_AMT5 + + PAY_AMT6

model4 <- step(null.model, full.model.form, direction = "both")

summary(model4)

# As per the stepwise model, the statistically significant variables are def.trend,
# Limit Balance, payment 1,2,3, Education, Bill amount 1, Sex, default status 0,2.

logistic.pred <- predict.glm(model4,newdata = data.test, type = "response")

logistic.pred[logistic.pred > 0.5] <- 1
logistic.pred[logistic.pred < 0.5] <- 0

table(True_Class=data.test$default.payment.next.month,Predictions = logistic.pred)

log.accuracy <- 1 - sum(logistic.pred != data.test$default.payment.next.month)/nrow(data.test)

log.accuracy

log.recall <- 326/(326+940)
  
# Classification Tree

library(tree)

tree.model = tree(default.payment.next.month ~ ., data.train)
summary(tree.model)
plot(tree.model)
text(tree.model,pretty=0)

tree.predict <- predict(tree.model, newdata = data.test, type = "class")

table(True_Class=data.test$default.payment.next.month,Predictions = tree.predict)

tree.accuracy <- 1 - sum(tree.predict != data.test$default.payment.next.month)/nrow(data.test)
tree.accuracy

tree.recall <- 412/(412+854)
tree.recall

# Random Forest

library(randomForest)

data.rf = randomForest(default.payment.next.month ~ ., data = data.train,mtry = 4,
                       importance=TRUE)

random.f.pred = predict(data.rf,newdata=data.test)

table(True_Class=data.test$default.payment.next.month,Predictions = random.f.pred)

rf.accuracy <- 1 - sum(random.f.pred != data.test$default.payment.next.month)/nrow(data.test)
rf.accuracy

rf.recall <- 469/(469+797)
rf.recall

# lda

library(MASS)

lda.model = lda(default.payment.next.month ~., data = data.train)
lda.pred <- predict(lda.model,data.test)
LDA.predict <- lda.pred$class

table(True_Class=data.test$default.payment.next.month,Predictions = LDA.predict)

lda.accuracy <- 1 - sum(LDA.predict != data.test$default.payment.next.month)/nrow(data.test)
lda.accuracy

lda.recall <- 375/(326+891)
lda.recall

#QDA

qda.model = qda(default.payment.next.month ~., data = data.train)
qda.pred <- predict(qda.model,data.test)
QDA.predict <- qda.pred$class

table(True_Class=data.test$default.payment.next.month,Predictions = QDA.predict)

qda.accuracy <- 1 - sum(QDA.predict != data.test$default.payment.next.month)/nrow(data.test)
qda.accuracy

qda.recall <- 855/(855+411)
qda.recall
