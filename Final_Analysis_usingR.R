#Import data
SGH_data <- data.frame(read_excel("SGH data.xls"))

#View(SGH_data)
head(SGH_data,5)
tail(SGH_data,5)
str(SGH_data)
summary(SGH_data)


#Re-arranging the dataset in to a more analyzable form
SGH_data[SGH_data$G == 0,]$G <-'F'
SGH_data[SGH_data$G == 1,]$G <-'M'

SGH_data$outcome <- ifelse(SGH_data$Died == -999,
                           "0", "1") #0 - survives and 1 - dies
#30-day post-operative mortality
SGH_data$ThirtyDayMortality <- ifelse((SGH_data$outcome
                                       == 1 & SGH_data$Died < 31), "1", "0")

Year <- as.numeric(format(as.Date(SGH_data$OP_DATE,
                                  "%Y-%m-%d"),"%Y"))
#Year
SGH_data$OP_DATE <- Year

#Descriptive STATS for the data set
SGH_data <- select(SGH_data,Hospital, OP_DATE, 
                   G, age, Parsonnet, Died, outcome,
                   ThirtyDayMortality, everything())
SGH_data[45:50,]
summary(SGH_data)

#Descriptive stats for age
SGH_data %>%
  ggplot( aes(x=age, fill=G)) +
  geom_histogram( color="white", alpha=0.6, 
                  position = 'identity') +
  scale_fill_manual(values=c("#FD625E", "#B599B8")) +
  labs(fill="Gender",title= "Histogram for Age vs Gender",
       x="Age", y="Count")

#Train data set and Test data set

#Train dataset
train <- filter(SGH_data,OP_DATE %in% c(1992,1993))
summary(train)

#Test dataset
test <- filter(SGH_data,OP_DATE %in% c(1994:1998))
summary(test)



#Further analysis

length(which(SGH_data$Died != -999)) #770 deaths occurred
all_MR <- ((length(which(SGH_data$Died != -999))))/nrow(SGH_data)*100
all_MR # mortality rate is 11.00944%
length(which(train$Died != -999)) #275 deaths occurred in
#the train dataset
length(which(SGH_data$Died != -999 & SGH_data$Died < 31)) 
#461 deaths occurred
length(which(test$Died != -999)) #495 deaths occurred in 
#the test dataset
length(which(test$Died != -999 & test$Died < 31)) 
#318 deaths occurred
overall_MR <- ((length(which(SGH_data$Died != -999 & 
                               SGH_data$Died < 31)))/nrow(SGH_data))*100
overall_MR #overall mortality rate is 6.591364%
length(which(train$Died != -999 & train$Died < 31)) 
#143 deaths occured
train_MR <- ((length(which(train$Died != -999 & train$Died < 31)
))/nrow(train))*100
train_MR #mortality rate  is 6.44725%
length(which(test$Died != -999 & test$Died < 31)) 
#318 deaths occured
test_MR <- ((length(which(test$Died != -999 & test$Died < 31)
))/nrow(test))*100
test_MR #mortality rate  is 6.6583%

#Logistic Regression model using only Parsonnet Score

library(MASS)
model <- glm(ThirtyDayMortality ~ Parsonnet, data = train,
             family = binomial) %>%
  stepAIC(trace = FALSE)
summary(model)


#Plotting fitted mortality rates for each individual surgeon

predicted.data <- data.frame(
  probability.of.outcome=logistic$fitted.values,
  Parsonnet = train$Parsonnet)
df1 <- filter(train,train$surgeon == 1)
logistic1 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df1, family = "binomial")
predicted.data1 <- data.frame(
  probability.of.outcome1=logistic1$fitted.values,
  Parsonnet1 = df1$Parsonnet)
df2 <- filter(train,train$surgeon == 2)
logistic2 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df2, family = "binomial")
predicted.data2 <- data.frame(
  probability.of.outcome2=logistic2$fitted.values,
  Parsonnet2 = df2$Parsonnet)
df3 <- filter(train,train$surgeon == 3)
logistic3 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df3, family = "binomial")
predicted.data3 <- data.frame(
  probability.of.outcome3=logistic3$fitted.values,
  Parsonnet3 = df3$Parsonnet)
df4 <- filter(train,train$surgeon == 4)
logistic4 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df4, family = "binomial")
predicted.data4 <- data.frame(
  probability.of.outcome4=logistic4$fitted.values,
  Parsonnet4 = df4$Parsonnet)
df5 <- filter(train,train$surgeon == 5)
logistic5 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df5, family = "binomial")
predicted.data5 <- data.frame(
  probability.of.outcome5=logistic5$fitted.values,
  Parsonnet5 = df5$Parsonnet)
df6 <- filter(train,train$surgeon == 6)
logistic6 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df6, family = "binomial")
predicted.data6 <- data.frame(
  probability.of.outcome6=logistic6$fitted.values,
  Parsonnet6 = df6$Parsonnet)
df7 <- filter(train,train$surgeon == 7)
logistic7 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df7, family = "binomial")
predicted.data7 <- data.frame(
  probability.of.outcome7=logistic7$fitted.values,
  Parsonnet7 = df7$Parsonnet)
## We can plot the data...
ggplot() +
  geom_line(data=predicted.data, aes(x=Parsonnet, 
                                     y=probability.of.outcome), size = 1.5) +
  geom_line(data=predicted.data1, aes(x=Parsonnet1, 
                                      y=probability.of.outcome1, col = "Surgeon 1"), size = 1) +
  geom_line(data=predicted.data2, aes(x=Parsonnet2, 
                                      y=probability.of.outcome2, col = "Surgeon 2"), size = 1) +
  geom_line(data=predicted.data3, aes(x=Parsonnet3, 
                                      y=probability.of.outcome3, col = "Surgeon 3"), size = 1) +
  geom_line(data=predicted.data5, aes(x=Parsonnet5, 
                                      y=probability.of.outcome5, col = "Surgeon 5"), size = 1) +
  geom_line(data=predicted.data6, aes(x=Parsonnet6, 
                                      y=probability.of.outcome6, col = "Surgeon 6"), size = 1) +
  geom_line(data=predicted.data7, aes(x=Parsonnet7, 
                                      y=probability.of.outcome6, col = "Surgeon 7"), size = 1) +
  xlab("Parsonnet") +
  ylab("Fitted mortality rate")


#Plotting fitted mortality rates for each different surgical procedure 

predicted.data <- data.frame(
  probability.of.outcome=logistic$fitted.values,
  Parsonnet = train$Parsonnet)
df1 <- filter(train,train$proced == 1)
logistic1 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df1, family = "binomial")
predicted.data1 <- data.frame(
  probability.of.outcome1=logistic1$fitted.values,
  Parsonnet1 = df1$Parsonnet)
df2 <- filter(train,train$proced == 2)
logistic2 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df2, family = "binomial")
predicted.data2 <- data.frame(
  probability.of.outcome2=logistic2$fitted.values,
  Parsonnet2 = df2$Parsonnet)
df3 <- filter(train,train$proced == 3)
logistic3 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df3, family = "binomial")
predicted.data3 <- data.frame(
  probability.of.outcome3=logistic3$fitted.values,
  Parsonnet3 = df3$Parsonnet)
df4 <- filter(train,train$proced == 4)
logistic4 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df4, family = "binomial")
predicted.data4 <- data.frame(
  probability.of.outcome4=logistic4$fitted.values,
  Parsonnet4 = df4$Parsonnet)
df5 <- filter(train,train$proced == 5)
logistic5 <- glm(ThirtyDayMortality ~ Parsonnet, 
                 data = df5, family = "binomial")
predicted.data5 <- data.frame(
  probability.of.outcome5=logistic5$fitted.values,
  Parsonnet5 = df5$Parsonnet)

## We can plot the data...
ggplot() +
  geom_line(data=predicted.data, aes(x=Parsonnet, 
                                     y=probability.of.outcome), size = 1.5) +
  geom_line(data=predicted.data1, aes(x=Parsonnet1, 
                                      y=probability.of.outcome1, col = "Procedure 1"), size = 1) +
  geom_line(data=predicted.data2, aes(x=Parsonnet2, 
                                      y=probability.of.outcome2, col = "Procedure 2"), size = 1) +
  geom_line(data=predicted.data3, aes(x=Parsonnet3, 
                                      y=probability.of.outcome3, col = "Procedure 3"), size = 1) +
  geom_line(data=predicted.data4, aes(x=Parsonnet4, 
                                      y=probability.of.outcome4, col = "Procedure 4"), size = 1) +
  geom_line(data=predicted.data5, aes(x=Parsonnet5, 
                                      y=probability.of.outcome5, col = "Procedure 5"), size = 1) +
  xlab("Parsonnet") +
  ylab("Fitted mortality rate")


#Assigning scores based on mortality rate and failure rate

#Using the mortality rate of the surgeons of the first 
#two years we assign a new variable called surgeon_scores, 
#values ranging from 0-10
#Depending on the failure rate of the type of surgery we
#create a new variable assigning relevant scores
train$surgeon_scores <- train$surgeon
train$surgeon_scores[train$surgeon == 1] <- 9.56
train$surgeon_scores[train$surgeon == 2] <- 9.41
train$surgeon_scores[train$surgeon == 3] <- 4.63
train$surgeon_scores[train$surgeon == 4] <- 10
train$surgeon_scores[train$surgeon == 5] <- 1.95
train$surgeon_scores[train$surgeon == 6] <- 5.06
train$surgeon_scores[train$surgeon == 7] <- 6.54

test$surgeon_scores <- test$surgeon
test$surgeon_scores[test$surgeon == 1] <- 9.56
test$surgeon_scores[test$surgeon == 2] <- 9.41
test$surgeon_scores[test$surgeon == 3] <- 4.63
test$surgeon_scores[test$surgeon == 4] <- 10
test$surgeon_scores[test$surgeon == 5] <- 1.95
test$surgeon_scores[test$surgeon == 6] <- 5.06
test$surgeon_scores[test$surgeon == 7] <- 6.54

train$surgery_scores <- train$proced
train$surgery_scores[train$proced == 1] <- 4.15
train$surgery_scores[train$proced == 2] <- 9.42
train$surgery_scores[train$proced == 3] <- 9.48
train$surgery_scores[train$proced == 4] <- 22.99
train$surgery_scores[train$proced == 5] <- 15.12

test$surgery_scores <- test$proced
test$surgery_scores[test$proced == 1] <- 4.15
test$surgery_scores[test$proced == 2] <- 9.42
test$surgery_scores[test$proced == 3] <- 9.48
test$surgery_scores[test$proced == 4] <- 22.99
test$surgery_scores[test$proced == 5] <- 15.12

head(train,5)
head(test,5)


#Logistic Regression model fitting

model <- glm(ThirtyDayMortality ~ surgeon_scores, 
             data = train, family = binomial) %>%
  stepAIC(trace = FALSE)
summary(model)

model <- glm(ThirtyDayMortality ~ surgery_scores, 
             data = train, family = binomial) %>%
  stepAIC(trace = FALSE)
summary(model)


#Risk-Adjusted CUSUM charts

#Calculating each patient's risk
p_risk <- exp(-3.679771+0.076799*test$Parsonnet)/
  (1+exp(-3.679771+0.076799*test$Parsonnet))
test$prisk <- p_risk

n <- nrow(test) #number of patients
#initial CUSUM value
X_t <- 0 #to detect increase in surgical failure rate
df_1 <- matrix(0,n,2) #create a matrix
#(These values can be changed to RA = 0.5 or 1.5 or 2)
R0 <- 1 #if the estimated patient's risks are based on
#the current conditions
RA <- 1.5 #odds ratio under the alternative hypothesis
CL <- 3.5

for (t in 1:n){
  p_t <- test[t,20] #accessing the patient risk for each patient
  y_t <- test[t,8] #accessing the outcome of each patient
  if (y_t == 1) {
    #log-likelihood ratio scores for patient t
    #patient dies
    W_t <- log(((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)) } 
  else {
    #patient survives
    W_t <- log((1 - p_t + R0*p_t)/(1 - p_t + RA*p_t)) 
  }
  X_t <- max(0,X_t + W_t) #new CUSUM values
  df_1[t,] <- c(t,X_t)
}

minY_1 = min(df_1[,2], 0)
maxY_1 = max(df_1[,2],CL)

par(mfrow=c(2,1))
plot(df_1[,1],df_1[,2],ylab="CUSUM Xt",type="l",xlab="patient (t)",
     ylim=c(minY_1, maxY_1))
abline(CL,0,col="blue",lty=2)    #add the control limit

n <- nrow(test) #number of patients
#initial CUSUM value
Z_t <- 0 #to detect decrease in surgical failure rate
df_2 <- matrix(0,n,2)
#(These values can be changed to RA = 0.5 or 1.5 or 2)
R0 <- 1 #if the estimated patient's risks are based on the 
#current conditions
RA <- 0.5 #odds ratio under the alternative hypothesis
CL <- -4.22

for (t in 1:n){
  p_t <- test[t,20] #accessing the patient risk for each patient
  y_t <- test[t,8] #accessing the outcome of each patient
  if (y_t == 1) {
    #log-likelihood ratio scores for patient t
    #patient dies
    W_t <- log(((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)) } 
  else {
    #patient survives
    W_t <- log((1 - p_t + R0*p_t)/(1 - p_t + RA*p_t)) 
  }
  Z_t <- min(0,Z_t - W_t)
  df_2[t,] <- c(t,Z_t)
}

minY_2 = min(df_2[,2], 0)
maxY_2 = max(df_2[,2],CL)

plot(df_2[,1],df_2[,2],ylab="CUSUM Zt",type="l",xlab="patient (t)", 
     ylim=c(minY_2, maxY_2))
abline(CL,0,col="blue",lty=2)    #add the control limit



#Multivariate CUSUM charts

weights_1 <- data.frame(train$Parsonnet,train$surgeon_scores,train$surgery_scores)
head(weights_1,5)
weights_2 <- data.frame(test$Parsonnet,test$surgeon_scores,test$surgery_scores)
head(weights_2,5)
c1 <- weights_1[sample(nrow(weights_1), 30, replace = F), ]
c2 <- weights_2[sample(nrow(weights_2), 30, replace = F), ]

library("MSQC")
Xmv <- mult.chart(c1, type = "t2") $Xmv
S <- mult.chart(c1, type = "t2") $covariance
mult.chart(type = "mcusum", c2, Xmv = Xmv, S = S) #Crosier (1988)
mult.chart(type = "mcusum2", c2, Xmv = Xmv, S = S) #Pignatiello and Runger (1990)




#Risk-Adjusted Multivariate CUSUM charts

#Calculating each surgeon's risk
sn_risk <- exp(-3.896665+0.017857*test$surgeon_scores)/(
  1+exp(-3.896665+0.017857*test$surgeon_scores))
test$snrisk <- sn_risk
#Calculating each surgical risk
sc_risk <- exp(-3.45698+0.10624*test$surgery_scores)/(
  1+exp(-3.45698+0.10624*test$surgery_scores))
test$scrisk <- sc_risk

p_risk <- exp(-3.679771+0.076799*train$Parsonnet)/(
  1+exp(-3.679771+0.076799*train$Parsonnet))
train$prisk <- p_risk
sn_risk <- exp(-3.896665+0.017857*train$surgeon_scores)/(
  1+exp(-3.896665+0.017857*train$surgeon_scores))
train$snrisk <- sn_risk
#Calculating each surgical risk
sc_risk <- exp(-3.45698+0.10624*train$surgery_scores)/(
  1+exp(-3.45698+0.10624*train$surgery_scores))
train$scrisk <- sc_risk

R0 <- 1 #if the estimated patient's risks are based on the current conditions
RA <- 0.5 #odds ratio under the alternative hypothesis
p_t <- test$prisk
K <- 2

test$W_pt <- ifelse(test$ThirtyDayMortality == 1, 
                    log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)), 
                    log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

p_t <- test$snrisk

test$W_snt <- ifelse(test$ThirtyDayMortality == 1, 
                     log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)),
                     log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

p_t <- test$scrisk

test$W_sct <- ifelse(test$ThirtyDayMortality == 1,
                     log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)),
                     log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

R0 <- 1 #if the estimated patient's risks are based on the current conditions
RA <- 0.5 #odds ratio under the alternative hypothesis
p_t <- train$prisk
K <- 2

train$W_pt <- ifelse(train$ThirtyDayMortality == 1,
                     log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)),
                     log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

p_t <- train$snrisk

train$W_snt <- ifelse(train$ThirtyDayMortality == 1,
                      log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)),
                      log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

p_t <- train$scrisk

train$W_sct <- ifelse(train$ThirtyDayMortality == 1,
                      log(K*((1 - p_t + R0*p_t)*RA)/((1 - p_t + RA*p_t)*R0)),
                      log(K*(1 - p_t + R0*p_t)/(1 - p_t + RA*p_t))) #0 - survives and 1 - dies

#Risk-Adjusted Multivariate CUSUM chart
n <- nrow(weights_2)
X_t <- 0
S_t <- matrix(0,3,1)
df <- matrix(0,n,2)
CL <- 3.5
for (i in 1:n ){
  W_t <- data.matrix(c(weights_2[i,1],weights_2[i,2],weights_2[i,3]))
  S_t <- S_t + W_t
  X_t <- sqrt(t(S_t)%*%a%*%(S_t))
  df[i,]<-c(i,X_t)
}
minY = min(df[,2], 0)
maxY = max(df[,2],CL)

plot(df[,1],df[,2],ylab="CUSUM Xt",type="l",xlab="patient",
     ylim=c(minY, maxY))
abline(CL,0,col="blue",lty=2)