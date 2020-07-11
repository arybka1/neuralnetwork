# NN #

source("http://blogs.fiveelementanalytics.com/RCode/min_max_normalization.R")
## The above code is courtesy of Dr. Alex Pelaez of 5 Element Analytics. You can find out ##
## more at his company's website, https://www.fiveelementanalytics.com/ ##
## Much of the below code is structured and based on assignments from his course. ##
## In addition, Dr. Palaez was instrumental in helping me frequently troubleshoot. ##
## Thank you, Dr. Palaez! ##

library(neuralnet)
library(readr)

## This dataset uses a FIFA player dataset. While not the exact dataset used, a functionally similar##
## dataset can be found at this URL: https://www.kaggle.com/karangadiya/fifa19 ##
## If your dataset does not include 89 columns and 18,207 records, you will need to adjust lines 20-22##

## Prior to running the below code be sure to import the dataset as "fifatotal". The below code ##
## assumes that missing data has been removed. ##

fifatotal = fifa[,c(16,55:88)]
train <- fifatotal[(1:12711),]
test <- fifatotal[(12712:18207),]
train = train[complete.cases(train),]
test = test[complete.cases(test),]
test[,-1] = as.data.frame(lapply(test[,-16], min_max_normal))
train[,-1] = as.data.frame(lapply(train[,-16], min_max_normal))

train[, "one"] = as.numeric(train[,"International.Reputation"] == 1)
train[, "two"] = as.numeric(train[,"International.Reputation"] == 2)
train[, "three"] = as.numeric(train[,"International.Reputation"] == 3)
train[, "four"] = as.numeric(train[,"International.Reputation"] == 4)
train[, "five"] = as.numeric(train[,"International.Reputation"] == 5)


test[, "one"] = as.numeric(test[,"International.Reputation"] == 1)
test[, "two"] = as.numeric(test[,"International.Reputation"] == 2)
test[, "three"] = as.numeric(test[,"International.Reputation"] == 3)
test[, "four"] = as.numeric(test[,"International.Reputation"] == 4)
test[, "five"] = as.numeric(test[,"International.Reputation"] == 5)



inputs = ncol(train) - 1
outputs = 5
neuron_estimate  = ceiling(nrow(train) / (2 * (inputs + outputs)))
n = colnames(train)
frm = 'one + two + three + four + five ~ '
for (i in 2:35) {
  
  frm = paste(frm, names(train)[i],sep="+")
  
}
frm

nn = neuralnet(frm,data=train,hidden=c(30,20),linear.output=F)
plot(nn)
predict_num = compute(nn, covariate = test[,-(36:40)])$net.result

tmp_output = cbind(test[1:5448,"International.Reputation"], round(predict_num[1:5448,]))
tmp_output = as.data.frame(tmp_output)
names(tmp_output) = c("Actual", "One", "Two", "Three", "Four", "Five")
tmp_output$Pred=tmp_output$One*1+tmp_output$Two*2+tmp_output$Three*3+tmp_output$Four*4+tmp_output$Five*5
tmp_output$Match = as.numeric(tmp_output$Actual==tmp_output$Pred)
tmp_output
sum (tmp_output$Match)

# Randomized NN test #

newtest = fifatotal[sample(nrow(fifatotal), 5000), ]
newtest = newtest[complete.cases(newtest),]
newtest[,-1] = as.data.frame(lapply(newtest[,-16], min_max_normal))

newtest[, "one"] = as.numeric(newtest[,"International.Reputation"] == 1)
newtest[, "two"] = as.numeric(newtest[,"International.Reputation"] == 2)
newtest[, "three"] = as.numeric(newtest[,"International.Reputation"] == 3)
newtest[, "four"] = as.numeric(newtest[,"International.Reputation"] == 4)
newtest[, "five"] = as.numeric(newtest[,"International.Reputation"] == 5)

predict_num = compute(nn, covariate = newtest[,-(36:40)])$net.result
tmp_output = cbind(newtest[1:4988,"International.Reputation"], round(predict_num[1:4988,]))
tmp_output = as.data.frame(tmp_output)
names(tmp_output) = c("Actual", "One", "Two", "Three", "Four", "Five")
tmp_output$Pred=tmp_output$One*1+tmp_output$Two*2+tmp_output$Three*3+tmp_output$Four*4+tmp_output$Five*5
tmp_output$Match = as.numeric(tmp_output$Actual==tmp_output$Pred)
tmp_output
sum (tmp_output$Match)

summary(newtest)