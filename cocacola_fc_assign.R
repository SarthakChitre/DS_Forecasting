library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(rmarkdown)
cocacola=read_excel(file.choose())
View(cocacola)
plot(cocacola$Sales, type = "o")

?grepl
Q1=ifelse(grepl("Q1",cocacola$Quarter),"1","0")
Q2=ifelse(grepl("Q2",cocacola$Quarter),"1","0")
Q3=ifelse(grepl("Q3",cocacola$Quarter),"1","0")
Q4=ifelse(grepl("Q4",cocacola$Quarter),'1','0')

ccd=cbind(cocacola,Q1,Q2,Q3,Q4)
View(ccd)
ccd["t"]=1:42
ccd["t_square"]=ccd["t"]*ccd["t"]
ccd["log_Sales"]=log(ccd$Sales)
View(ccd)
attach(ccd)

#Data Partition
train <- ccd[1:36,]
test <- ccd[37:42,]

library("Metrics")

#######Linear Model###############
linear_model=lm(Sales~t, data=train)
summary(linear_model)
linear_pred=data.frame(predict(linear_model,interval = "predict",newdata = test))
View(linear_pred)
rmse_linear=rmse(test$Sales,linear_pred$fit)
rmse_linear       #667.425

#######Exponential Model####
expo_model=lm(log_Sales~t,data=train)
expo_pred=data.frame(predict(expo_model,interval="predict",newdata=test))
View(expo_pred)
rmse_expo=rmse(test$Sales,exp(expo_pred$fit))
rmse_expo         #526.76

plot(expo_pred$fit)

####### Quadratic Model ######

Quad_model <- lm(Sales~t+t_square,data = train)
summary(Quad_model)


Quad_pred <- data.frame(predict(Quad_model,interval = 'predict', newdata = test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad    #485.14

######## Add Seasonality #######

add_sea_model <- lm(Sales~Q1+Q2+Q3, data = train)
summary(add_sea_model)

add_sea_pred <- data.frame(predict(add_sea_model,newdata=test,
                                   interval ='predict'))
View(add_sea_pred)
rmse_add_sea <- sqrt(mean((test$Sales- add_sea_pred$fit)^2,na.rm=T))

rmse_add_sea  #1895.55

########add seasonality with Linear#########

add_sea_lm <- lm(Sales~t+Q1+Q2+Q3, data = train)
summary(add_sea_lm)
add_sea_lm_pred <- data.frame(predict(add_sea_lm,interval='predict',newdata=test))
rmse_add_lm <- sqrt(mean((test$Sales-add_sea_lm_pred$fit)^2,na.rm=T))
rmse_add_lm    #555.34

#######add seasonality with Quadratic #######

add_sea_Quad <- lm(Sales~t+t_square+Q1+Q2+Q3, data = train)
summary(add_sea_Quad)

add_sea_Quad_pred <- data.frame(predict(add_sea_Quad,interval='predict',newdata=test))
rmse_add_Quad <- sqrt(mean((test$Sales-add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_add_Quad   #283.06

###### Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi <- sqrt(mean((test$Sales-multi_sea_pred$fit)^2,na.rm=T))
rmse_multi   #4595.376

#additive seasonality with Quadratic trend is the best model since least RMSE error


# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_add_sea","rmse_add_lm","rmse_add_Quad","rmse_multi"),
                       c(rmse_linear,rmse_expo,rmse_Quad,rmse_add_sea,rmse_add_lm,rmse_add_Quad,rmse_multi))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

### Combining Training & test data to build Additive seasonality using Quadratic Trend ####
new_model<-lm(Sales~t+t_square+Q1+Q2+Q3,data=ccd)
residual=new_model$residuals
View(residual)
acf(new_model$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot

##### Not Needed###
A <- arima(new_model$residuals, order = c(1,0,0))
A$residuals
Aerrors <- A$residuals
acf(Aerrors, lag.max = 10)
####




new_model_fin <- new_model$fitted.values
View(new_model_fin)
Quarter <- as.data.frame(ccd$Quarter)
final <- as.data.frame(cbind(Quarter,ccd$Sales,new_model_fin))
View(final)
colnames(final) <- c("Quarter","Sales","New_pred_value")

plot(final$Sales, col.axis="blue")

plot(final$Sales,main="ActualGraph",xlab="Sales(Actual",
     ylab = "Quarter",
     col.axis="blue",type="o")

plot(final$New_pred_value, main = "PredictedGraph", xlab = "Sales(predicted)",ylab = "Quarter",
     col.axis="Red",type="s")


View(final)
