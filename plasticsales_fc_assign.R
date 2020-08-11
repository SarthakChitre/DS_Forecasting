library(forecast)
library(fpp)
library(rmarkdown)
library(smooth)
library(readxl)

plastics <- read.csv(file.choose())
View(plastics)
dim(plastics)
head(plastics)
windows()
plot(plastics$Sales,type='o')

#Dummy variables
x=data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(x) <- month.abb
plasticsales=cbind(plastics,x)

View(plasticsales)

plasticsales["t"] <- 1:60
View(plasticsales)
plasticsales["log_sales"] <- log(plasticsales["Sales"])
plasticsales["t_square"] <- plasticsales["t"]*plasticsales["t"]
attach(plasticsales)

#train test

train <- plasticsales[1:48,]
test <- plasticsales[49:60,]

#linear model

plastic_lm <- lm(Sales~t,data = train)
summary(plastic_lm) 
lm_pred <-data.frame(predict(plastic_lm, interval = 'predict',newdata = test))
View(lm_pred)
rmse_lm <- sqrt(mean((test$Sales-lm_pred$fit)^2,na.rm = T))
rmse_lm  #260.93
plot(lm_pred$fit)

#Exponential

plastic_expo <- lm(log_sales~t, data = train)
summary(plastic_expo) 
expo_pred <- data.frame(predict(plastic_expo, interval = 'predict', newdata = test))
View(expo_pred) 
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo    #268.69

#Quadratic

plastic_Quad <- lm(Sales~t+t_square, data = train)
summary(plastic_Quad) #R-squared:  0.1253, p-value: 0.03173
Quad_pred  <- data.frame(predict(plastic_Quad, interval='predict', newdata=test))
View(Quad_pred)
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm =T))
rmse_Quad   #297.40

#Additive seasonality

sea_add_model <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov , data=train)
summary(sea_add_model)   
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
View(sea_add_pred)
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #235.60

# Additive Seasonality with Linear

Add_sea_Linear_model <- lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred <- data.frame(predict(Add_sea_Linear_model,interval='predict',newdata = test))
View(Add_sea_Linear_pred)
rmse_Add_sea_Linear <- sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear  #135.55

# Additive Seasonality with Quadratic 

Add_sea_Quad_model <- lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                         data = train)
summary(Add_sea_Quad_model) 
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model,interval='predict',
                                        newdata=test))
View(Add_sea_Quad_pred)

rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad   #218.19

# Multiplicative Seasonality 

multi_sea_model <- lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                      data = train)
summary(multi_sea_model) 

multi_sea_pred <- data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
View(multi_sea_pred)

rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea   #239.65


# Multiplicative Seasonality Linear trend 

multi_add_sea_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,
                          data = train)
summary(multi_add_sea_model)

multi_add_sea_pred <- data.frame(predict(multi_add_sea_model,newdata=test,
                                         interval='predict'))
View(multi_add_sea_pred)

rmse_multi_add_sea <- sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea #160.68

# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_lm","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Linear",
                           "rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),
                         c(rmse_lm,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Linear,rmse_Add_sea_Quad,
                           rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


#Additive seasonality linear model has the least rmse

new_model=lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = plasticsales)
new_model_fin=new_model$fitted.values
View(new_model_fin)

Month=as.data.frame(plasticsales$Month)
Final <- as.data.frame(cbind(Month,plasticsales$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
View(Final)


plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")

#ACF
acf(new_model$residuals,lag.max = 10)
A=arima(new_model$residuals,order = c(1,0,0))
acf(A$residuals,lag.max = 10)

#predicting errors
library(forecast)
errors_12 <- forecast(A, h = 12) # head of months
errors_12
predict_error=predict(A,n.ahead = 12)
future_errors=as.data.frame(predict_error$pred)
View(future_errors)
colnames(future_errors)="predicted_errors"
View(future_errors)
