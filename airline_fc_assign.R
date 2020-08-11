install.packages("rmarkdown")
library(rmarkdown)
install.packages("forecast")
library(forecast)
install.packages("fpp")
library(fpp)
install.packages("smooth")
library(smooth)
#read file
library(readxl)
airlines=read_excel(file.choose())
View(airlines)
plot(airlines$Passengers,type = "o")

#Creating dummy variables
x=data.frame(outer(rep(month.abb,length = 96),month.abb,"==")+0)
View(x)
colnames(x)=month.abb
airlinedata=cbind(airlines,x)
View(airlinedata)

airlinedata["t"]=1:96
airlinedata["t_square"]=airlinedata["t"]*airlinedata["t"]
View(airlinedata)

airlinedata["log_passenger"] <- log(airlinedata["Passengers"])
attach(airlinedata)

#DataPartition
train <- airlinedata[1:84,]
test <- airlinedata[85:96,]

install.packages("Metrics")
library(Metrics)

###################Linear Model####################
airline_linear=lm(Passengers~t,data=train)
linear_pred=data.frame(predict(airline_linear,interval = "predict",newdata=test))
View(linear_pred)
rmse_lm =rmse(test$Passengers,linear_pred$fit) #53.199
rmse_lm

################Exponential Model############
airline_expo <- lm(log_passenger~t,data=train)
airline_expo_pred <- data.frame(predict(airline_expo,interval='predict', newdata=test))
View(airline_expo_pred)
rmse_expo=rmse(test$Passengers,exp(airline_expo_pred$fit)) #46.05
exp(airline_expo_pred$fit)
test$Passengers

##############Quaddratic Model#############
airline_quad=lm(Passengers~t+t_square,data=train)
airline_quad_pred=data.frame(predict(airline_quad,interval="predict",newdata=test))
View(airline_quad_pred)
rmse_Quad=rmse(test$Passengers,airline_quad_pred$fit)   #48.05
rmse_Quad

#############Additive Seasonality########
sea_add_model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov , data=train)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
View(sea_add_pred)
rmse_sea_add=rmse(test$Passengers,sea_add_pred$fit)      #132.81

#########Additive Seasonlity with linear#######
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
View(Add_sea_Linear_pred)
rmse_Add_sea_Linear=rmse(test$Passengers,Add_sea_Linear_pred$fit)  #35.34    
rmse_Add_sea_Linear     #35.34

######## Additive Seasonality with Quadratic ######
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #26.36082

# Multiplicative Seasonality 
multi_sea_model<-lm(log_passenger~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea  #140.0632

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_lm","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_lm,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Additive seasonality with Quadratic has least RMSE value

### Combining Training & test data to build Additive seasonality using Quadratic Trend ####

Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = airlines)
summary(Add_sea_Quad_model_final)
residual=Add_sea_Quad_model_final$residuals
#####
plot(Add_sea_Quad_model_final)
acf(Add_sea_Quad_model_final$residuals, lag.max = 10) # take all residual value of the model built & plot ACF plot
A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,0,0))

###########
A$residuals
Aerrors <- A$residuals
acf(Aerrors, lag.max = 10)
# predicting next 12 months errors using arima( order =c(1,0,0))
library(forecast)
errors_12 <- forecast(A, h = 12) # head of months
errors_12
predict_res=predict(arima(residual,order=c(1,0,0)),n.ahead=12)
predict_res$pred
future_errors=as.data.frame(predict_res$pred)
View(future_errors)


#newdata
newtest_data=read_excel(file.choose())
View(newtest_data)
pred_new=data.frame(predict(Add_sea_Quad_model_final,newdata=newtest_data,interval="predict"))
View(pred_new)
predicted_new_values <- pred_new$fit + future_errors
View(predicted_new_values)
plot(predicted_new_values,type="o")
