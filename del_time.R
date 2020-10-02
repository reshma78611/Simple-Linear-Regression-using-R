library(readr)
del_time<-read.csv("C:/Users/HP/Desktop/assignments submission/simple linear regression/delivery_time.csv")
View(del_time)
attach(del_time)
plot(Sorting.Time,Delivery.Time,main='scatter')
cor(Sorting.Time,Delivery.Time)
#r=0.825


#############Linear Model###############
model<-lm('Delivery.Time~Sorting.Time',data=del_time)
summary(model)
##will check for p-value<0.05 and high R-squared value(0.682) to be a good model,if not go for transformation
pred<-predict(model)
pred
Error<-model$residuals
Error
final_data<-data.frame(del_time,pred,Error)
final_data
ggplot(data=del_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='red')+geom_line(color='blue',data=del_time,aes(x=Sorting.Time,y=pred))
confint(model)
predict(model,interval='predict')
rmse=sqrt(mean(Error**2))
rmse
#rmse=2.79


###############Log Transformation###############
cor(log(Sorting.Time),Delivery.Time)
#r=0.833
log_model<-lm('Delivery.Time~log(Sorting.Time)',data=del_time)
summary(log_model)
##will check for p-value<0.05 and high R-squared value(0.69) to be a good model,if not go for transformation
pred_log<-predict(log_model)
pred_log
Error_log<-log_model$residuals
Error_log
final_data_log<-data.frame(del_time,pred_log,Error_log)
final_data_log
ggplot(data=del_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='red')+geom_line(color='blue',data=del_time,aes(x=Sorting.Time,y=pred_log))
confint(log_model)
predict(log_model,interval='predict')
rmse_log=sqrt(mean(Error_log**2))
rmse_log
#rmse=2.73



######################Exponential Transformartion####################
cor(Sorting.Time,log(Delivery.Time))
#r=0.843
exp_model<-lm('log(Delivery.Time)~Sorting.Time',data=del_time)
summary(exp_model)
##will check for p-value<0.05 and high R-squared value(0.71) to be a good model,if not go for transformation
pred1<-predict(exp_model)
pred1
pred_exp<-exp(pred1)
pred_exp
#Error_exp<-exp_model$residuals
Error_exp<-Delivery.Time-pred_exp
Error_exp
final_data_exp<-data.frame(del_time,pred_exp,Error_exp)
final_data_exp
ggplot(data=del_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='red')+geom_line(color='blue',data=del_time,aes(x=Sorting.Time,y=pred_exp))
confint(exp_model)
predict(exp_model,interval='predict')
rmse_exp=sqrt(mean(Error_exp**2))
rmse_exp
#rmse=2.94



##################Quadratic Transformation###############
cor(Sorting.Time*Sorting.Time,Delivery.Time)
#r=0.79
Quad_model<-lm('Delivery.Time~Sorting.Time+Sorting.Time*Sorting.Time',data=del_time)
summary(Quad_model)
##will check for p-value<0.05 and high R-squared value(0.68) to be a good model,if not go for transformation
pred_Quad<-predict(Quad_model)
pred_Quad
Error_Quad<-Quad_model$residuals
Error_Quad
final_data_Quad<-data.frame(del_time,pred_Quad,Error_Quad)
final_data_Quad
ggplot(data=del_time,aes(x=Sorting.Time,y=Delivery.Time))+geom_point(color='red')+geom_line(color='blue',data=del_time,aes(x=Sorting.Time,y=pred_Quad))
confint(Quad_model)
predict(Quad_model,interval='predict')
rmse_Quad=sqrt(mean(Error_Quad**2))
rmse_Quad
#rmse=2.79


