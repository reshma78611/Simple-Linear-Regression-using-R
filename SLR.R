library(readr)
wc_at<-read.csv("C:/Users/HP/Desktop/datasets/wc-at.csv")
View(wc_at)
attach(wc_at)

############scatter plot#############
plot(Waist,AT,main = "scatterplot")

#relationship => linear
#direction => positive
#strength => moderate

##########correlation coefficient(r)#######
r=cor(Waist,AT)
r
#r=0.81 => moderately correlated

################linear model#############
model<-lm(AT~Waist,data=wc_at)
summary(model)
##will check for p-value<0.05 and high R-squared value(0.67) to be a good model,if not go for transformation
pred<-predict(model)
pred
#Error
model$residuals
final_data<-data.frame(wc_at,pred,"Error"=model$residuals)
final_data
#Model plot
ggplot(data=wc_at,aes(x=Waist,y=AT))+geom_point(color='red')+geom_line(color='blue',data=wc_at,aes(x=Waist,pred))
confint(model)
#confidence interval for intercept and slope
predict(model,interval='predict')
rmse<-sqrt(mean(model$residuals**2))
rmse
#rmse=32.76


#################Log Transformation###################
cor(log(Waist),AT)
#r=0.82
model2<-lm('AT~log(Waist)',data=wc_at)
summary(model2)
#will check for p-value<0.05 and high R-squared value(0.6753) to be a good model,if not go for transformation
pred2<-predict(model2)
pred2
Error<-model2$residuals
Error
final_data2<-data.frame(wc_at,pred2,Error)
final_data2
#Model plot
ggplot(data=wc_at,aes(x=Waist,y=AT))+geom_point(color='red')+geom_line(color='blue',data=wc_at,aes(x=Waist,y=pred2))
confint(model2)
predict(model2,interval='predict')
rmse<-sqrt(mean(Error**2))
rmse
#rmse=32.49


###################Exponential Transfromation####################
cor(Waist,log(AT))
#r=0.84
model3<-lm('log(AT)~Waist',data=wc_at)
summary(model3)
#will check for p-value<0.05 and high R-squared value(0.7071) to be a good model,if not go for transformation
pred_log<-predict(model3)
pred_log
pred3<-exp(pred_log)
Error3<-model3$residuals
Error3
final_data3<-data.frame(wc_at,pred3,Error3)
final_data3
#Model plot
ggplot(data=wc_at,aes(x=Waist,y=AT))+geom_point(color='green')+geom_line(color='blue',data=wc_at,aes(x=Waist,y=pred3))
confint(model3)
predict(model3,interval='predict')
rmse=sqrt(mean(Error3**2))
rmse
#rmse=0.3496


##############Quadratic Transformation###################
cor(Waist*Waist,AT)
#r=0.81
model4<-lm('AT~Waist+I(Waist*Waist)',data=wc_at)
summary(model4)
#will check for p-value<0.05 and high R-squared value(0.677) to be a good model,if not go for transformation
pred4<-predict(model4)
pred4
Error4<-model4$residuals
Error4
final_data4<-data.frame(wc_at,pred4,Error4)
final_data4
#Model plot
ggplot(data=wc_at,aes(x=Waist,y=AT))+geom_point(color='green')+geom_line(color='red',data=wc_at,aes(x=Waist,y=pred4))
confint(model4)
predict(model4,interval='predict')
rmse=sqrt(mean(Error4**2))
rmse
#rmse=32.36
