library(readr)
sal_hike<-read.csv('C:/Users/HP/Desktop/assignments submission/simple linear regression/Salary_Data.csv')
View(sal_hike)
attach(sal_hike)
cor(YearsExperience,Salary)
#r=0.978

#############Linear Model#################
lin_model<-lm('Salary~YearsExperience',data=sal_hike)
summary(lin_model)
##will check for p-value<0.05 and high R-squared value(0.957) to be a good model,if not go for transformation
lin_predict<-predict(lin_model)
lin_predict
lin_Error<-Salary-lin_predict
lin_Error
ggplot(data=sal_hike,aes(x=YearsExperience,y=Salary))+geom_point(color='red')+geom_line(color='blue',data=sal_hike,aes(x=YearsExperience,y=lin_predict))
confint(lin_model)
predict(lin_model,interval='predict')
cor(Salary,lin_predict)
#r=0.978
lin_rmse=sqrt(mean(lin_Error**2))
lin_rmse
#rmse=5592.044



#############Log Model#################
log_model<-lm('Salary~log(YearsExperience)',data=sal_hike)
summary(log_model)
##will check for p-value<0.05 and high R-squared value(0.853) to be a good model,if not go for transformation
log_predict<-predict(log_model)
log_predict
log_Error<-Salary-log_predict
log_Error
ggplot(data=sal_hike,aes(x=YearsExperience,y=Salary))+geom_point(color='red')+geom_line(color='blue',data=sal_hike,aes(x=YearsExperience,y=log_predict))
confint(log_model)
predict(log_model,interval='predict')
cor(Salary,log_predict)
#r=0.924
log_rmse=sqrt(mean(log_Error**2))
log_rmse
#rmse=10302.89


#############Exponential Model#################
Exp_model<-lm('log(Salary)~YearsExperience',data=sal_hike)
summary(Exp_model)
##will check for p-value<0.05 and high R-squared value(0.932) to be a good model,if not go for transformation
pred_anti<-predict(Exp_model)
Exp_predict<-exp(pred_anti)
Exp_predict
Exp_Error<-Salary-Exp_predict
Exp_Error
ggplot(data=sal_hike,aes(x=YearsExperience,y=Salary))+geom_point(color='red')+geom_line(color='blue',data=sal_hike,aes(x=YearsExperience,y=Exp_predict))
confint(Exp_model)
predict(Exp_model,interval='predict')
cor(Salary,Exp_predict)
#r=0.966
Exp_rmse=sqrt(mean(Exp_Error**2))
Exp_rmse
#rmse=7213.235


#############Quadratic Model#################
Quad_model<-lm('Salary~YearsExperience*YearsExperience+YearsExperience',data=sal_hike)
summary(Quad_model)
##will check for p-value<0.05 and high R-squared value(0.957) to be a good model,if not go for transformation
Quad_predict<-predict(Quad_model)
Quad_predict
Quad_Error<-Salary-Quad_predict
Quad_Error
ggplot(data=sal_hike,aes(x=YearsExperience,y=Salary))+geom_point(color='red')+geom_line(color='blue',data=sal_hike,aes(x=YearsExperience,y=Quad_predict))
confint(Quad_model)
predict(Quad_model,interval='predict')
cor(Salary,Quad_predict)
#r=0.978
Quad_rmse=sqrt(mean(Quad_Error**2))
Quad_rmse
#rmse=5592.044