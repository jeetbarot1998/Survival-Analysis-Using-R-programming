data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
summary(data) 


linregmodel = lm(lifetime~.-broken,data=data)  
summary(linregmodel)

library(survival) 

dependantvars = Surv(data$lifetime, data$broken) 
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) 
summary(survreg)  

will_break=predict(survreg, newdata=data, type="quantile", p=.5) 
Forecast=data.frame(will_breake)

Forecast$lifetime=data$lifetime 
Forecast$broken=data$broken 

# Expected Remaining Lifetime
Forecast$RemainingLT=Forecast$will_break-data$lifetime 
View(Forecast) 

Forecast=Forecast[order(Forecast$RemainingLT),] 
ActionsPriority=Forecast[Forecast$broken==0,] 
View(ActionsPriority)
