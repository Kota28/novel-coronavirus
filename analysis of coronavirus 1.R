library(readr)
cases<- read.csv("COVID-19.csv",fileEncoding = "UTF-8-BOM") #import data set
library(tidyverse)
library(magrittr) #import library
cases %<>%
  mutate(確定日=確定日 %>% as.Date.character(format='%m/%d/%Y')) #change format
cases %<>%
  group_by(確定日) %>%
  summarise(発症数=n()) %>%
  filter(!is.na(確定日))
cases %<>%
  mutate(growth.rate=発症数/lag(発症数)) #calculate growth-rate
cases %<>%
  filter(確定日>='2020-02-14' %>% as.Date)
#In this analysis we analyze after 2/14
cases %>%
  group_by(確定日) %>%
  summarize(発症数=n()) %>%
  ggplot(aes(x=確定日,y=発症数)) +
  geom_path()+
  geom_point()+
  scale_y_log10()
#plot the number of infected
n<-lm(log(発症数)~確定日,data=cases)
#regression
plot(cases$確定日,log(cases$発症数),xlab="確定日",ylab="log(感染者数)",type='l')
R<-array(0,dim=c(length(cases$growth.rate),1))
#logg<-array(0,dim=c(length(cases$growth.rate),1))
#for(i in 1:length(cases$growth.rate)){
  #logg[i]<-log10(cases$発症数[i+1])/lag(log10(cases$発症数[i]))
#}
#we calculated R (effective reproduction number)
for(i in 1:length(cases$growth.rate)){
  R[i]<-10*(log(cases$growth.rate[i]+0.1))
}
plot(cases$確定日,R,type='l',xlab='確定日')
cases<-data.frame(cases,R)
#we calculated acf
cases$R %>% acf
par(mfrow=c(1,1))
dev.off()
#The periodicity of two, five, and seven days was confirmed.
plot(diff(cases$R,lag=2),type="l")
plot(diff(cases$R,lag=5),type="l")
plot(diff(cases$R,lag=7),type="l")
diff(cases$R,lag=2) %>% acf
diff(cases$R,lag=5) %>% acf
diff(cases$R,lag=7) %>% acf
plot(cases$R,type="l",col="red",xlab='time',ylab='R',xlim=c(0,80),ylim=c(-10,10))
par(new=T)
#Since the seven-day periodicity is the strongest, consider removing this periodicity by taking a difference every seven days.
plot(diff(cases$R,lag=7),type="l",col="blue",xlab='time',ylab='R',xlim=c(0,80),ylim=c(-10,10))
labels=c("cases$R","diff(cases$R,lag=7)")
colos=c("red","blue")
legend("bottomright",legend=labels,col=colos)
diff(cases$R,lag=7) %>% which.max %>% cases$確定日[.]
require(stats)
#Due to the convenience of the analysis, we try to smooth the data below by taking a moving average instead of taking the difference.
newR<-stats::filter(cases$R,filter=rep(1/7,7))
cases %<>%
  filter(確定日>='2020-03-25' %>% as.Date)
cases %<>%
  mutate(日数=確定日-'2020-01-01' %>% as.Date) %>%
  mutate(日数=日数%>%as.numeric)
na.omit(newR)
newR<-stats::filter(cases$R,filter=rep(1/7,7))
#regression
a<-lm(newR~cases$日数,data=cases)
plot(cases$日数,newR,xlab="日数",ylab="R")
abline(a)
summary(a)
plot(a)
newx<-data.frame(x=seq(84,123,length=40))
conf.interval<-predict(a,newdata=newx,interval='confidence',level=0.95)
head(conf.interval)
plot(cases$日数,newR, xlab = '日数', ylab = 'Ｒ')
abline(a,col='red')
#we also plot CI.
lines(newx$x, conf.interval[, 1], col = 'orange')
lines(newx$x, conf.interval[, 2], col = 'darkgreen')
lines(newx$x, conf.interval[, 3], col = 'darkgreen')
AIC(a)
BIC(a)
#We also do multiple regression.
b<-lm(newR~1+(日数)+poly((日数),2)+poly((日数),3)+poly((日数),4)+poly((日数),5)+poly((日数),6)+poly((日数),7)+poly((日数),8),data=cases)
step(b)
lines(sort(cases$日数),fitted(b)[order(cases$日数)],col='blue')
c<-lm(newR~1+(日数)+poly((日数),2)+poly((日数),3)+poly((日数),4)+poly((日数),5),data=cases)
step(c)
lines(sort(cases$日数),fitted(c)[order(cases$日数)],col='green')