library(zoo)
library(forecast)

## data preparation
new<-read.csv("/Users/heyunyu/Desktop/5200/database.csv",stringsAsFactors = F,header = T,na.strings = "")
new<-dplyr::filter(new,new$DATE>="2016-01-01")
new1<-new[which(is.na(new$LATITUDE)),]
new2<-new1[,-c(8:13)]
neww<-new[,c(8:13)]


## variables data 
num<-rep(0,nrow(new2))
for(i in 1:nrow(new2)){
  num[i]<-sum((new2$VEHICLE.1.TYPE[i]!="NA")+(new2$VEHICLE.2.TYPE[i]!="NA")
              +(new2$VEHICLE.3.TYPE[i]!="NA")+(new2$VEHICLE.4.TYPE[i]!="NA")
              +(new2$VEHICLE.5.TYPE[i]!="NA"))
}
new3<-cbind(new2,num)
new3<-cbind(new2,num)
View(new3)
new3$VEHICLE.1.TYPE[which(new3$VEHICLE.1.TYPE=="UNKNOWN")]<-"NA"
new3$VEHICLE.2.TYPE[which(new3$VEHICLE.2.TYPE=="UNKNOWN")]<-"NA"
new3$VEHICLE.3.TYPE[which(new3$VEHICLE.3.TYPE=="UNKNOWN")]<-"NA"
new3$VEHICLE.4.TYPE[which(new3$VEHICLE.4.TYPE=="UNKNOWN")]<-"NA"
new3$VEHICLE.5.TYPE[which(new3$VEHICLE.5.TYPE=="UNKNOWN")]<-"NA"
new3$VEHICLE.1.FACTOR[which(new3$VEHICLE.1.FACTOR=="UNSPECIFIED")]<-"NA"
new3$VEHICLE.2.FACTOR[which(new3$VEHICLE.2.FACTOR=="UNSPECIFIED")]<-"NA"
new3$VEHICLE.3.FACTOR[which(new3$VEHICLE.3.FACTOR=="UNSPECIFIED")]<-"NA"
new3$VEHICLE.4.FACTOR[which(new3$VEHICLE.4.FACTOR=="UNSPECIFIED")]<-"NA"
new3$VEHICLE.5.FACTOR[which(new3$VEHICLE.5.FACTOR=="UNSPECIFIED")]<-"NA"

a<-unique(new3$VEHICLE.1.TYPE)
b<-unique(new3$VEHICLE.2.TYPE)
c<-unique(new3$VEHICLE.3.TYPE)
d<-unique(new3$VEHICLE.4.TYPE)
e<-unique(new3$VEHICLE.5.TYPE)

a<-unique(a,b,c,d,e)

aa<-unique(new3$VEHICLE.1.TYPE)
bb<-unique(new3$VEHICLE.2.TYPE)
cc<-unique(new3$VEHICLE.3.TYPE)
dd<-unique(new3$VEHICLE.4.TYPE)
ee<-unique(new3$VEHICLE.5.TYPE)
aa<-unique(aa,bb,cc,dd,ee)

a<-as.character(a)
aa<-as.character(aa)

## type level
for(i in 1:nrow(new3)){
  new3$type1[i]=sum((new3$VEHICLE.1.TYPE[i]==a[1])+(new3$VEHICLE.2.TYPE[i]==a[1])+(new3$VEHICLE.3.TYPE[i]==a[1])
                    +(new3$VEHICLE.4.TYPE[i]==a[1])+(new3$VEHICLE.5.TYPE[i]==a[1]))
}
  
for(i in 1:nrow(new3)){
  new3$type2[i]=sum((new3$VEHICLE.1.TYPE[i]==a[2])+(new3$VEHICLE.2.TYPE[i]==a[2])+(new3$VEHICLE.3.TYPE[i]==a[2])
                    +(new3$VEHICLE.4.TYPE[i]==a[2])+(new3$VEHICLE.5.TYPE[i]==a[2]))
}

for(i in 1:nrow(new3)){
  new3$type3[i]=sum((new3$VEHICLE.1.TYPE[i]==a[3])+(new3$VEHICLE.2.TYPE[i]==a[3])+(new3$VEHICLE.3.TYPE[i]==a[3])
                    +(new3$VEHICLE.4.TYPE[i]==a[3])+(new3$VEHICLE.5.TYPE[i]==a[3]))
}


for(i in 1:nrow(new3)){
  new3$type4[i]=sum((new3$VEHICLE.1.TYPE[i]==a[4])+(new3$VEHICLE.2.TYPE[i]==a[4])
                    +(new3$VEHICLE.3.TYPE[i]==a[4])
                    +(new3$VEHICLE.4.TYPE[i]==a[4])+(new3$VEHICLE.5.TYPE[i]==a[4]))
}

for(i in 1:nrow(new3)){
  new3$type5[i]=sum((new3$VEHICLE.1.TYPE[i]==a[5])+(new3$VEHICLE.2.TYPE[i]==a[5])
                    +(new3$VEHICLE.3.TYPE[i]==a[5])
                    +(new3$VEHICLE.4.TYPE[i]==a[5])+(new3$VEHICLE.5.TYPE[i]==a[5]))
}
for(i in 1:nrow(new3)){
  new3$type6[i]=sum((new3$VEHICLE.1.TYPE[i]==a[6])+(new3$VEHICLE.2.TYPE[i]==a[6])
                    +(new3$VEHICLE.3.TYPE[i]==a[6])
                    +(new3$VEHICLE.4.TYPE[i]==a[6])+(new3$VEHICLE.5.TYPE[i]==a[6]))
}

for(i in 1:nrow(new3)){
  new3$type7[i]=sum((new3$VEHICLE.1.TYPE[i]==a[7])+(new3$VEHICLE.2.TYPE[i]==a[7])
                    +(new3$VEHICLE.3.TYPE[i]==a[7])
                    +(new3$VEHICLE.4.TYPE[i]==a[7])+(new3$VEHICLE.5.TYPE[i]==a[7]))
}

for(i in 1:nrow(new3)){
  new3$type8[i]=sum((new3$VEHICLE.1.TYPE[i]==a[8])+(new3$VEHICLE.2.TYPE[i]==a[8])
                    +(new3$VEHICLE.3.TYPE[i]==a[8])
                    +(new3$VEHICLE.4.TYPE[i]==a[8])+(new3$VEHICLE.5.TYPE[i]==a[8]))
}

for(i in 1:nrow(new3)){
  new3$type9[i]=sum((new3$VEHICLE.1.TYPE[i]==a[9])+(new3$VEHICLE.2.TYPE[i]==a[9])
                    +(new3$VEHICLE.3.TYPE[i]==a[9])
                    +(new3$VEHICLE.4.TYPE[i]==a[9])+(new3$VEHICLE.5.TYPE[i]==a[9]))
}
for(i in 1:nrow(new3)){
  new3$type10[i]=sum((new3$VEHICLE.1.TYPE[i]==a[10])+(new3$VEHICLE.2.TYPE[i]==a[10])
                    +(new3$VEHICLE.3.TYPE[i]==a[10])
                    +(new3$VEHICLE.4.TYPE[i]==a[10])+(new3$VEHICLE.5.TYPE[i]==a[10]))
}
for(i in 1:nrow(new3)){
  new3$type11[i]=sum((new3$VEHICLE.1.TYPE[i]==a[11])+(new3$VEHICLE.2.TYPE[i]==a[11])
                    +(new3$VEHICLE.3.TYPE[i]==a[11])
                    +(new3$VEHICLE.4.TYPE[i]==a[11])+(new3$VEHICLE.5.TYPE[i]==a[11]))
}

for(i in 1:nrow(new3)){
  new3$type12[i]=sum((new3$VEHICLE.1.TYPE[i]==a[12])+(new3$VEHICLE.2.TYPE[i]==a[12])
                    +(new3$VEHICLE.3.TYPE[i]==a[12])
                    +(new3$VEHICLE.4.TYPE[i]==a[12])+(new3$VEHICLE.5.TYPE[i]==a[12]))
}
for(i in 1:nrow(new3)){
  new3$type13[i]=sum((new3$VEHICLE.1.TYPE[i]==a[13])+(new3$VEHICLE.2.TYPE[i]==a[13])
                    +(new3$VEHICLE.3.TYPE[i]==a[13])
                    +(new3$VEHICLE.4.TYPE[i]==a[13])+(new3$VEHICLE.5.TYPE[i]==a[13]))
}
for(i in 1:nrow(new3)){
  new3$type14[i]=sum((new3$VEHICLE.1.TYPE[i]==a[14])+(new3$VEHICLE.2.TYPE[i]==a[14])
                    +(new3$VEHICLE.3.TYPE[i]==a[14])
                    +(new3$VEHICLE.4.TYPE[i]==a[14])+(new3$VEHICLE.5.TYPE[i]==a[14]))
}
for(i in 1:nrow(new3)){
  new3$type15[i]=sum((new3$VEHICLE.1.TYPE[i]==a[15])+(new3$VEHICLE.2.TYPE[i]==a[15])
                    +(new3$VEHICLE.3.TYPE[i]==a[15])
                    +(new3$VEHICLE.4.TYPE[i]==a[15])+(new3$VEHICLE.5.TYPE[i]==a[15]))
}
for(i in 1:nrow(new3)){
  new3$type16[i]=sum((new3$VEHICLE.1.TYPE[i]==a[16])+(new3$VEHICLE.2.TYPE[i]==a[16])
                    +(new3$VEHICLE.3.TYPE[i]==a[16])
                    +(new3$VEHICLE.4.TYPE[i]==a[16])+(new3$VEHICLE.5.TYPE[i]==a[16]))
}

for(i in 1:nrow(new3)){
  new3$factor1[i]=sum((new3$VEHICLE.1.FACTOR[i]==aa[1])+(new3$VEHICLE.2.FACTOR[i]==aa[1])
                    +(new3$VEHICLE.3.FACTOR[i]==aa[1])
                    +(new3$VEHICLE.4.FACTOR[i]==aa[1])+(new3$VEHICLE.5.FACTOR[i]==aa[1]))
}


## factor level
for(j in 1:length(aa)){
for(i in 1:nrow(new3)){
  new3[i,34+j]=sum((new3$VEHICLE.1.FACTOR[i]==aa[j])+(new3$VEHICLE.2.FACTOR[i]==aa[j])
                      +(new3$VEHICLE.3.FACTOR[i]==aa[j])
                      +(new3$VEHICLE.4.FACTOR[i]==aa[j])+(new3$VEHICLE.5.FACTOR[i]==aa[j]))
}
}


new3$factor2<-c(rep(0,nrow(new3)))
new3[,37:81]<-c(rep(0,nrow(new3)))

View(new3)

time<-unique(neww$DATE)
fre<-c(rep(0,length(time)))
for(i in 1:length(time)){
  fre[i]<-sum(neww$DATE==time[i])
}
data<-data.frame(time,fre)
View(data)
x<-ts(data$fre)
auto.arima(x)

wea<-read.csv("/Users/heyunyu/Desktop/we.csv",header = T)
new3$d<-as.POSIXct(new3$DATE)
wea$d<-as.POSIXct(wea$date)
ddd<-merge(new3,wea,by="d")
View(ddd)
ddd$precipitation[which(ddd$precipitation=="T")]<-1

ddd$precipitation[which(ddd$precipitation!="0")]<-1
ddd$precipitation<-factor(ddd$precipitation)

fit<-lm(number~num+type1+type2+type3+type4+type5
        +type6+type7+type8+type9+type10+type11
        +type12+type13+type14+type15+type16+factor1+factor2+V37+
          V38+V39+V40+V41+V42+V43+V44+V45+V46+
          V47+V48+V49+V50+V51+V52+V53+V54+V55+V56
        +V57+V58+V59+V60+V61+V62+V63+V64+V65+V66+V67+V68
        +V69+V70+V71+V72+V73+V74+V75+V76+V77+V78+V79+V80
        +V81+precipitation
        +average.temperature,data=ddd)
summary(fit)

## stepwise regression
fstep<-step(fit)

fit1<-lm(number ~ num + type1 + type2 + type3 + type4 + type5 + type6 + 
  type7 + type8 + type9 + type10 + type13 + type15 + factor1 + 
  factor2 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + 
  V46 + V47 + V49 + V50 + V52 + V53 + V54 + V56 + V59 + V61 + 
  V62 + V65 + V66 + V69 + V70 + V71 + V72 + V75 + V78 + precipitation + 
  average.temperature,data = ddd)

summary(fit1)

drop1(fstep)

fit2<-lm(number ~ num + type1 + type2 + type3 + type4 + type5 + type6 + 
           type7 + type8 + type9 + type10 + type13 + type15 + factor1 + 
           factor2 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + 
           V46 + V47 + V49 + V50 + V52 + V53 + V54 + V56 + V59  + 
           V62 + V65 + V66 + V69 + V70 + V71 + V72 + V75 + V78 + precipitation + 
           average.temperature,data = ddd)
summary(fit2)

fit3<-lm(number ~ num + type1 + type2 + type3 + type4 + type5 + type6 + 
           type7 + type8  + type10 + type13 + type15 + factor1 + 
           factor2 + V37 + V38 + V39 + V40 + V41 + V42 + V43 + V44 + 
           V46 + V47 + V49 + V50 + V52 + V53 + V54 + V56 + V59  + 
           V62 + V65 + V66 + V69 + V70 + V71 + V72 + V75 + V78 + precipitation + 
           average.temperature,data = ddd)
summary(fit3)

fit4<-lm(number ~ num + type1 + type2 + type3 + type4 + type5 + type6 + 
           type7 + type8  + type10 + type13 + type15 + factor1 + 
           factor2 + V37 + V38 + V39 + V40 + V43 + V44 + 
           V46 + V47 + V49 + V50 + V52 + V53 + V54 + V56 + V59  + 
           V65 + V66 + V69 + V70 + V71 + V72 + V75 + V78 + precipitation + 
           average.temperature,data = ddd)
summary(fit4)


fit4<-lm(number ~ num + type1 + type4 + type13 + type15 + factor1 + 
           factor2 + V37 + V38 + V39 + V40 + V43 + V44 + 
           V46 + V47 + V49 + V50 + V52 + V53 + V54 + V56 + V59  + 
           V65 + V66 + V69 + V70 + V71 + V72 + V75 + V78 + precipitation + 
           average.temperature,data = ddd)
summary(fit4)

fit5<-lm(number ~  type1 + type2 + type3 + type4 + type5 + type6 + 
           type7 + type8  + type10 + type13 + type15 + V37 + V38 + V39 + V43 + V44 + 
           V46 + V47 + V49+ V52 + V53 + V54 + V56 + V59  + 
           V65 + V69 + V71 + V75 + V78 + precipitation + 
           average.temperature,data = ddd)
summary(fit5)


## time series
## model 1

len<-unique(neww$DATE)
pn<-rep(0,length(len))
for(i in 1:length(len)){
  for(j in 1:nrow(neww)){
  if(neww$DATE[j]==len[i]){
    pn[i]<-ddd$number[j]
  }
  }
  }


len<-unique(neww$DATE)
for(i in 1:nrow(neww)){
neww$number[i]<-sum(neww$PERSONS.INJURED[i]+neww$PERSONS.KILLED[i])
}
pn<-rep(0,length(len))
for(i in 1:length(len)){
  pn[i]<-sum(neww$number[which(neww$DATE==len[i])])
  }

p.data<-data.frame(time,pn)
View(p.data)
q<-ts(p.data$pn,start = 2016,frequency = 365)
plot(q)
for (i in 1:2) print(Box.test(q,lag = 6*i))
acf(q)
pacf(q)
q.fit<-auto.arima(q)
q.fit
for (i in 1:2) print(Box.test(q.fit$residuals,lag = 6*i))
a<-arima(q, order=c(0,1,2))
q.fore<-forecast(q.fit,h=30,level=c(99.5))
plot(q.fore)
ts.plot(q,predict(a,n.ahead=30)$pred)

## model 2 
time<-unique(neww$DATE)
fre<-c(rep(0,length(time)))
for(i in 1:length(time)){
  fre[i]<-sum(neww$DATE==time[i])
}
data<-data.frame(time,fre)
View(data)
x<-ts(data$fre)
plot(x)
for (i in 1:2) print(Box.test(x,lag = 6*i))
acf(diff(x))
pacf(diff(x))
x.fit<-auto.arima(x)
x.fit
for (i in 1:2) print(Box.test(x.fit$residuals,lag = 6*i))
b<-arima(x, order=c(5,2,3))
b
for (i in 1:2) print(Box.test(b$residuals,lag = 6*i))
x.fore<-forecast(b,h=30,level=c(99.5))
plot(x.fore)
