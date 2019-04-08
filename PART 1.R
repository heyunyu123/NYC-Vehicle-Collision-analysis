library(stringr)
library(lubridate)
library(ggplot2)
library(ggmap)
 
vehi<-read.csv("/Users/heyunyu/Desktop/5200/database.csv",stringsAsFactors = F,header = T)
## to replace null values with NA
vehi<-read.csv("/Users/heyunyu/Desktop/5200/database.csv",stringsAsFactors = F,header = T,na.strings = "")
## external data
zip<-read.csv("/Users/heyunyu/Desktop/5200/zipcode.csv",header =T )
summary(vehi)

## types of variables
str(vehi) 
str(zip)

## variables of date and time showing up as characters.
vehi$DATE_TIME <- paste(vehi$DATE,vehi$TIME)
vehi$DATE <- mdy(vehi$DATE)
vehi$DATE_TIME <-mdy_hm(vehi$DATE_TIME)
vehi$day <- wday(vehi$DATE_TIME,label = T)
vehi$month <- month(vehi$DATE_TIME,label = T)
vehi$hour <- hour(vehi$DATE_TIME)

## transform variables of persons.injured/killed into numeric data
vehi$PERSONS.INJURED<-as.numeric(vehi$PERSONS.INJURED)
vehi$PERSONS.KILLED<-as.numeric(vehi$PERSONS.KILLED)

#transform variables of borough and more into character data
zip$borough<-as.character(zip$borough)
zip$more<-as.character(zip$more)
##test
class(zip$borough)
class(zip$more)

## check whether the data is duplicated
all.equal(length(unique(vehi$UNIQUE.KEY)),nrow(vehi))

## remove needless variables 
names(vehi)
vehi<-vehi[,-c(2,3,8,14:19)]
##test
names(vehi)

### check typos in strings
unique(vehi$BOROUGH) 
unique(vehi$CROSS.STREET.NAME)
unique(vehi$ON.STREET.NAME)
unique(vehi$OFF.STREET.NAME)
unique(vehi$VEHICLE.1.TYPE)
unique(vehi$VEHICLE.2.TYPE)
unique(vehi$VEHICLE.3.TYPE)
unique(vehi$VEHICLE.4.TYPE)
unique(vehi$VEHICLE.5.TYPE)
unique(vehi$VEHICLE.1.FACTOR)
unique(vehi$VEHICLE.2.FACTOR)
unique(vehi$VEHICLE.3.FACTOR)
unique(vehi$VEHICLE.4.FACTOR)
unique(vehi$VEHICLE.5.FACTOR)

## fix typos
for(i in c(6:8,11:20)){
  vehi[,i]<-str_replace_all(vehi[,i],"  ","")
}
##test
which(str_detect(vehi[,c(6:8,11:20)],"  "))


## to identify invalid data.
# sort the latitude data to see its feature.
head(sort(vehi$LATITUDE),20)
head(sort(vehi$LATITUDE,decreasing = TRUE),20)
# identify invalid data and replace them with NA.
vehi[which(vehi$LATITUDE==0),c("LATITUDE","LONGITUDE")]<-NA 

# sort the longitude data to see its feature.
head(sort(vehi$LONGITUDE),20)
head(sort(vehi$LONGITUDE,decreasing = TRUE),20)
vehi[which(vehi$LONGITUDE<=-200|vehi$LONGITUDE>=-40),c("LATITUDE","LONGITUDE")]<-NA

##outliers
boxplot(vehi$PERSONS.INJURED)
boxplot(vehi$PERSONS.KILLED)

## check missing values
sum(is.na(vehi)) ##too many NA

## to assure there is no seperate data omission in geocode.
sum(is.na(vehi$LATITUDE)==TRUE | is.na(vehi$LONGITUDE)==TRUE)-sum(is.na(vehi$LATITUDE)==TRUE & is.na(vehi$LONGITUDE)==TRUE)
## number of NA of latitude and longitude
sum(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE)

## 1.LATITUDE and LONGITUDE mising values
## use other information (street,zipcode and borough) to impute NA of latitude and longitude.

## firstly use street information to impute NA

## use on.street.name
sum(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE &is.na(vehi$ON.STREET.NAME)==FALSE) 
os<-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE&is.na(vehi$ON.STREET.NAME)==FALSE)
b_os<-sum(is.na(vehi$LATITUDE))

for (i in 1:length(os)){
  lon_lat<-geocode(vehi$ON.STREET.NAME[os[i]])
  vehi$LATITUDE[os[i]]<-lon_lat[2]
  vehi$LONGITUDE[os[i]]<-lon_lat[1]
}
a_os<-sum(is.na(vehi$LATITUDE))
identical(b_os,a_os)

## use cross.street.name
sum(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE &is.na(vehi$CROSS.STREET.NAME)==FALSE) 
cs<-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE&is.na(vehi$CROSS.STREET.NAME)==FALSE)
b_cs<-sum(is.na(vehi$LATITUDE))

for (i in 1:length(cs)){
  lon_lat<-geocode(vehi$CROSS.STREET.NAME[cs[i]])
  vehi$LATITUDE[os[i]]<-lon_lat[2]
  vehi$LONGITUDE[os[i]]<-lon_lat[1]
}
a_cs<-sum(is.na(vehi$LATITUDE))
identical(b_cs,a_cs)

## use off.street.name
sum(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE &is.na(vehi$OFF.STREET.NAME)==FALSE) 
fs<-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE&is.na(vehi$OFF.STREET.NAME)==FALSE)
b_fs<-sum(is.na(vehi$LATITUDE))

for (i in 1:length(fs)){
  lon_lat<-geocode(vehi$OFF.STREET.NAME[fs[i]])
  vehi$LATITUDE[fs[i]]<-lon_lat[2]
  vehi$LONGITUDE[fs[i]]<-lon_lat[1]
}
a_fs<-sum(is.na(vehi$LATITUDE))
identical(b_fs,a_fs)

## secondly use zipcode to impute NA
sum(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE&is.na(vehi$ZIP.CODE)==FALSE)
zc<-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE&is.na(vehi$ZIP.CODE)==FALSE)
b_zc<-sum(is.na(vehi$LATITUDE))

for (i in 1:length(zc)){
  for(j in 1:nrow(zip)){
    if (vehi$ZIP.CODE[zc[i]]==zip$code[j]){
      if(zip$code[j]=="QUEENS"){
        lon_lat<-geocode(zip$more[j])
        vehi$LATITUDE[zc[i]]<-lon_lat[2]
        vehi$LONGITUDE[zc[i]]<-lon_lat[1]
      }else{
        lon_lat<-geocode(zip$borough[j])
        vehi$LATITUDE[zc[i]]<-lon_lat[2]
        vehi$LONGITUDE[zc[i]]<-lon_lat[1]
      }}
    else{
      vehi$LATITUDE[zc[i]]<-vehi$LATITUDE[zc[i]]
      vehi$LONGITUDE[zc[i]]<-vehi$LONGITUDE[zc[i]]
    }
    
  }}
a_zc<-sum(is.na(vehi$LATITUDE))
identical(b_zc,a_zc)

## thirdly use borough to impute NA
bo<-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)&is.na(vehi$BOROUGH)==FALSE)
b_bo<-sum(is.na(vehi$LATITUDE))

for (i in 1:length(bo)){
  lon_lat<-geocode(vehi$BOROUGH[bo[i]])
  vehi$LATITUDE[bo[i]]<-lon_lat[2]
  vehi$LONGITUDE[bo[i]]<-lon_lat[1]
}
a_bo<-sum(is.na(vehi$LATITUDE))
identical(b_bo,a_bo)
# omit mising values of longitude and latitude
vehi<-vehi[-which(is.na(vehi$LATITUDE)==TRUE&is.na(vehi$LONGITUDE)==TRUE),]


## 2.BOROUGH missing values

## to check if zipcode can be used to impute mising values of BOROUGH
sum(is.na(vehi$ZIP.CODE)==FALSE & is.na(vehi$BOROUGH)==TRUE)
l<-which(is.na(vehi$BOROUGH));l

## new data issue:the type changed(list)
vehi$LATITUDE<-as.numeric(vehi$LATITUDE)
vehi$LONGITUDE<-as.numeric(vehi$LONGITUDE)

bna<-which(is.na(vehi$BOROUGH)==TRUE&is.na(vehi$LATITUDE)==FALSE)
b_bna<-sum(is.na(vehi$BOROUGH))
for(i in 1:length(bna)){
  loc<-revgeocode(c(vehi$LONGITUDE[bna[i]],vehi$LATITUDE[bna[i]]))
  for(j in 1:nrow(zip)){
    if(substring(unlist(str_split(loc,","))[3],5,9)==zip$code[j]){
      vehi$BOROUGH[bna[i]]=zip$code[j]
    }else
      vehi$BOROUGH[bna[i]]=vehi$BOROUGH[bna[i]]
  }
}
a_bna<-sum(is.na(vehi$BOROUGH))
identical(b_bna,a_bna)

unique(c(unique(vehi1$VEHICLE.1.TYPE),unique(vehi1$VEHICLE.3.TYPE),
         unique(vehi1$VEHICLE.4.TYPE),unique(vehi1$VEHICLE.5.TYPE),
         unique(vehi1$VEHICLE.2.TYPE)))
