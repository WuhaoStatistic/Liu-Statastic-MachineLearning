set.seed(1234567890)
library(geosphere)
library(ggplot2)
stastions <- read.csv('stations.csv')
temps50k <- read.csv('temps50k.csv')
st <- merge(stastions,temps50k,by = 'station_number')
class(st[,'date'])
# first parameter is from data
# second parameter is point of interest
d1 <- function(p1,p2)
{
  return(distHaversine(p1,as.matrix(p2,ncol=2)))
}

d2 <- function(day1,day2)
{
  return(as.numeric(difftime(as.POSIXct(day1,format="%Y-%m-%d"),as.POSIXct(day2,format="%Y-%m-%d"),units="days")))
}

d3 <- function(time1,time2)
{
  return(as.numeric(difftime(as.POSIXct(time1,format="%H:%M:%S"),as.POSIXct(time2,format="%H:%M:%S"),units="hours")))
}

# this function is about transforming the data into certain form that can be used in computing kernal value.
# lonti and lati should be numeric descriabing the location
# date is character like 'yyyy-mm-dd'
# time is character like 'hh:mm:ss'(24 hours a day)

build_point <- function(lonti,lati,date,time)
{
  return(c(lonti,lati,date,time))
}

# data 1 is built from build_point
# data 2 is observation from  data
kernal_value <- function(data1,data2,h_dis=1000000,h_date=400,h_time=4)
{
  k1 <- exp(-(d1(as.numeric(data1[1:2]),data2[,5:4])/h_dis)**2)
  
  k2 <- exp(-(d2(data1[3],data2[,9])/h_date)**2)
  
  k3 <- exp(-(d3(data1[4],data2[,10])/h_time)**2)
  
  ks <- k1+k2+k3
  
  km <- k1*k2*k3
  
  return(c(sum(ks*data2[,11])/sum(ks),sum(km*data2[,11])/sum(km)))
}

  
times <- c("04:00:00", "06:00:00","08:00:00" ,"10:00:00","12:00:00","14:00:00", 
           "16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")

temp1 <- c()
temp2 <- c()
for (i in times) 
{
  data1 <- build_point(55.4274,12.826,'2013-11-04',i)
  data2 <- st[st$date < '2013-11-04',]
  data2 <- rbind(data2,st[st$date == '2013-11-04' && d3(st$time,i)>0])
  temp1 <- c(temp1,kernal_value(data1,data2)[1])
  temp2 <- c(temp2,kernal_value(data1,data2)[2])
}

df <- data.frame(times=times, temp1=temp1,temp2=temp2)
p1<-ggplot(df,aes(x=times, y=temp1))+
  geom_point()+
  labs(title="sum")+
  theme(plot.title = element_text(hjust = 0.5))
p2<-ggplot(df,aes(x=times, y=temp2))+
  geom_point()+
  labs(title="multip")+
  theme(plot.title = element_text(hjust = 0.5))
plot(gridExtra::arrangeGrob(p1,p2))
