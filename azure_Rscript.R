a <- maml.mapInputPort(1)
install.packages("dplyr")
library(dplyr)
a$starttime <-a$starttime+28800
a$date <-as.Date(a$starttime)
a <-filter(a, a$tasklocation=="Bedroom")

newdata <-a

newdata$status1 <-"duration"

i=1

while (i<nrow(newdata)) {
  if (difftime(newdata$starttime[i+1],newdata$starttime[i], units = "min")>=30) newdata$status1[i]="start" 
  i=i+1
}
###get the ending of the 30min interval
newdata$status2 <-"duration"
i=1

while (i<nrow(newdata)) {
  if (difftime(newdata$starttime[i+1],newdata$starttime[i], units = "min")>=30) newdata$status2[i+1]="end" 
  i=i+1
}
str(newdata)
head(newdata)
###############################################
calendar <-table(newdata$date)
calendar <-data.frame(calendar)
names(calendar) <-c("date", "freq")
calendar$date <-as.Date(calendar$date)

###################
b <-calendar
b$`wake up` <-paste(b$date, "08:30:01")
b$`wake up` <-strptime(b$`wake up`, "%Y-%m-%d %H:%M:%S")
b$`wake up` <-as.POSIXct(b$`wake up`)


b <-b[,-2]
#######################
####
series1 <-filter(newdata, newdata$date==calendar$date[1])

starting <-filter(series1, series1$status1=="start")
ending <-filter(series1, series1$status2=="end")


newdata$time
starting <-filter(starting, starting$starttime<b$`wake up`[i] )
ending <-filter(ending, ending$starttime<b$`wake up`[i])

b$`wake up`[1] <-tail(ending$starttime,1)

flag <-paste(b$date[1], "05:00:00")
flag <-strptime(flag, "%Y-%m-%d %H:%M:%S")

if (nrow(ending)==0) b$`wake up`[1] = head(series1$time,1)

if (b$`wake up`[1] <flag) series1 <-filter(series1,series1$time>flag)
if (b$`wake up`[1] <flag) b$`wake up`[1] = head(series1$time,1)
######
############
i=2
j=nrow(calendar)+1
while (i <j){
  series1 <-filter(newdata, newdata$date==calendar$date[i])
  starting <-filter(series1, series1$status1=="start")
  ending <-filter(series1, series1$status2=="end")
  
  
  starting <-filter(starting, starting$starttime<b$`wake up`[i] )
  ending <-filter(ending, ending$starttime<b$`wake up`[i])

  flag <-paste(b$date[i], "05:00:00")
  flag <-strptime(flag, "%Y-%m-%d %H:%M:%S")
  
  b$`wake up`[i] <-tail(ending$time,1)
  
  if (nrow(ending)==0) b$`wake up`[i] = head(series1$time,1)
  
  if (b$`wake up`[i] <flag) series1 <-filter(series1,series1$time>flag)
  if (b$`wake up`[i] <flag) b$`wake up`[i] = head(series1$time,1)

  i=i+1
}
############
#######################
b$sleep <-paste(b$date, "20:59:59")
b$sleep <-strptime(b$sleep, "%Y-%m-%d %H:%M:%S")
b$sleep <-as.POSIXct(b$sleep)

#find the real sleep time
i=2
j=nrow(calendar)+2
while (i <j){
  series1 <-filter(newdata, newdata$date==calendar$date[i]|newdata$date==calendar$date[i-1])
  
  starting <-filter(series1, series1$status1=="start")
  ending <-filter(series1, series1$status2=="end")
  
  
  ending <-filter(ending, ending$starttime>b$sleep[i-1] & ending$starttime<b$`wake up`[i])
  starting <-filter(starting, starting$starttime>b$sleep[i-1] & starting$starttime <b$`wake up`[i])
  
  b$sleep[i-1] <-head(starting$starttime,1)
  series1 <-filter(newdata, newdata$date==calendar$date[i-1])
  if (nrow(starting)==0) b$sleep[i-1] = tail(series1$starttime,1)
  i=i+1
}
#######################
i=1
j=nrow(calendar)+1
while (i<j) {
  series1 <-filter(newdata, newdata$date==calendar$date[i])
  if(nrow(series1)==0) b$`wake up`[i]=NA 
  if(nrow(series1)==0) b$sleep[i]=NA
  i=i+1
}
#######################
b
#######################
maml.mapOutputPort("b")