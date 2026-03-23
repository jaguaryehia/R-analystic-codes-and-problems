date<-function(d){
  df2 <- d[order(d$dob),]
  df3<-head(df2,3)
  df3<-subset(df3, select = c("dob","gender", "ancestry","avg_commute"))
  return(df3)
}

top<-function(data){
  top<-head(data,10)
  return(top)
}

bottom<-function(data){
  bott<-tail(data,10)
  return(bott)
}

chlid<-function(d){
  df2<-d[d$children>2,]
  df3<-subset(df2,select = c("children","gender","ancestry","avg_commute","daily_internet_use","disease"))
  return(df3)
  
}

missOrNot<-function(d){
  df2<-rowSums(is.na(d))
  df3<-table(df2)
  return(df3)
} 

summarization<-function(d){
  summary(d)
}

misscol<-function(d){
  x<-names(which(colSums(is.na(d))>0))
  return(x)
}

remove<-function(d){
  r<-d[complete.cases(d[,c("id","gender","dob","zipcode","employment_status","daily_internet_use","disease","education","marital_status","children","ancestry","avg_commute")]),]
  return(r)
}

avg<-function(d){
  x<-d %>% group_by(education) %>% summarise_at(vars(daily_internet_use), list(average = mean))
  return(x)

}

histdigram<-function(d){
  x<-hist(d$children,xlab ="count Chlidren",breaks = 5)
  return(x)
}

hist2<-function(d){
  x<-d[d$gender == 'male', ]
  m<-x$avg_commute
  y<-d[d$gender=='female', ]
  f<-y$avg_commute
  par(mfrow=c(1,2))
  plot(density(m),col=2)
  plot(density(f),col=5)
}

genderhist<-function(d){
  z<-d$gender
  x<-as.factor(z)
  par(mfrow=c(1,2))
  hist(table(x),freq = TRUE,xlab = levels(x),ylab = 'frequency' )
  barplot(prop.table(table(z)))    
}

library(dplyr)
df<-read.csv("C:/Users/jaguar/Desktop/data.csv")
df1<-data.frame(df)
z<-df1[df1$gender,]
x<-df1 %>% group_by(df1$gender) 
x
x<-as.factor(z)
par(mfrow=c(1,2))
hist(table(x),freq = TRUE,xlab = levels(x),ylab = 'frequency' )
barplot(prop.table(table(z)))


