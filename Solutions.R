#### Week 3: ####
# Quiz 1:
for(i in 0:10) { 
  if(i%%2!=0) cat("tu numero es", i , "y tambien ") 
}

# Quiz 2:
notfound<-TRUE 
i<-0 
while(notfound) { 
  if(i%%2!=0) { 
    cat(i) 
    notfound<-FALSE 
  } 
}

# Quiz 4:
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
mean(x)
lapply(x,mean)
lapply(mean(x))

matrix(1:6)

x<-matrix(1:12,4)
x[cbind(c(1,3,2),c(3,3,2))] 
x[c(1,3,2),c(3,3,2)] 

#### Week 4: ####
# Quiz 2:

  matrix(1:9, ncol=3,byrow = T)
  
# Quiz 3:
  x<-1:10
## Impares  
  x[2*(1:5)-1]
## Pares
  x[rep(c(F,T),5)]
  
# Quiz 4:
  x<-matrix(-4:5,nrow=2)
  View(x)
  
# Lab:
  set.seed(9852)
  my.data<-list()
  for(i in 1:100){
    my.data[[i]]<-matrix(rnorm(16),nrow=4)
  }

# 4.1:
my.index<-list()
for(i in 1:100){
  my.index[i]<-(my.data[[i]]<0)
}

# 4.2:
my.negatives<-matrix(rep(0,16),nrow=4)
for(i in 1:100){
  my.negatives<-my.negatives+my.index[[i]]
}
my.negatives

# 4.3:
sum(my.negatives)

# 4.4:
my.negative.values<-numeric(0) 
for(i in 1:100){
my.negative.values<-c(my.negative.values,my.data[[i]][my.index[[i]]])
}

# 4.5:
summary(my.negative.values)
___________________________
#### week 5: ####
# Quiz 5:
# 5.2:
setwd("D:\\")
df1<-read.table("grade.txt", header=T, skip=2, nrow=12,sep=" ")
df2<-read.csv("grade.txt", header=T, nrow=12) # No vale
df3<-read.csv("grade.txt", header=T, skip=2, nrow=12)

# 5.3:
dat.txt<-cat("A B C D", "2.50 3.50 4 2.75", "FALSE FALSE TRUE FALSE")
readLines(dat.txt)
df3<-readLines("grade.txt", header=F, skip=2, nrow=12)
## MAL

# 5.4:
f1<-file("grade.txt", open="r")
scan(f1,what="",nlines=2)

# Lab 5:
# 1:
cd()
dir()
setwd()
getwd()

# 2:
f1<-file("grade.txt", open="r")

# 3:
my.data<-read.table(f1,skip=4,comment.char="%",nrows=7)

# 4:
my.data2<-read.table(f1,skip=3,sep=";",dec=",",nrows=2)

# 5:
my.data3<-read.table(f1,skip=5,na.strings="-9999",sep=",",nrows=2)

# 6:
my.all.data<-rbind(my.data,my.data2,my.data3)

#### Week 6: ####

set.seed(9007)
my.data<-data.frame(x=rnorm(10),y=rnorm(10)+5,z=rchisq(10,1))
additional.data<-data.frame(x=rnorm(3),y=rnorm(3)+5,z=rchisq(3,1))

#New week 6:
sink()
writeLines()
write.table()
dump()
dput()
save()
load()

# Quiz 6:
# 1:
df <- read.table("mtcars.txt",header = TRUE,sep = ",")

m2<-write.table(mtcars,file = "mtcars.txt",sep = ",")

# 2:
d1<-write.table(mtcars,file = df,sep = ",")
d2<-write.csv(mtcars,file = "mtcars.txt")

# Lab:
set.seed(9007)
my.data<-data.frame(x=rnorm(10),y=rnorm(10)+5,z=rchisq(10,1))

# 3:
my.data2<-my.data*10e5

#### Week 7: ####
library(RODBC)
# Connecting to SQL server databases:#

## Open a connection to an ODBC database
odbcDriverConnect()
## Submit a query to an ODBC database and return the results
sqlQuery()
## List tables on an ODBC Connection
sqlTables()
## Read a table from an ODBC database into a data frame
sqlFetch()
## Query column structure in ODBC tables
sqlColumns()
## Close the connection
close(connection)

# Coding:
connStr <- paste(
    "Server=m...database.windows.net",
    "Database=DW",
    "uid=Rlogin",
    "pwd=P@ssw0rd",
    "Driver={SQL Server}",
    sep=";",
    )

conn<-odbcDriverConnect(connStr)

# Connecting to a local SQL Database on your harddisk:
connStr <- paste(
    "Server=My_machine",
    "Database=DW",
    "uid=Rlogin",
    "pwd=P@ssw0rd",
    "Driver={SQL Server}",
    sep=";"
  )
  
conn <- odbcDriverConnect(connStr)

# First Query:
tab <- sqlTables(conn)
head(tab)

# Getting a Table:
mf<-sqlFetch(conn, "bi.manuf")

# Submit real SQL:
query <- "
      SELECT Manufacturer
      FROM   bi.manuf
      WHERE  ManufacturerID < 10
"
sqlQuery(conn, query)

#Large tables:
sqlQuery(conn, "SELECT COUNT(*) FROM bi.salesFact")

## Show some column info:
sqlColumns(conn, "bi.salesFact")[c("COLUMN_NAME", "TYPE_NAME")]

## Show first two rows:
sqlQuery(conn, "SELECT TOP 2 * FROM bi.salesFact")

## Fetch a subset:
df <- sqlQuery(conn, "SELECT * FROM bi.salesFact WHERE Zip='30116'")

# Data types:
## Clases of variables on the R side:
sapply(df, class)

## Example:
df <- sqlQuery(conn, 
            "SELECT AVG(Revenue), STDEV(Renevue), Zip
             FROM   bi.salesFact
             GROUP BY Zip"
            )

colnames(df) <- c("AVG(Revenue"), "STDEV(Revenue)", "Zip")

close(conn)

# Lab 7:
connStr <- paste(
  "Server=msedxeus.database.windows.net",
  "Database=DAT209x01",
  "uid=Rlogin",
  "pwd=P@ssw0rd",
  "Driver={SQL Server}",
  sep=";"
)

conn <- odbcDriverConnect(connStr)


my.data.frame <- sqlQuery(conn,
                          "SELECT SUM(Revenue), SUM(Units), ProductID
                          FROM bi.salesFact
                          WHERE Date > '2013-12-31' AND Date < '2015-01-01'
                          GROUP BY ProductID"
)

names(my.data.frame)<-c("SUM(Revenue)","SUM(Units)","ProductID")

# 4:
my.data.frame$ProductID[order(my.data.frame$"SUM(Units)",decreasing=TRUE)][1:5]

# 5:
my.data.frame$ProductID[order(my.data.frame$"SUM(Revenue)", decreasing = T)][1:5]


  
#### Week 8: ####

## ver funcion "julian()"
# Lab:
set.seed(449)
my.days<-as.Date(sample(18000:20000,20), origin = "1960-01-01")

# 1:
your.days<-c(julian(my.days,origin=as.Date("1960-01-01")))

# 2:
## save the current locale for time objects in old.local
  old.locale<-Sys.getlocale("LC_TIME")
  
## set the locale for time objects to be English
  Sys.setlocale("LC_TIME", "English")
  
## Programming logic below  
  
## set the locale for time objects back to the old settings   
  Sys.setlocale("LC_TIME", old.locale)
  
# 3:
## Instal chron.
  library(chron)
## La siguiente funcion separa en meses, dias y años las fechas que tiene cargadas  
  my.days.structure<-month.day.year(my.days,origin=c(1,1,1960))
  
# 4:
  my.days<-as.Date(my.days, origin = "1960-01-01") 
  my.date.info<-c(Weekday=weekdays(my.dates),my.days.structure)

  my.date.info<-data.frame(Weekday=weekdays(my.dates),my.days.structure)
  View(my.date.info)

#### Week 9: ####
  presidents
  tapply(presidents,cycle(presidents),mean,na.rm=T)
  table(presidents)
  
  sapply(airquality, my.summary)
  
  # Quiz 9:
  # 1:
  colMeans(airquality)
  lapply(airquality, mean) 
  sapply(airquality, mean) # Igual a colMeans

  # 2:
  aggregate(weight~feed, data=chickwts, mean)
  by(chickwts$weight, chickwts$feed, mean)
  
  # 3:
  with(airquality, table(Month, Temp < 65))
  table(LowTemp = airquality$Temp < 65, airquality$Month)

  # 4:
  prop.table(with(airquality, table(Month, Temp > 80)),1)
  prop.table(with(airquality, table(Temp > 80, Month)),2)
  
  # 5:
  lapply(airquality, mean)
  sapply(airquality, mean)
  as.list(sapply(airquality, mean))
  unlist(sapply(airquality, mean))
  is.list(sapply(airquality, mean))
  
  # Lab 9:
  # 1:
  my.data<-data.frame(Treatment=c(rep("A",4),rep("B",4)),
                      Stone=rep(rep(c("Small","Large"),c(2,2)),2),
                      Success=rep(c(1,0),4),
                      Count=c(81,6,192,71,234,36,55,25))
  
  # 2:
  my.table<-xtabs(Count~Treatment+Success+Stone,data=my.data) 
  View(my.table)
  
  # 3.1:
  prop.table(margin.table(my.table, 1:2),1) 
    
  # 4:
  prop.table(my.table[,,2],1)
  
  # 5:
  prop.table(my.table[,,1],1)
  
  # 6:
  prop.table(my.table[1,,],2)
  
  # 7:
  prop.table(my.table[2,,],2)
  
  # 8:
  prop.table(margin.table(my.table, 1:3),1)
  prop.table(margin.table(my.table, c(3,1)),1)
  prop.table(margin.table(my.table, 3:1),2)
  prop.table(margin.table(my.table, c(1,3)),2)


#### Week 10: ####
# Ej 1:
par(mfrow=c(3,3))

set.seed(779)
for(i in 1:9){
  hist(rnorm(25), probability=TRUE,main=paste("Histogram",i))
  curve(dnorm,add=TRUE,col="red",lwd=3)
}

# Ej 2:
....



# Quiz 1:
a<-(3.373546+4.183643+3.164371)/3
b<-(1.164371+2.183643+1.373546)/3
c<-(5.373546+6.183643+5.164371)/3
d<-(-2.835629+ (-1.816357)+(-2.626454))/3

# Quiz 3:
n<-10000 
doone <- function(){ 
  x<-rbinom(1,50,1/6) 
  p<-x/50 
  p 
} 
p.sim<-replicate(n,doone())
mean(p.sim)
sd(p.sim)

# Quiz 4:
n<-100
doone <- function(){ 
  x<-rbinom(1,50,1/6) 
  p<-x/50 
  p 
} 
p.sim<-replicate(n,doone()) 
hist(p.sim,breaks=20)

# Quiz 5:
hist(rnorm(1000, mean=5, sd=2))

#### Lab: #### 
t10<-read.table("https://raw.githubusercontent.com/MicrosoftLearning/Programming-in-R-for-Data-Science/master/10.Simulation/Lab10.csv", header=T, sep=",")

  # 1:
library(data.table)


tabla10$Genotype<-as.character(tabla10$Genotype)
t.test(tabla10$systolic.bp)

  
  # 2:
data1<-t10$systolic.bp[t10$Genotype=="BA"] 
data2<-t10$systolic.bp[t10$Genotype=="BB"] 


  # 3:
testResult <- t.test(data1,data2)

  # 4:
plot

#### Week 11: ####
# Ej 1:
my.analysis<-lm(log(Ozone)~Solar.R+Wind+Temp,data=airquality[airquality>1,])
my.analysis2<-lm(log(Ozone)~Solar.R+Wind+Temp,data=airquality)
## Compare residuals with the normal distribution:
qqnorm(my.analysis$residuals)
sd.1<-sd(my.analysis$residuals)
lines((-3):3,((-3):3)*sd.1,type="l",lwd=3,col="red")

# Ej 2:
my.analysis3<-lm(log(Ozone)~Solar.R+Wind+Temp+
                   Solar.R:Wind+Solar.R:Temp+Wind:Temp,
                 data=airquality[airquality>1,])
## Reduce the model with the drop1()
drop1(my.analysis3, test="F")
summary(my.analysis3)

## Update the model:
my.analysis<-update(my.analysis,~.-Solar.R:Wind)

# Ej 3:
library(glm2)
my.analysis<-glm(satellite~width,family=binomial,data=crab.data)

## predict.glm:
my.linear.predictor<-data.frame(
  prediction=predict(my.analysis,se.fit=TRUE)$fit,
  lower=predict(my.analysis,se.fit=TRUE)$fit-
    1.96*predict(my.analysis,se.fit=TRUE)$se.fit,
  upper=predict(my.analysis,se.fit=TRUE)$fit+
    1.96*predict(my.analysis,se.fit=TRUE)$se.fit)

## Linear predictors:
my.linear.predictor<-my.linear.predictor[order(crab.data$width),]

## Transform to lineal regresion
logistic<-function(x){exp(x)/(1+exp(x))}
my.predictor<-logistic(my.linear.predictor)

## Plot:
plot(sort(crab.data$width),my.predictor$prediction,type="l",
     xlab='width',ylab='p(satellite)')
lines(sort(crab.data$width),my.predictor$upper,type="l",lty=2)
lines(sort(crab.data$width),my.predictor$lower,type="l",lty=2) 

## Summary:
summary(crab.data$width)

## 
my.cut<-cut(crab.data$width,breaks=20+(0:5)*3) 
my.means<-tapply(crab.data$satellite,my.cut,mean) 
lines(20+(0:4)*3+1.5,my.means,type="p",pch=16) 

# Quiz 11:
# 1 and 2:
library(UsingR)
a<-father.son
reg1<-lm(father.son$sheight~father.son$fheight)
summary(reg1)

# 3:
y ~ x + z + w + x:z + x:w + z:w ## Modelo
y ~ x + z*w + x:z + x:w
y ~ x + z + w + z*w + x*z + x*w
y ~ z*w + x*z + x:w

# Lab 11:
install.packages("R330")
library(R330)
wine<-wine.df
View(wine)
# 1:
my.analysis<-lm(wine$price~wine$year + wine$temp + wine$h.rain + wine$w.rain + wine$h.rain:wine$w.rain)
summary(my.analysis)

# 2: 
drop1(my.analysis, test="F")

# 3:
coef(precio)[4]+800*coef(precio)[6] 

# 4:
predict(precio)
m_w_t<-mean(wine$temp)
m_w_hr<-mean(wine$h.rain)
m_w_wr<-mean(wine$w.rain)

# 4.1
newdata <- subset(year=19885,wine, temp >= mean(wine$temp) & h.rain >= mean(wine$h.rain) & w.rain >= mean(wine$w.rain))
predict(precio, temp=mean(wine$temp), h.rain=mean(mean(wine$h.rain)),w.rain=mean(mean(wine$w.rain)))

# Official:
new.data<-data.frame(year=1985,
                     temp=mean(wine.df$temp),
                     h.rain=mean(wine.df$h.rain),
                     w.rain=mean(wine.df$w.rain))

predict(my.analysis,newdata=new.data)


# 5:
log_precio<-lm(log(wine$price) ~ wine$year + wine$temp + wine$h.rain + wine$w.rain + wine$h.rain:wine$w.rain)
summary(log_precio)

# 6:
drop1(log_precio,test="F")

log_precio<-update(log_precio,~.-h.rain:w.rain)
drop1(log_precio,test="F")

# 7:
log_precio2<-update(log_precio,~.-wine$h.rain:wine$w.rain)
summary(log_precio2)

#another option:
my.analysis<-lm(log(price)~year+temp+h.rain+w.rain,data=wine.df)
summary(my.analysis)


# 8: 
new.data<-data.frame(year=1985,
                     temp=mean(wine.df$temp),
                     h.rain=mean(wine.df$h.rain),
                     w.rain=mean(wine.df$w.rain))

exp(predict(my.analysis,newdata=new.data))


#### Week 12: ####
# Quiz 1:
packageurl <- "https://mran.revolutionanalytics.com/snapshot/2015-11-30/bin/windows/contrib/3.2/ggplot2_1.0.1.zip"
install.packages(packageurl, repos=NULL, type="source")
library(ggplot2)

qplot(hp, qsec, data=mtcars, geom=c("point","smooth"), method="lm") 

# Quiz 2:
##color & facets

# Quiz 3:
p<-gplot(data=mtcars)
p<-p+aes(x=qsec, y=hp)
p<-p+geom_point()+geom_smooth(method=lm)
p

# Quiz 4:
hist(airquality$Temp, breaks=10)
qplot(Temp, data=airquality,binwidth=5)

# Quiz 5:
x<-rnorm(1000, mean=-5)
plot(density(x))

ggplot()+aes(x=x)+geom_density()
qplot(x)
qplot(x, geom = "density")

# Lab 12:
my.data<-data.frame(federal.states=c("Baden-Württemberg","Bayern","Berlin",
                                     "Brandenburg","Bremen","Hamburg","Hessen",
                                     "Mecklenburg-Vorpommern","Niedersachsen",
                                     "Nordrhein-Westfalen","Rheinland-Pfalz",
                                     "Saarland","Sachsen","Sachsen-Anhalt",
                                     "Schleswig-Holstein","Thüringen"), 
                    Population=c(10716644,12691568,3469849,2457872,661888,1762791,
                                 6093888,1599138,7826739,17638098,4011582,989035,4055274,
                                 2235548,2830864,2156759))

# 1:
library(ggplot2)
library(ggmap)
str(my.data$federal.states)

my.data$federal.states<-as.character(my.data$federal.states)

# 2:
latlon <- geocode(my.data$federal.states)

# 3:
my.data$federal.states[1]<-"Baden-Wurttemberg"
my.data$federal.states[16]<-"Thuringen Germany"

# 4:
latlon <- geocode(my.data$federal.states)
View(latlon)
my.data<-cbind(my.data,latlon)

my.data$lon <- latlon$lon
my.data$lat <- latlon$lat

# 5:
Germany <- ggmap(get_map(location="Germany",zoom=6), extent="panel")

## Fixed error
library(devtools)
install_github('ggmap','dkahle')

# 6:
circle_scale<-0.000002
Germany+geom_point(aes(x=lon, y=lat),
                   data=my.data,
                   col="red",
                   alpha=0.4,
                   size=my.data$Population*circle_scale)

