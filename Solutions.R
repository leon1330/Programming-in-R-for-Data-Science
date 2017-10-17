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
  
