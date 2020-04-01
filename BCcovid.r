
library(tidyverse)
library(ggplot2)
library(forecast)


data1 <- readxl::read_xlsx("BCcovid.xlsx")[,c(1,3)]
data1
tail(data1)
data1 <- as.data.frame(data1)
data1[,1] <- as.Date(data1[,1])
data1[,2] <- as.numeric(data1[,2])
# data1 <- data1[-54,]

data1
fit <- forecast(auto.arima(data1[1:44,2]))
data1 <- cbind(data1,as.data.frame(matrix(ncol=5,nrow=nrow(data1))))
colnames(data1)[3:7] <- c("Pred","Lo80","Hi80","Lo95","Hi95")
data1[45:54,"Pred"] <- fit$mean
data1[45:54,c("Lo80","Lo95")] <- fit$lower
data1[45:54,c("Hi80","Hi95")] <- fit$upper
data1[54,1] <- data1[53,1] +1 
data1

data1
fit <- forecast(auto.arima(data1[,2]))
# data1 <- cbind(data1,as.data.frame(matrix(ncol=5,nrow=nrow(data1))))
colnames(data1)[3:7] <- c("Pred","Lo80","Hi80","Lo95","Hi95")


data1[55:64,"Pred"] <- fit$mean
data1[55:64,c("Lo80","Lo95")] <- fit$lower
data1[55:64,c("Hi80","Hi95")] <- fit$upper

for (i in 55:64){
  data1[i,1] <- data1[i-1,1]+1
}

ggplot(data1[35:63,],aes(x=Date)) + 
  geom_line(aes(y=Case),color="black") +
  geom_line(aes(y=Pred),color="red") +
  geom_line(aes(y=Lo95),color="blue") +
  geom_line(aes(y=Hi95),color="blue") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Prediction of Total Case in BC by Date")
# theme_bw()
data1

###
data1 <- as.data.frame(readxl::read_xlsx("BCcovid.xlsx"))
colnames(data1)

bcforecast <- function(col,d){
  
  data1[,"Date"] <- as.Date(data1[,"Date"])
  data1 %>% select(Date,col) -> data1
  data1
  
  
  date <- as.data.frame(seq.Date(min(data1[,1]),max(data1[,1]),by="1 day"))
  colnames(date) <- c("Date")
  data1 <- left_join(as.data.frame(date),data1,by="Date")
  data1
  data1[,2] <- as.integer(data1[,2])
  
  for (i in 1:nrow(data1)){
    if (is.na(data1[i,2])==TRUE) {
      data1[i,2] <- 0
    }
  }
  
  
  forecast <- forecast(auto.arima(data1[1:(nrow(data1)-d),2]))
  
  nr <- nrow(data1)
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Pred"] <- forecast$mean[1:10]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Lo80"] <- forecast$lower[,1]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Lo95"] <- forecast$lower[,2]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Hi80"] <- forecast$upper[,1]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Hi95"] <- forecast$upper[,2]
  
  for (i in (nr+1):(nrow(data1))){
    data1[i,1] <- data1[i-1,1]+1
  }
  print(data1)
  
  colnames(data1)[2] <- c("Case")
  ggplot(data1[(nrow(data1)-25):(nrow(data1)),],aes(x=Date)) +
    geom_line(aes(y=Case),color="black") +
    geom_line(aes(y=Pred),color="red") +
    geom_line(aes(y=Lo80),color="blue") +
    geom_line(aes(y=Lo95),color="green") +
    geom_line(aes(y=Hi80),color="blue") +
    geom_line(aes(y=Hi95),color="green") +
    theme_bw()
  
}
colnames(data1)
#issues:
# the way missing data is handled should be different depending on the column of interest
# if the column is Deaths, New, Recoveries, then the missing values should be filled with 0
# if the column is Case, then the missing values should be filled with the previous numerical value
