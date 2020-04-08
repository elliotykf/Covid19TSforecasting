
library(tidyverse)
library(ggplot2)
library(forecast)
library(rvest)
#dry run 
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



###forecaster function for BC
#automatically scrape data from the wikipedia page and do model fitting, forecasting and graphing 
##takes 2 arguments: 
#col = the column of interest. taken in a string form when entering the argument 
#d = the number of days to go back to. taken in as a numeric input 
bcforecast <- function(col,d){
  ##automatic webscraping from wikipedia page
  link <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_British_Columbia"
  web <- read_html(link)
  
  #the 4th html table is the table of the data
  sd <- (html_table(web,fill=TRUE)[4])
  
  #convert to dataframe from list 
  dt <- as.data.frame(sd[[1]])
  
  ##rename the columns 
  for (i in 1:length(colnames(dt))){
    # print(colnames(dt)[i])
    if (colnames(dt)[i]==""){
      colnames(dt)[i] <- colnames(dt)[i-1]
      print(colnames(dt)[i])
      # colnames(dt)[i] <- paste(dt[1,i],colnames(dt)[i])
    }
  }
  
  for (i in 2:length(colnames(dt))){
    colnames(dt)[i] <- paste(dt[1,i],colnames(dt[i]))
  }
  dt <- dt[,!(grepl(colnames(dt)[3], colnames(dt)))]
  
  # 
  # for (i in 1:length(colnames(dt))){
  #   #the first row of observation is the sub-name of the column
  #   #concatenate the sub name with the column name and put that as the actual column name
  #   if (colnames(dt)[i] %in% c("Cases","Deaths","Recoveries")){
  #     colnames(dt)[i] <- paste(dt[1,i],colnames(dt)[i])
  #   }
  # }
  
  #remove the first observation which contains the sub name 
  dt %>% filter(Date != "Date") -> data1
  
  #numbers above a thousand is recorded with a comma, it cannot be converted to numeric automatically
  #same issue for numbers recorded with % 
  #convert them into numeric before proceeding 
  
  #check each column whether they contain , or % 
  for (i in 1:ncol(data1)){
    # if the column contains more than 1 observation that has ,
    if (sum(grepl(",",data1[,i]))>=1){
      #strip , and convert to numeric
      data1[,i] <- as.numeric(gsub(",","",data1[,i]))
    }
    # if the column contains more than 1 observation that has %
    if (sum(grepl("%",data1[i]))>=1){
      #strip %, convert to numeric and divide by 100
      data1[,i] <- as.numeric(gsub("%","",data1[,i]))/100
    }
  }
  
  #convert the date column into date datatype 
  data1[,"Date"] <- as.Date(data1[,"Date"])
  #choose whatever column of interests when calling the function and the column will be merged with the date column
  data1 %>% select(Date,col) -> data1

  ##change the data column
  #the rows where the date is filled by "number of days" became NA after the conversion
  #and NA is considered the max value 
  #so, remove the NA before autofilling the dates 
  date <- na.omit(data1[,1])
  #autofill the dates from the first day to the current day
  date <- as.data.frame(seq.Date(min(date),max(date),by="1 day"))
  #rename the column name for table joining later 
  colnames(date) <- c("Date")
  #join the auto-filled date df and the actual df which contains a column of interest 
  data1 <- left_join(as.data.frame(date),data1,by="Date")
  #convert the case column into integer type 
  data1[,2] <- as.integer(data1[,2])
  
  #check the column of interest to decide how to handle missing data
  #if the column of interest from the input is Total
  #then autofill missing data with the previously encountered numerical value
  if (col %in% c("Total Cases","Total Deaths", "Total Recoveries")){
    
    x <- NULL
    #some provinces have 0 case on the first day of all, so after the merge the value will be NA on the first day
    #if the value is NA then fill the cell with 0 until an actual value is encountered
    if (is.na(data1[1,2])==TRUE){
      x = 0
    }
    #check the cells for actual numerical values and not NA
    for (i in 1:nrow(data1)){
      if (class(data1[i,2])=="integer" & !is.na(data1[i,2])){
        #if the cell actually contains a value then the value-storage variable will change to that value
        x <- data1[i,2]
      }
      #if the cell has NA value, then fill it with whatever value saved in the value-storage variable
      #either 0 or an actual value
      if (is.na(data1[i,2])==TRUE){
        data1[i,2] <- x
      }
    } #if the column of interest is not Total, then it'll be New
  } else {
    #autofill all missing data with 0
    for (i in 1:nrow(data1)){
      if (is.na(data1[i,2])==TRUE) {
        data1[i,2] <- 0
      }
    }
  }
  #auto arima model fitting using the data
  #user define the number of days 
  #which is the number of days that we want to go back 
  #a questionable 6-days lag in data updating was noted on the national website, not sure how relevant it is for the provincial data but might as well
  #so if d=6, data from day1 until 6 days prior to the current date will be included in the training data
  forecast <- forecast(auto.arima(data1[1:(nrow(data1)-d),2]))
  #save the number of rows of this dataframe for indexing purpose later 
  #for calculating the index of where the forecast value should be placed in
  nr <- nrow(data1)
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Pred"] <- forecast$mean[1:10]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Lo80"] <- forecast$lower[,1]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Lo95"] <- forecast$lower[,2]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Hi80"] <- forecast$upper[,1]
  data1[(nr-d+1):(nr-d+length(forecast$mean)),"Hi95"] <- forecast$upper[,2]
  #autofill the forecasted days 
  for (i in (nr+1):(nrow(data1))){
    data1[i,1] <- data1[i-1,1]+1
  }
  #take a look at the data
  print(data1)
  print(dt)
  #rename the user-selected column to Case 
  #probably not necessary if i just changed the y in ggplot 
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

bcforecast("Total Recoveries",2)


##automatic webscraping from wikipedia page
link <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_British_Columbia"
web <- read_html(link)

#the 4th html table is the table of the data
sd <- (html_table(web,fill=TRUE)[4])

#convert to dataframe from list 
dt <- as.data.frame(sd[[1]])
head(dt,1)
colnames(dt)[1]

for (i in 1:length(colnames(dt))){
  if (!(colnames(dt)[i]==dt[1,i])){
  }
  else{
  }
}


colnames(dt)
##rename the columns 
for (i in 1:length(colnames(dt))){
  # print(colnames(dt)[i])
  if (colnames(dt)[i]==""){
    colnames(dt)[i] <- colnames(dt)[i-1]
    print(colnames(dt)[i])
    # colnames(dt)[i] <- paste(dt[1,i],colnames(dt)[i])
  }
}

for (i in 2:length(colnames(dt))){
  colnames(dt)[i] <- paste(dt[1,i],colnames(dt[i]))
}
dt <- dt[,!(grepl(colnames(dt)[3], colnames(dt)))]
dt
# 