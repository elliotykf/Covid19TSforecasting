library(ggplot2)
library(tidyverse)
library(forecast)

###dry run
#notes on each step are in the functions 
covid <- read.csv("covid19.csv")
covid$date <- as.Date(covid$date,format="%d-%m-%Y")
covid %>% 
  filter(prname=="Canada") %>%
  select(date, numconf) -> covidCanada

dcovid <- as.data.frame(seq.Date(min(covid$date),max(covid$date),by="day"))
colnames(dcovid) <- c("date")

covidCanada <- left_join(dcovid,covidCanada,by="date") %>% rename(case=numconf)

x <- NULL
for (i in 1:nrow(covidCanada)){
  if (class(covidCanada[i,2])=="integer" & !is.na(covidCanada[i,2])){
    x <- covidCanada[i,2]
  }
  if (is.na(covidCanada[i,2])==TRUE){
    covidCanada[i,2] <- x
  }
}

covidCanada[,"Pred"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
covidCanada[,"Lo80"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
covidCanada[,"Hi80"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
covidCanada[,"Lo95"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
covidCanada[,"Hi95"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))


for (i in 1:20){
  fit <- forecast(auto.arima(covidCanada[1:40+i,2]))
  covidCanada[40+i,"Pred"] <- fit$mean[1]
  covidCanada[40+i,c("Lo80","Lo95")] <- fit$lower[1,1:2]
  covidCanada[40+i,c("Hi80","Hi95")] <- fit$upper[1,1:2]
}
ggplot(covidCanada,aes(x=date)) + 
  geom_line(aes(y=case)) +
  geom_line(aes(y=Pred),color="red") +
  geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
  theme_bw()
covidCanada



forecast <- forecast(auto.arima(covidCanada[1:59,2]))

covidCanada[60:69,"Pred"] <- forecast$mean
covidCanada[60:69,c("Lo80","Lo95")] <- forecast$lower[,1:2]
covidCanada[60:69,c("Hi80","Hi95")] <- forecast$upper[,1:2]

for (i in 60:69){
  covidCanada[i,1] <- covidCanada[i-1,1]+1 
}
ggplot(covidCanada[40:69,],aes(x=date)) + 
  geom_line(aes(y=case)) +
  geom_line(aes(y=Pred),color="red") +
  geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
  geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
  theme_bw()

covidCanada

###extractor function 
#takes in the name of the province as a string
#outputs the dataframe with the cases in the province by date 
extract <- function(province){
  covid %>% 
    filter(prname==province) %>%
    select(date, numconf) -> covidCanada
  
  #the data source excluded the days where there's no change in number
  #autofill a dataframe with the dates from the first day to the current day
  dcovid <- as.data.frame(seq.Date(min(covid$date),max(covid$date),by="day"))
  colnames(dcovid) <- c("date")
  
  #merge the case data into the dataframe with all dates 
  covidCanada <- left_join(dcovid,covidCanada,by="date") %>% rename(case=numconf)
  #after merging, the days where there's no change in number will have NA values
  
  ##fill the NA values 
  #initialize value-storage variable x
  x <- NULL
  #some provinces have 0 case on the first day of all, so after the merge the value will be NA on the first day
  #if the value is NA then fill the cell with 0 until an actual value is encountered
  if (is.na(covidCanada[1,2])==TRUE){
    x = 0
  }
  #check the cells for actual numerical values and not NA
  for (i in 1:nrow(covidCanada)){
    if (class(covidCanada[i,2])=="integer" & !is.na(covidCanada[i,2])){
      #if the cell actually contains a value then the value-storage variable will change to that value
      x <- covidCanada[i,2]
    }
    #if the cell has NA value, then fill it with whatever value saved in the value-storage variable
    #either 0 or an actual value
    if (is.na(covidCanada[i,2])==TRUE){
      covidCanada[i,2] <- x
    }
  }
  
  #create columns to contain the forecast
  #not the most convenient way to add more columns to a dataframe...
  #the *forecast* function from the package *forecast* will be used
  #the *forecast* function will compute these values that will be useful on the plot
  covidCanada[,"Pred"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
  covidCanada[,"Lo80"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
  covidCanada[,"Hi80"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
  covidCanada[,"Lo95"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
  covidCanada[,"Hi95"] <- as.data.frame(matrix(ncol=1,nrow=nrow(covidCanada)))
  
  #not necessary but good to have a look at the created dataframe...
  print(covidCanada)
}

#model training function with data
#not really useful... but could probably tell whether the model is underestimating or overestimating from here... 
#the extractor function is called in here so there's no need to call the extractor function and save the dataframe in an object
trainForecast <- function(covidCanada){
  #extract the dataframe using the name of the province in a string
  covidCanada <- extract(covidCanada)
  
  #the number of rows of the dataset
  #useful for indexing in loops
  nrow <- nrow(covidCanada)
  
  #use 20 less data to train model
  for (i in 1:20){
    #use auto arima to automatically select the model with smallest aic
    #new models will be trained whenever a new value is incorporated
    #not sure if it's a good practice...
    #save forecast object for data extraction later 
    fit <- forecast(auto.arima(covidCanada[1:(nrow-20)+i,2]))
    
    #the predicted value of the first upcoming datapoint is in the $mean[1] of the forecast object
    covidCanada[(nrow-20)+i,"Pred"] <- fit$mean[1]
    #extract lower CI at 80% and 95% 
    covidCanada[(nrow-20)+i,c("Lo80","Lo95")] <- fit$lower[1,1:2]
    #same thing but upper CI
    covidCanada[(nrow-20)+i,c("Hi80","Hi95")] <- fit$upper[1,1:2]
  }
  
  #not necessary but why not have a look at the df to check if the values are properly stored in
  print(covidCanada)
  
  #now plotting
  #start the plot from the preceeding 30 points
  ggplot(covidCanada[(nrow-30):nrow,],aes(x=date)) +
    geom_line(aes(y=case)) +
    geom_line(aes(y=Pred),color="red") +
    geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
    #personal favourite theme in ggplot
    theme_bw()
}


##actual forecaster function
#extractor function embedded so no need to call it separately 
forecaster <- function(covidCanada){
  #get the dataframe using the name of the province in a string
  covidCanada <- extract(covidCanada)
  #the number of observations in the data, for indexing purpose later
  nrow <- nrow(covidCanada)
  #save the forecast object from the model training 
  #the model uses 1 less data point than the actual number of the observation.
  #when forecasting, the first forecast value will be an already-made observation
  #not sure how useful it is in terms of checking model performance... but why not
  forecast <- forecast(auto.arima(covidCanada[1:(nrow-1),2]))
  
  #the forecast is made on 10 values at a time
  #since one observation was taken off, the first forecast will be for that observation
  #so there's only 9 new forecast left
  #hence indexing +9 
  covidCanada[nrow:(nrow+9),"Pred"] <- forecast$mean
  covidCanada[nrow:(nrow+9),c("Lo80","Lo95")] <- forecast$lower[,1:2]
  covidCanada[nrow:(nrow+9),c("Hi80","Hi95")] <- forecast$upper[,1:2]
  
  #autofill the dates for the new forecast 
  for (i in nrow:(nrow+9)){
    covidCanada[i,1] <- covidCanada[i-1,1]+1 
  }
  
  #again not necessary but why not take a look at the numbers and look at the graph later... 
  print(covidCanada)
  
  #plotting
  #show 10 preceeding values up to the newest forecasted value
  ggplot(covidCanada[(nrow-10):(nrow+9),],aes(x=date)) + 
    geom_line(aes(y=case)) +
    geom_line(aes(y=Pred),color="red") +
    geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
    theme_bw()
}

###forecaster function with an option to choose how many days to go back when training the model for forecasting 
forecasterDB <- function(covidCanada,db){
  covidCanada <- extract(covidCanada)
  nrow <- nrow(covidCanada)
  val <- nrow-db
  forecast <- forecast(auto.arima(covidCanada[1:val,2]))
  
  covidCanada[val:(val+9),"Pred"] <- forecast$mean
  covidCanada[val:(val+9),c("Lo80","Lo95")] <- forecast$lower[,1:2]
  covidCanada[val:(val+9),c("Hi80","Hi95")] <- forecast$upper[,1:2]
  
  for (i in val:(val+9)){
    covidCanada[i,1] <- covidCanada[i-1,1]+1 
  }
  
  print(covidCanada)
  
  ggplot(covidCanada[(val-10):(val+9),],aes(x=date)) + 
    geom_line(aes(y=case)) +
    geom_line(aes(y=Pred),color="red") +
    geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
    theme_bw()
}
