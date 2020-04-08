library(ggplot2)
library(tidyverse)
library(forecast)
library(rvest)
library(stringr)

###dry run
#notes on each step are in the functions 
covid <- read.csv('https://health-infobase.canada.ca/src/data/covidLive/covid19.csv')
covid$date <- as.Date(covid$date,format="%d-%m-%Y")
covid %>% 
  filter(prname=="Canada") %>%
  select(date, numconf) -> covidCanada
covidCanada
dcovid <- as.data.frame(seq.Date(min(covid$date),max(covid$date),by="day"))
colnames(dcovid) <- c("date")

covidCanada <- left_join(dcovid,covidCanada,by="date") %>% rename(case=numconf)
covidCanada$case <- as.integer(as.character(covidCanada$case))
x <- NULL
for (i in 1:nrow(covidCanada)){
  if (class(covidCanada[i,2])=="integer" & !is.na(covidCanada[i,2])){
    x <- covidCanada[i,2]
  }
  if (is.na(covidCanada[i,2])==TRUE){
    covidCanada[i,2] <- x
  }
}

mode(covidCanada)

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
#takes in the name of the province and the name of the columns, both in the string form
#outputs the dataframe with the cases variable of interest in the province by date 
extract <- function(pn,cn){
  covid <- read.csv('https://health-infobase.canada.ca/src/data/covidLive/covid19.csv')
  covid$date <- as.Date(covid$date,format="%d-%m-%Y")
  
  covid %>% 
    filter(prname==pn) %>%
    select(date, cn) -> covidCanada
  
  #the data source excluded the days where there's no change in number
  #autofill a dataframe with the dates from the first day to the current day
  dcovid <- as.data.frame(seq.Date(min(covid$date),max(covid$date),by="day"))
  colnames(dcovid) <- c("date")
  
  #merge the case data into the dataframe with all dates 
  covidCanada <- left_join(dcovid,covidCanada,by="date") %>% rename(case=cn)
  #after merging, the days where there's no change in number will have NA values
  
  #convert the case column from factor to numeric integer 
  covidCanada$case <- as.integer(as.character(covidCanada$case))
  
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
trainForecast <- function(pn,cn){
  #embed extractor function
  #extractor function takes name of province and name of column both in strings
  covidCanada <- extract(pn,cn)
  
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
forecaster <- function(pn,cn){
  #embed extractor function
  #extractor function takes name of province and name of column both in strings
  covidCanada <- extract(pn,cn)
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
#6 days of lag in data confirmation was noted in the source
#meaning the data point from up to 6 days before the current date until the current date may be unreliable and is probably not useful as training data
forecasterDB <- function(pn,cn,db){
  #embed extractor function 
  #extractor function takes the name of the province and the name of the column, both in string 
  covidCanada <- extract(pn,cn)
  #record the number of rows of the subsetted dataset for indexing purpose
  nrow <- nrow(covidCanada)
  #this is the index of the data which the user will use to train the model
  #get it by subtracting the defined day lags (db) from the number of total observation in the dataframe 
  val <- nrow-db
  #subset the training data and fit the model
  forecast <- forecast(auto.arima(covidCanada[1:val,2]))
  
  #saving forecast and CI values into the dataframe 
  covidCanada[val:(val+9),"Pred"] <- forecast$mean
  covidCanada[val:(val+9),c("Lo80","Lo95")] <- forecast$lower[,1:2]
  covidCanada[val:(val+9),c("Hi80","Hi95")] <- forecast$upper[,1:2]
  
  #autofill the dates for the new forecast values
  for (i in val:(val+9)){
    covidCanada[i,1] <- covidCanada[i-1,1]+1 
  }
  
  #display the data table
  print(covidCanada)
  
  #plot 
  ggplot(covidCanada[(val-10):(val+9),],aes(x=date)) + 
    geom_line(aes(y=case)) +
    geom_line(aes(y=Pred),color="red") +
    geom_line(aes(y=Lo80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi80), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Lo95), color = "blue", linetype = "dashed") +
    geom_line(aes(y=Hi95), color = "blue", linetype = "dashed") +
    theme_bw()
}


###Forecaster function using data from the epidemiological summary of canada
#dti = data table of interest: "N" = new cases table, "C" = cumulative cases table
#d = number of days to go back from current date to include as the training data for forecasting 
Cforecaster <- function(dti,d){
  #scrape data table from government of canada covid19 epidemiological summary website 
  link <- 'https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/health-professionals/epidemiological-summary-covid-19-cases.html'
  web <- read_html(link)
  
  #user can choose which table to read and analyze (N or C)
  #N = new cases; C = Cumulative cases 
  # if (dti=="N"){
    #New cases table is in the 2nd html table 
  data1 <- (html_table(web,fill=TRUE)[dti])
  # } else if (dti=="C"){
    #Cumulative cases table is in the 1st html table
    # data1 <- (html_table(web,fill=TRUE)[1])
  # }
  
  #convert to data frame
  data1 <- as.data.frame(data1)
  colnames(data1) <- c("Date","Case")
  #convert character to date
  data1[,1] <- as.Date(data1[,1])

  #cumulative cases number over 1000 is recorded with a comma
  if (dti=="C"){
    #strip the comma and convert to numeric before analysis 
    data1[,2] <- as.numeric(gsub(",","",data1[,2]))
  }
  #forecast
  #user defines the number of days to go back, this sets the last data point to be included in the training data 
  forecast <- forecast(auto.arima(data1[1:(nrow(data1)-d),2]))
  #save the number of rows in data1 for indexing purpose 
  nr <- nrow(data1)
  #there are 10 forecasted values each time forecast is called 
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
Cforecaster(1,3)

forecasterDB("British Columbia","numtoday",1)


colnames(covid)
covid %>% filter(prname %in% c("British Columbia","Ontario","Quebec"))  %>% select(prname,date,numconf,numdeaths,numtotal,numtested,numtoday) -> df
ggplot(df,aes(x=date,y=numtotal,group=prname,color=prname)) + geom_line() + theme_bw()

colnames(covid)

head(covid)
unique(newdf$prname)
covid %>%
  mutate(prname=str_replace_all(prname,"British Columbia","BC")) %>%
  mutate(prname=str_replace_all(prname,"Alberta","AB")) %>% 
  mutate(prname=str_replace_all(prname,"Quebec","QC")) %>% 
  mutate(prname=str_replace_all(prname,"Ontario","ON")) %>%
  mutate(prname=str_replace_all(prname,"Saskatchewan","SK")) %>% 
  mutate(prname=str_replace_all(prname,"Manitoba","MB")) %>%
  mutate(prname=str_replace_all(prname,"New Brunswick","NB")) %>% 
  mutate(prname=str_replace_all(prname,"Newfoundland and Labrador","NL")) %>%
  mutate(prname=str_replace_all(prname,"Nova Scotia","NS")) %>% 
  mutate(prname=str_replace_all(prname,"Nunavut","NU")) %>% 
  mutate(prname=str_replace_all(prname,"Northwest Territories","NT")) %>% 
  mutate(prname=str_replace_all(prname,"Prince Edward Island","PE"))  %>%
  filter(prname!= "Repatriated travellers") %>%
  filter(prname!= "Repatriated Travellers") %>% 
  filter(prname!="Canada") %>%
  select(date,prname,numconf,numdeaths,numtoday) %>%
  rename(`Number of Death` = numdeaths,
         `Total Cases` = numconf,
         `New Cases` = numtoday) -> newdf

gather(newdf,"category","Cases",-date,-prname) -> newdf
labels <- seq.Date(min(covid$date), max(covid$date), length.out=10)

ggplot(newdf,aes(x=date,y=Cases,color=prname)) + 
  geom_line() + 
  facet_grid(category~prname,scales="free") +
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) + 
  scale_x_discrete(breaks=labels, labels=as.character(labels))
labels
# and set breaks and labels
p <- p + scale_x_discrete(breaks=labels, labels=as.character(labels))
p