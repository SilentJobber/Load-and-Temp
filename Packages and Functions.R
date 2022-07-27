#__ Needed Librarys __####
library(ggplot2)
library(lubridate)  #Used for DateTime
library(timeDate) #Time Date has to be loaded in after lubridate. If this is not done the function "hour()" and other date fucntions will not work past year 2038. (look up 32bit Integer for more information)
library(smooth)
library(dummies)  #https://cran.r-project.org/web/packages/dummies/dummies.pdf
library(readr)
library(RcppRoll) #Used for roll_mean (Moving average)
library(stringi)
library(stringr)
library(bit64)
library(openxlsx)
library(tidyverse)
library(tidyr)



# Working Directory Map List ####
# Make a list of all the possible working dir's based on a starting path
### Map of all folders in a base directory

#basedir = getwd()
wd_setup <- function(basedir,save.levels = 2){
  #browser()
  dirs <- list.dirs(basedir)
  
  wds <- strsplit(dirs,split = "/")
  baselength <- length(unlist(strsplit(basedir,split = "/")))
  endlength <- baselength + save.levels
  
  
  
  wds <- lapply(wds,function(x)(x[1:endlength]))
  wds <- lapply(wds, function(x)(x[!is.na(x)]))
  wds <- unique(wds)

  foldernames <- unlist(lapply(wds, function(x)tail(x,1)))
  wdm <- lapply(wds,function(x)paste(x,collapse = "/"))
  names(wdm) <- foldernames
  return(wdm)
}

vector.probability.v3 <- function(x, sample.size = 10000,key.cols = c("UNIT.ID","Month","Hour")) {
  #     x = Solar.prob
  #     x = Sys.prob
  #     sample.size = 10000
  #     key.cols = c("UNIT.ID","Month","Hour")  
  #     key.cols = c("UNIT.ID")  
  #     Cap.col = "Interval.Capacity"
  #browser()
  ##### 
  #x must include the following columns (UNIT.ID,Vector,Interval.Capacity,Capacity,EFOR)  and may include (Month,Hour)
  # - UNIT.ID = character that defines or seperated more then one Unit or Intertie
  # - Vector = integer sequence starting at Vector 0 (Which represents the likelyhood of the outcome to be 0MW)
  # - Interval.Capacity = The resolution of the model from Vector[t] - Vector[t-1]
  # - Interval.EFOR = The estimate possibility the vector will not be available
  # - Month = integer value of a month
  # - Hour = integer value of hour
  ##### Notes about data structure
  sample.size <- min(100000,sample.size)
  filter.list <- x %>% group_by_at(key.cols) %>% group_split()
  units <- unique(select(x,all_of(key.cols)))
  
  list.lg = length(filter.list)
  i <- 14
  results.ls <- list()
  for (i in 1:list.lg) {
    filter.DF <- filter.list[[i]]
    vector <- 0
    prob <- filter.DF %>% filter(Vector == vector) %>% pull(Interval.EFOR)
    key <- data.frame(Sample = 1:sample.size,Vector = vector,Available = sample(c(0, 1),sample.size,replace = T,prob = c(prob, (1 - prob))))
    list <- list("0" = key)
    for (vector in filter.DF %>% filter(Vector != 0 & !is.na(Interval.EFOR)) %>%pull(Vector)) {
      
      probs <- filter.DF %>% filter(Vector == vector) %>% pull(Interval.EFOR)
      list[[as.character(vector)]] <-  list[[as.character(vector-1)]] %>% rename(Available.old = Available) %>% mutate(Vector = vector,Available = Available.old * sample(c(0, 1),sample.size,replace = T,prob = c(probs, (1 - probs)))) %>% 
        select(-Available.old)
      
    }
    #view(list[[vector]])
    #print(paste(i," Vector",vector,"result probability:",sum(list[[23]]$Available) / sample.size))
    results.ls[[i]] <-
      do.call(rbind, list) %>% filter(Vector != 0) %>% left_join(filter.DF, by = "Vector") %>% select(all_of(key.cols), Sample, Vector, Available)
    
    print(i)
  }
  
  results.df <- do.call(rbind, results.ls)
  
  
  return(results.df)
}
# Vector Probability v2 ####
vector.probability.v2 <- function(x, sample.size = 10000,key.cols = c("UNIT.ID","Month","Hour"),Cap.col = "Interval.Capacity") {
  #     x = Solar.prob
  #     x = Sys.prob
  #     sample.size = 10000
  #     key.cols = c("UNIT.ID","Month","Hour")  
  #     key.cols = c("UNIT.ID")  
  #     Cap.col = "Interval.Capacity"
  #browser()
  ##### 
  #x must include the following columns (UNIT.ID,Vector,Interval.Capacity,Capacity,EFOR)  and may include (Month,Hour)
  # - UNIT.ID = character that defines or seperated more then one Unit or Intertie
  # - Vector = integer sequence starting at Vector 0 (Which represents the likelyhood of the outcome to be 0MW)
  # - Interval.Capacity = The resolution of the model from Vector[t] - Vector[t-1]
  # - Interval.EFOR = The estimate possibility the vector will not be available
  # - Month = integer value of a month
  # - Hour = integer value of hour
  ##### Notes about data structure
  sample.size <- min(100000,sample.size)
  filter.list <- x %>% group_by_at(key.cols) %>% group_split()
  units <- unique(select(x,all_of(key.cols)))
  
  list.lg = length(filter.list)
  i <- 14
  results.ls <- list()
  for (i in 1:list.lg) {
    filter.DF <- filter.list[[i]]
    vector <- 0
    prob <- filter.DF %>% filter(Vector == vector) %>% pull(Interval.EFOR)
    key <- data.frame(Sample = 1:sample.size,Vector = vector,Available = sample(c(0, 1),sample.size,replace = T,prob = c(prob, (1 - prob))))
    list <- list("0" = key)
    for (vector in filter.DF %>% filter(Vector != 0 & !is.na(Interval.EFOR)) %>%pull(Vector)) {
      
      probs <- filter.DF %>% filter(Vector == vector) %>% pull(Interval.EFOR)
      list[[as.character(vector)]] <-  list[[as.character(vector-1)]] %>% rename(Available.old = Available) %>% mutate(Vector = vector,Available = Available.old * sample(c(0, 1),sample.size,replace = T,prob = c(probs, (1 - probs)))) %>% 
        select(-Available.old)
      
    }
    #view(list[[vector]])
    #print(paste(i," Vector",vector,"result probability:",sum(list[[23]]$Available) / sample.size))
    results.ls[[i]] <-
      do.call(rbind, list) %>% filter(Vector != 0) %>% left_join(filter.DF, by = "Vector") %>% select(all_of(key.cols), Sample, Vector, Available)
    print(i)
  }
  Interval.Capacity <- x[,c("Vector",key.cols,Cap.col)]
  results.df <- do.call(rbind, results.ls) %>% left_join(Interval.Capacity) %>% mutate(Avail.Capacity = Available * Interval.Capacity)
  results.df.sum <- results.df %>% group_by_at(c(key.cols,"Sample")) %>% summarise(Max.Capacity = sum(Interval.Capacity),Avail.Capacity = sum(Avail.Capacity))
  
  return(results.df.sum)
}

#### Start a Time Series ####
DefineTS <- function(from = "2016-12-31 01:00",to = "2020-1-2 00:00" ,TimeColumn = "DateTime",date = F){
  require(dplyr)
  require(timeDate)
  #browser()
  from <- as.POSIXct(from,tz = "UTC")
  to <- as.POSIXct(to,tz = "UTC")
  
  x <- as.data.frame(timeSequence(from,to,by = "hour"))
  
  colnames(x) <- TimeColumn
  
  if(date){
    x <- x %>% mutate(Date = as.Date(!!sym(TimeColumn)),Hour = hour(!!sym(TimeColumn))) %>% 
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1))
  }
  
  return(x)
} 

`%!in%` <- function(x,y)!('%in%'(x,y))

#df <- ts
## DateTime to Date/Hour in existing data.frame #####
data.frame.DatetimeConversion <- function(df,format,TimeColumnName = "DateTime",Year = T,Month = T, DOW = F) {
  if(!is.data.frame(df)){stop("Input is not a data.frame")}
  #browser()
  require(dplyr)
  require(lubridate)
  
  paste(sum(as.integer(is.na(df[,TimeColumnName]))),"NA's due to DateTime in df.")
  df <- filter(df,!is.na(TimeColumnName))
  x <- df
  ## Convert Datetime column into POSIXct if needed
  if(is.POSIXct(df[,TimeColumnName])){
    df <- data.frame(DateTime = df[,TimeColumnName], Date = as.Date(df[,TimeColumnName]),Hour = hour(df[,TimeColumnName])) %>% distinct() %>%
      mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1)) %>% rename({{TimeColumnName}} := DateTime)
    names(df)[1] <- TimeColumnName
    
    if(Year){df$Year <- year(df$Date)}
    if(Month){df$Month <- month(df$Date)}
    if(DOW){df$DOW <- weekdays(df$Date)}
  } else(stop("Convert TimeColumnName to POSIXct before using this function"))
  
  df <- left_join(df,x)
  return(df)
}



#### NERC Holdiday Adjustment ####
## Inputs
# Date, DOW(Full text day of the week only)
## Output
# Date, DOW (Full text and the following days of the week coverted due to holidays)
NERCHoliday.Adj <- function(x){
  x <- x[,c("Date","DOW")]
  Holidays <- do.call(rbind,lapply(list(
    data.frame(holiday(unique(year(x$Date)),Holiday = "USNewYearsDay") - 3600*24,"DayBefore_USNewYearsDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USNewYearsDay"),"USNewYearsDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USMemorialDay"),"USMemorialDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USMemorialDay") + 3600*24,"DayAfter_USMemorialDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USIndependenceDay") - 3600*24,"DayBefore_USIndependenceDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USIndependenceDay"),"USIndependenceDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay") - 3600*24,"DayBefore_USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay"),"USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USLaborDay") + 3600*24,"DayAfter_USLaborDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay") - 3600*24,"DayBefore_USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay"),"USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USThanksgivingDay") + 3600*24,"DayAfter_USThanksgivingDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay") - 3600*24,"DayBefore_USChristmasDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay"),"USChristmasDay"),
    data.frame(holiday(unique(year(x$Date)),Holiday = "USChristmasDay") + 3600*24,"DayAfter_USChristmasDay")),
    setNames,c("Date","Holiday")))   
  x <- left_join(x,Holidays)
  
  x$DOW[x$Holiday %in% c("DayAfter_USMemorialDay", "DayBefore_USThanksgivingDay")]   <- "Monday" # Holidays Modeled as Monday
  x$DOW[x$Holiday %in% "DayAfter_USLaborDay"] <- "Thursday" # Holidays Modeled as Thursday
  x$DOW[x$Holiday %in% "DayBefore_USIndependenceDay"]   <-  "Friday" # Holidays Modeled as Friday
  x$DOW[x$Holiday %in% c("DayBefore_USNewYearsDay",
                         "USMemorialDay",
                         "DayBefore_USLaborDay",
                         "USLaborDay",
                         "USThanksgivingDay",
                         "DayAfter_USThanksgivingDay",
                         "DayBefore_USChristmasDay",
                         "DayAfter_USChristmasDay")] <-  "Saturday" #Holidays Modeled as Saturday
  x$DOW[x$Holiday %in% c("USNewYearsDay", "USIndependenceDay", "USChristmasDay") & x$DOW %in% "Friday"] <-  "Saturday"
  x$DOW[x$Holiday %in% c("USNewYearsDay",  "USIndependenceDay", "USChristmasDay") & x$DOW != "Friday"] <-    "Sunday"
  return(x)
}

#### Dummy.DOW ####
## Inputs
# Date, DOW (Full Text DOW only, usually the output of NERCHoliday.Adj)\
## Outputs
# Date, DOW Dummy variables

dummy.DOW <- function(t){
  
  holder <- data.frame(Date = t$Date)
  
  for(i in c("Sunday","Monday","Tuesday","Wednesday","Tuesday/Wednesday","Thursday","Friday","Saturday")){
    
    holder[,i] <- as.integer(t$DOW == i)
    if(sum(holder[,i]) == 0){
      holder[,i] <- NULL
    }
  }
  
  return(holder)
}


#### Seasonal Categories #### 
# Data Inputs
#   DateTime
## Data Outputs
# Datetime, Month dummies, Hour dummies, DOW Dummies

Categorical <- function(x,DayofWeek = "Summarized",NERCHolidays = T){
  #browser()
  #Require the package dummies
  require(dummies)
  require(timeDate)
  require(dplyr)
  #### Input (x) should be the same as DefineTS 
  # DateTime  Date  Hour  Year  Month  
  x <- data.frame(DateTime = x$DateTime, Date = as.Date(x$DateTime),Hour = hour(x$DateTime)) %>% unique() %>%
    mutate(Hour = replace(Hour,Hour == 0, 24)) %>% mutate(Date = replace(Date, Hour == 24, Date[which(Hour == 24)] - 1)) %>% 
    mutate(Month = month(Date),DOW = weekdays(Date)) #Takes the input Date time and recreates the needed Date, Hour, and Month Data for Categorical values
  
  #Month
  Month <- dummy.data.frame(x[,c("DateTime","Month")],names = "Month")[,1:12] # Leaves out Month 12
  
  #Hour
  Hour <- dummy.data.frame(x[,c("DateTime","Hour")],names = "Hour")[,1:24] # Leaves out Out 24 
  
  #DayofWeek Summarised is the hong tau method
  
  DOW <- unique(x[,c("Date","DOW")])
  
  #DOW.Order <- data.frame(DOW.number = c(1,2,3,4,5,6,7), DOW = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  
  
  
  if(DayofWeek == "Summarized" & isTRUE(NERCHolidays)){
    
    DOW$DOW[DOW$DOW %in% c("Tuesday","Wednesday")] <- "Tuesday/Wednesday" #Summarize Wednedays to Tuesdays
    DOW <- NERCHoliday.Adj(DOW)
    DOW <- DOW[,c("Date","DOW")]
    DOW <- dummy.DOW(DOW)
    DOW$Sunday <- NULL
  } else if(DayofWeek == "Summarized" & isFALSE(NERCHolidays)){
    
    DOW$DOW[DOW$DOW %in% c("Tuesday","Wednesday")] <- "Tuesday/Wednesday" #Summarize Wednedays to Tuesdays
    DOW <- DOW[,c("Date","DOW")]
    DOW <- dummy.DOW(DOW)
    DOW$Sunday <- NULL
  } else if(isTRUE(NERCHolidays)){
    
    DOW <- NERCHoliday.Adj(DOW)
    DOW <- DOW[,c("Date","DOW")]
    DOW <- dummy.DOW(DOW)
    DOW$Sunday <- NULL
  }else {
    DOW <- DOW[,c("Date","DOW")]
    DOW <- dummy.DOW(DOW)
    DOW$Sunday <- NULL
  }
  
  export <- left_join(x,Month,by = "DateTime")
  export <- left_join(export,Hour,by = "DateTime")
  export <- left_join(export,DOW, by = "Date")
  
  return(export)
}


Temperature.Manipulation <- function(x,DateTime.col = "DateTime",Temperature.col = "MID Station Temperature",lag.hours,poly.order = 3){
  #x <- weather.calculations
  cols <- c(DateTime.col,Temperature.col)
  x <- select(x,all_of(cols))
  
  
  if(hasArg(lag.hours)){
    x.poly <- x
    x.poly[[1]] <- x.poly[[1]] + hours(lag.hours)
    colnames(x.poly)[2] <- paste0(Temperature.col,".lag.",lag.hours,"hr")
    x <- left_join(x[1],x.poly)
  }
  Temperature.col <- colnames(x)[2]
  x.new <- select(x,all_of(DateTime.col))
  for(i in 1:poly.order){
    
    x.new[paste0(Temperature.col,"^",i)] <- x[,2]^i    
    
    
  }
  return(x.new)
}



#Relationships Function
RelationshipMultiplication <- function(DataFrameOne,DataFrameTwo,DF1_ColumnNames = "All",DF2_ColumnNames = "All"){
  
  if(dim(DataFrameOne)[1] != dim(DataFrameTwo)[1]){stop("DataFrameOne does not have the same number of rows as DataFrameTwo")} #Making sure that both data frames are the same length
  RowOrder <- match(DataFrameOne$OriginDateTime,DataFrameTwo$OriginDateTime)
  DataFrameTwo <- DataFrameTwo[RowOrder,]
  if(if(length(DF1_ColumnNames) == 1){ DF1_ColumnNames== "All"}else{FALSE}){
    DF1_ColumnNames <- colnames(DataFrameOne[,!(colnames(DataFrameOne) %in% c("OriginOriginDateTime","DateTime"))])
    #print("Using all columns in DF1")
  }else #If DF1_ColumnName = "All" Then use all column except "OriginDateTime" to Multiply
    if(all(DF1_ColumnNames %in% colnames(DataFrameOne))){ #Check to see that all the column names listed exist
      DF1_ColumnNames <- DF1_ColumnNames[!(DF1_ColumnNames %in% c("OriginOriginDateTime","DateTime"))]
      #print("The DF1 column names listed all exist")
    }else{
      stop( "(",paste(DF1_ColumnNames[!(DF1_ColumnNames %in% colnames(DataFrameOne))],sep = "",collapse = " & "),"):"," column names do not exist in DataFrameOne.")
    } #If statment to determine the First set of data to be used.
  
  if(if(length(DF2_ColumnNames) == 1){ DF2_ColumnNames== "All"}else{FALSE}){
    DF2_ColumnNames <-colnames(DataFrameTwo[,!(colnames(DataFrameTwo) %in% "OriginDateTime")])
    #print("Using all columns in DF1")
  }else #If DF1_ColumnName = "All" Then use all column except "OriginDateTime" to Multiply
    if(all(DF2_ColumnNames %in% colnames(DataFrameTwo))){ #Check to see that all the column names listed exist
      DF2_ColumnNames <- DF2_ColumnNames[!(DF2_ColumnNames %in% c("OriginDateTime","DateTime"))]
      #print("The DF2 column names listed all exist")
    }else{
      stop( "(",paste(DF2_ColumnNames[!(DF2_ColumnNames %in% colnames(DataFrameTwo))],sep = "",collapse = " & "),"):"," column names do not exist in DataFrameTwo.")
    } #If statment to determine the Second set of data to be used. 
  
  Relationship_List <- list()
  for(DF1_Column in DF1_ColumnNames){
    DF2loopCounter <- 0
    for(DF2_Column in DF2_ColumnNames){
      DF2loopCounter <- DF2loopCounter + 1
      NewColumnName <-  paste0(DF1_Column,DF2_Column)
      Relationship_List[[NewColumnName]] <- DataFrameOne[[DF1_Column]] * DataFrameTwo[[DF2_Column]]
      
    }# DF2 for loop
  } #DF1 for loop
  Relationships <- data.frame(OriginDateTime = DataFrameOne[,'OriginDateTime'],do.call(cbind,Relationship_List))  
  return(Relationships)
} # End of RelationshipMultiplicatin Function

#Leap Year Function
LeapYear <- function(year1, year2){
  
  return(c(year1:year2)[((c(year1:year2) %% 4 == 0) & (c(year1:year2) %% 100 != 0)) | (c(year1:year2) %% 400 == 0)])
  
}




#Copy dir to another folder (Used for scenario traking)
dir.copy <- function(from, to){
  
  ## check if from and to directories are valid
  if (!dir.exists(from)){
    cat('from: No such Directory\n')
    return (FALSE)
  }
  else if (!dir.exists(to)){
    cat('to: No such Directory\n')
    return (FALSE)
  }
  
  ## extract the directory name from 'from'
  split_ans <- unlist(strsplit(from,'/'))
  
  dir_name <- split_ans[length(split_ans)]
  
  new_to <- paste(to,dir_name,sep='/')
  
  ## create the directory in 'to'
  dir.create(new_to)
  
  ## copy all files in 'to'
  file_inside <- list.files(from,full.names = T)
  
  file.copy(from = file_inside,to=new_to)
  
  ## copy all subdirectories
  dir_inside <- list.dirs(path=from,recursive = F)
  
  if (length(dir_inside) > 0){
    for (dir_name in dir_inside)
      dir.copy(dir_name,new_to)
  }
  
  return (TRUE)
}


# File Opened Function
file.opened <- function(path) {
  suppressWarnings(
    "try-error" %in% class(
      try(file(path, 
               open = "w"), 
          silent = TRUE
      )
    )
  )
}



Scenarios.Building <- function(Fyear = 2021, 
                               HistDataTS = DefineTS(from = "2003-12-25 01:00",to =  "2021-01-09 0:00",TimeColumn = "Origin.DateTime"),
                               OriginYear.Range = 2004:2020,
                               ShiftParameters.Days = -7:7){
  require(tidyverse)
  require(lubridate)
  #browser()
  '%!in%' <- function(x,y)!('%in%'(x,y))
  Data <- HistDataTS
  Data  <- data.frame.DatetimeConversion(Data,TimeColumnName = "Origin.DateTime",Year = T,Month = F) %>% 
    mutate(Scenario = replace(Year,Year %!in% OriginYear.Range ,NA)) %>% mutate(Scenario = replace(Scenario,Year %!in% OriginYear.Range ,NA)) %>% select(-Year) 
  
  year(Data$Date) <- Fyear
  
  Data <- Data %>% mutate(DateTime = as.POSIXct(paste(Date,Hour),format = "%Y-%m-%d %H",tz = "UTC"),Shift.Days = 0) %>% select(Origin.DateTime,DateTime,Scenario,Shift.Days)
  
  
  
  shift = function(x, lag) {
    require(dplyr)
    switch(sign(lag)/2 + 1.5, lead(x, abs(lag)), lag(x, abs(lag)))
  }
  
  #Examples of how lag/lead works
  # (-) = lag  1 day lag example (-1): Forecast Date (12-31: December 31st) comes from (1-1: January 1st)
  # (+) = lead 1 day lead example (1): Forecast Date (12-31: December 31st) comes from (12-30: December 30th)
  
  # ShiftParameters.Days <- ShiftParameters.Days[ShiftParameters.Days!= 0]
  # Data.shift <- list(ShiftDays_0 = Data %>% mutate(Scenario = paste0("Scenario_OY",Scenario))) 
  Data.shift <- list()
  for(i in ShiftParameters.Days){
    
    Data.shift[[paste0("ShiftDays_",i)]] <- Data %>% mutate(Origin.DateTime = shift(Origin.DateTime,lag = i * 24), Scenario = paste0("Scenario_OY",Scenario),Shift.Days = i)
    
  }
  Data <- do.call(rbind,Data.shift)
  Data <- filter(Data,Scenario != "Scenario_OYNA") %>% filter(!is.na(DateTime))
  
  ## Cleaning Leap DateTimes from scenario Results
  leap_dates <- expand.grid(c("1988-2-29","1992-2-29","1996-2-29","2000-2-29","2004-2-29","2008-2-29","2012-2-29","2016-2-29","2020-2-29","2024-2-29",
                              "2028-2-29","2032-2-29","2036-2-29","2040-2-29","2044-2-29","2048-2-29","2052-2-29","2056-2-29","2060-2-29","2064-2-29"),1:24) %>% 
    mutate(Leap_DateTimes = paste(Var1,Var2)) %>% mutate(Leap_DateTimes = as.POSIXct(Leap_DateTimes,format = "%Y-%m-%d %H",tz = "UTC"))
  Data <- filter(Data,DateTime %!in% leap_dates$Leap_DateTimes)
  
  
  return(Data)
}