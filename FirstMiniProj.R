## Problem 1

unzip("rprog_data_specdata.zip", exdir = ".")   ## Unzipping the file
setwd("~/specdata")                             ## Setting the directory to the folder named 'specdata'
getwd()                                         ## Checking the current directory

pollutantMean <- function (directory, pollutant, id = 1:332){   ## The function pollutantMean takes 3 arguments
  specdata <-c("C:/Users/Kimberly/Documents/specdata/")         ## specdata holds the location of the .csv files (wd) 
  csvlist <-list.files(path=specdata, pattern = ".csv")         ## csvlist holds the list of files (csv) using the list.files() function
  x<-c()                                                        ## x being initialized with a null value using the c() function
  for (i in id){                                                ## the no. of iteration of this for loop will depend on id
    data<-read.csv(csvlist[i])                                  ## reads the ith element of the csvlist, stored to data,                                   
    x<- c(x,data[[pollutant]])                                  ## extracting/subset the data using [[]] as the desired pollutant being the argument, concatenating it with x, and storing the value to x
  }                                                             ## the value of x updates until the end of for loop 
  mean(x, na.rm=T)                                              ## mean() function gets the mean/average of x; NA values are being removed
}                                                               ## end of the pollutantMean function

pollutantMean(specdata, "sulfate", 1:10)    ## Sample codes in order to check/use the populationMean function
pollutantMean(specdata, "nitrate", 70:72)   ## Or get a mean value from a desired pollutant across the entered id
pollutantMean(specdata, "nitrate", 23)      ##  Wherein these files are located in the current directory being hold by specdata 

## Problem 2, Modifying codes in Problem 1

complete <- function (directory, id = 1:332){             ## The function complete takes 2 arguments
  specdata <-c("C:/Users/Kimberly/Documents/specdata/")   ## specdata holds the location of the .csv files (wd) 
  csvlist <-list.files(path=specdata, pattern = ".csv")   ## csvlist holds the list of files (csv) using the list.files() function
  y <- c()                                                ## y being initialized with a null value using the c() function
  for (i in id){                                          ## the no. of iteration of this for loop will depend on id
    data<-read.csv(csvlist[i])                            ## reads the ith element of the csvlist, stored to data
    total <- sum(complete.cases(data))                    ## total holds the sum of the complete cases (knowing these through complete.cases function) in each file of data                    
    y = c(y, total)                                       ## concatenating the total with y and storing it to y
  }                                                       ## the value of y updates until the end of for loop 
  data.frame(id, y)                                       ## creates a data frame with id as the 1st column and complete cases as the 2nd column
}                                                         ## end of the complete function

complete(specdata, 1)                 ## Sample codes in order to check/use the complete function
complete(specdata, c(2,4,8,10,12))    ## Or get a data frame containing the complete cases in each id
complete(specdata, 30:25)             ## And these files are located in the current directory being hold by specdata
complete(specdata, 3)

## Problem 3, Modifying codes in Problems 1 and 2

corr <- function(directory, threshold = 0){                 ## The function corr takes 2 arguments
  specdata <-c("C:/Users/Kimberly/Documents/specdata/")     ## specdata holds the location of the .csv files (wd) 
  csvlist <-list.files(path=specdata, pattern = ".csv")     ## csvlist holds the list of files (csv) using the list.files() function
  data_frame <- complete(directory = specdata, id = 1:332)  ## using the complete function
  View(data_frame)                                        ## Viewing data_frame with 332 obs. and 2 vars: id and y
  ids <- data_frame[data_frame["y"] > threshold, ] $id    ## Extracting only where the complete cases is greater that the threshold based on id                 
  corr <- c()                                             ## corr being initialized with a null value using the c() function                                                                                 
  for (i in ids){                                         ## the no. of iteration of this for loop will depend on id
    data<-read.csv(csvlist[i])                            ## reads the ith element of the csvlist, stored to data
    data_frame2= data[complete.cases(data), ]             ## Extracting the complete cases of data
    corr = c(corr, cor(data_frame2$sulfate, data_frame2$nitrate))  ## Correlating the sulfate and nitrate exclusive only for those with complete cases, concatenating this to corr
  }                                                       ## end of for loop
  return(corr)                                            ## returns the value of correlation
}                                                         ## end of corr function

cr <- corr(specdata, 150)                   ## Sample codes in order to check/use the corr function
head (cr); summary(cr)

cr <- corr(specdata, 400)                   ## using the head() and summary() functions.
head (cr); summary(cr)

cr <- corr(specdata, 5000)                  ## This gives value of correlation between sulfate and nitrate
head (cr); summary(cr); length(cr)

cr <- corr(specdata)                        ## based  on the id entered.                 
head (cr); summary(cr); length(cr)

## Problem 4

unzip("rprog_data_ProgHospData.zip", exdir = ".")   ## Unzipping the file
setwd("~/")                                         ## Setting the directory where the unzipped files were located
getwd()                                             ## Checking the current directory
outcome <- read.csv('outcome-of-care-measures.csv', colClasses= "character")   ## Reads the csv file
head(outcome)                                   ## head() function to show the first few rows of outcome
ncol(outcome)                                   ## ncol() function to show the number of columns of outcome
nrow(outcome)                                   ## nrow() function to show the number of rows of outcome

outcome[,11] <- as.numeric(outcome[,11])        ## coerce the objects, in column 11 of rows, to be in numeric
hist(outcome[,11])                              ## Histogram of outcome[,11] (column 11 of outcome)

outcome[,11] <- as.numeric(outcome[,11])        ## coerce the objects, in column 11 of outcome, to be in numeric
hist(outcome[,11] , main = paste(colnames(outcome[11])), xlab="Deaths")   ## Adding an argument main, to have a title
                                                                          ## which is the name of column 11 of outcome 
## (Hospital 30-Day Death (Morality) Rates from Heart Attack), and x-axis labels which is Death 