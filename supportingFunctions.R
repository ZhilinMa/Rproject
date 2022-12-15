##Biocomputing 2022 - R Project
#Coded by Lauren Sallay, 12/14/2022
#Supporting Functions

#set working directory
setwd("~/Desktop/biocomputing-2022/Rproject2")

#Country X and Y have different traditions for the delimiter in their data files
#Write a function that converts all files in a directory from.txt to .csv
convert_txt <- function(dir){
  files <- list.files(path = dir, pattern = "*.txt")
  ##create a for loop to apply to every file
  for (f in files){
    newfile<-read.txt(file, header=TRUE)
    write.table(newfile, file=.csv, append=FALSE)
  }
}

#Write a function to compile data from all .csv files in a directory into a single .csv file
#establish blank matrix for the data to be compiled
compiled.data <- function(dir){
  column_names <- c("gender", "age", "marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10","country","dayofYear")
  data <- matrix(column_names, ncol=15)
  #create for loop to apply to the data of each file
  for (country in dir) {
    files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
    for (file in files){
      data = read.csv(file)
      day = as.numeric(substr(file, 8, 10))
      data$country = country
      data$dayofYear = day
      data$infected = FALSE
      all.data = rbind(all.data,data)
    }
  }
  #present user with options to remove NAs, or keep them with or without a warning
  print("options: remove rows with NA's in any column (1), include NAs but receive a warning (2), or include NAs without a warning (3)")
  Option=readline("Pick your option:") 
  if(Option==1){
    fulldata = na.omit(all.data)
  }
  else if (Option==2){
    print("NAs will be included in the data")
    
    #option 3 will not run any extra functions
  }
  #write the new data table
  new.data <- all.data[,1:ncol(all.data)-1]
  write.table(new.data,"compiled.data.csv", sep='')
}
#Write a function to summarize the compiled data set:
summary_data <- function(dir){
  read.csv("allData", header = TRUE, sep = '')
  
  #number of screens run
  print(paste("Screens run:", nrow(df[[1]])))
  #percent of patients screened that were infected 
  num.infected = 0
  total.patients = 0
  for (i in 1:nrow(allData)) {
    row <- allData[i,]
    for(val in row [3:12]){
      if (val==1){
        num.infected = num.infected + 1
        allData[i,15] = TRUE
      }
    }
    total.patients = total.patients + 1
  }
  print(paste("Percent infected:", num.infected/total.patients*100))
  
  #male vs female patients
  #collect the numbers of those infected and the totals
  males.infected = sum(allData[which(fulldata$gender=="male"), 15])
  total.males = sum(allData$gender=="male")
  females.infected = sum(allData[which(allData$gender=="female"),15])
  females.total = sum(allData$gender=="female")
  #create dataframe
  genders.plot.data <- data.frame(infection=c("infected", "uninfected", "infected","uninfected"), gender=c("male","male","female","female"), number=c(males.infected,total.males-males.infected,females.infected,females.total-females.infected))
  #plot data - bar graph
  library(ggplot2)
  ggplot(data=genders.plot.data, aes(fill=gender, y=number,x=infection)) +
    geom_bar() + theme_classic()
  
  #age distribution of patients
  ggplot(allData, aes(x=age, color=infected))+
    geom_density() +
    theme_classic()
}