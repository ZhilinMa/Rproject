#contacted by US CDC to evaluate EID outbreak
#Goal 1: import data
#Set working directory, generate lists of file-paths to operate on
setwd("/Users/chris_turlo/Desktop/Rproject/Rproject2022")
filenamesX <- list.files(path = "/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryX", pattern = "screen_[0-9]{3}.csv")
filenamesY <- list.files(path = "/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryY", pattern = "screen_[0-9]{3}.txt")

#FUNCTIONAL DESIGN DO NOT EDIT
#CSV Conversion Function
#Write a function that converts all files in a directory with space/tab delimited data (.txt) into a .csv file
#Usage: dir = directory path, separator = how original file is delimited, specify " " for space, "\t" for tab
CSVConverter<-function(dir, separator){
  setwd(dir)
  filenames <- list.files(path = dir, pattern = ".txt")
  for(i in 1:length(filenames)){
    input<-filenames[i]
    output<-paste0(gsub("\\.txt$", "", input), ".csv")
    data = read.table(input, header=TRUE, sep=separator)
    write.table(data, file=output, sep=',', col.names=TRUE, row.names=FALSE)
  }
}

CSVConverter("/Users/chris_turlo/Desktop/Rproject/Rproject2022/CountryY"," ")


#During compilation, for whatever reason, my CSV files for CountryX were not compiling. 
#I generated this alternative to CSVConverter called TXT Converter
#Then I converted countryX to .txt files and then BACK to .csv files and the problems were resolved

TXTConverter<-function(dir, separator){
  setwd(dir)
  filenames <- list.files(path = dir, pattern = ".csv")
  for(i in 1:length(filenames)){
    input<-filenames[i]
    output<-paste0(gsub("\\.csv$", "", input), ".txt")
    data = read.table(input, header=TRUE, sep=separator)
    write.table(data, file=output, sep=' ', col.names=TRUE, row.names=FALSE)
  }
}

TXTConverter("/Users/chris_turlo/Desktop/Rproject/Rproject2022/CountryX", ',')
CSVConverter("/Users/chris_turlo/Desktop/Rproject/Rproject2022/CountryX", ' ')


##ADD dayofYear column
#Here are some attempts to add the dayofYear column to the 

DayofYear<-function(dir){
  setwd(dir)
  filenames<-list.files(path=dir, pattern=".csv")
  for(i in 1:length(filenames)){
    filenumber<-gsub("[A-Z a-z \\.\\ \\_\\]", "", filenames[i])
    filenames[i]$dayofyear<-as.data.frame(sapply(filenumber))
    print(filenumber)
  }
}

DayofYear<-function(dir){
  setwd(dir)
  filenames<-list.files(path=dir, pattern=".csv")
  for(i in 1:length(filenames)){
    filenames[i]$dayofyear<-as.data.frame(gsub("[A-Z a-z \\.\\ \\_\\]","", filenames[i]))
  }
}
##MORE ELEGANT COMPILE FUNCTION
#This function takes a directory, and a Country (in quotes) and outputs a compiled file of all .csv files within the directory
#This function adds a column to the file called "country" specifying where it comes from.

Compile <- function(dir, Country){
  setwd(dir)
  filelist<-list.files(path=dir, pattern=".csv")
  #Prompts to remove incomplete data from the set (for if working with datasets with incomplete testing information)
  manual_input_1 <- readline(prompt = "Would you like to remove incomplete data? (Y/N)")
  #if yes, then compile data, and then remove incomplete data (NA)
  if (manual_input_1 == "Y"){
    for (i in 1:length(filelist)){
      if(i==1){
        alldata<-read.csv(filelist[i], header=TRUE)
      }else if (i>1){
        alldata<-rbind(alldata, read.table(filelist[i], header=TRUE, sep=','))
      }
    }
    are_there_na<-any(is.na(alldata))
    if (are_there_na==TRUE){
      alldata<-alldata[complete.cases(alldata),]
      readline(prompt="Incomplete data has been removed successfully.")
    }else{
      readline(prompt="No incomplete data detected")
    }
    alldata$country<-c(Country)
    write.csv(alldata, file='allDATA.csv')
  }else{
    #this else statement is if you choose not to remove NA data
    #the following readline sets up the ask of whether or not you want to be warned that data is incomplete
    manual_input_2<-readline(prompt = "Do you wish to be warned of missing data? (Y/N)")
    if (manual_input_2 == "Y"){
      for (i in 1:length(filelist)){
        if(i==1){
          alldata<-read.csv(filelist[i], header=TRUE)
        }else if (i>1){
          alldata<-rbind(alldata,read.table(filelist[i], header=TRUE, sep=','))
        }
      }
      are_there_na<-any(is.na(alldata))
      if(are_there_na==TRUE){
        readline(prompt="WARNING: Incomplete data has been detected, but not removed.")
      }else{
        readline(prompt="No incomplete data has been detected.")
      }
      alldata$country<-c(Country)
      write.csv(alldata,file="allDATA.csv")
    }else if (manual_input_2=="N"){
      #If you say no to both questions, the following produces a compiled file with NA values present
      for (i in 1:length(filelist)){
        if(i==1){
          alldata<-read.csv(filelist[i], header=TRUE)
        }else if (i>1){
          alldata<-rbind(alldata,read.table(filelist[i], header=TRUE, sep=','))
        }
      }
      alldata$country<-c(Country)
      write.csv(alldata,file="allDATA.csv")
    }
  }
}

##Designed by JV, Imported and edited by CVT
summarizedData<-function(file){
  alldata<-read.csv(file, header=TRUE, sep=",")
  # Adding "infected" column that contains counts of the total markers for each patient.
  alldata$infected<-alldata$marker01+alldata$marker02+alldata$marker03+alldata$marker04+
    alldata$marker05+alldata$marker06+alldata$marker07+alldata$marker08+alldata$marker09+
    alldata$marker10
  
  # Adding "positive" column that assigned number1 to 
  # each patient that had a non-zero amount of markers observed.
  #graph case load per each country

  alldata$positive[alldata$infected>0]<-1
  library(ggplot2)
  DailyCaseLoad<-ggplot(data=alldata, aes(
    x=dayofYear, y=positive, color=as.factor(country)))+
    geom_col()+
    theme_bw()+ggtitle("Country Caseload Over Time") + 
    xlab("Day of Year") + ylab("Number of Positive Patients")
    
  ### SCREENS RUN ###
  
  screensrun <- nrow(alldata)
  
  
  ### INFECTED SCREENS: Looking at individual biomarkers ###
  
  # Marker Screens for Country X
  marker01X<-sum(alldata[ alldata$country == "X" , 3 ] )
  marker02X<-sum(alldata[ alldata$country == "X" , 4 ] )
  marker03X <-sum(alldata[ alldata$country == "X" , 5 ] )
  marker04X <-sum(alldata[ alldata$country == "X" , 6 ] )
  marker05X <-sum(alldata[ alldata$country == "X" , 7 ] )
  marker06X <-sum(alldata[ alldata$country == "X" , 8 ] )
  marker07X <-sum(alldata[ alldata$country == "X" , 9 ] )
  marker08X <-sum(alldata[ alldata$country == "X" , 10 ] )
  marker09X <-sum(alldata[ alldata$country == "X" , 11 ] )
  marker10X <-sum(alldata[ alldata$country == "X" , 12 ] )
  
  # Marker Screens for Country Y
  marker01Y<-sum(alldata[ alldata$country == "Y" , 3 ] )
  marker02Y<-sum(alldata[ alldata$country == "Y" , 4 ] )
  marker03Y <-sum(alldata[ alldata$country == "Y" , 5 ] )
  marker04Y <-sum(alldata[ alldata$country == "Y" , 6 ] )
  marker05Y <-sum(alldata[ alldata$country == "Y" , 7 ] )
  marker06Y <-sum(alldata[ alldata$country == "Y" , 8 ] )
  marker07Y <-sum(alldata[ alldata$country == "Y" , 9 ] )
  marker08Y <-sum(alldata[ alldata$country == "Y" , 10 ] )
  marker09Y <-sum(alldata[ alldata$country == "Y" , 11 ] )
  marker10Y <-sum(alldata[ alldata$country == "Y" , 12 ] )
  
  # Data table for Number of Positive Screens by Marker in Countries X and Y
  markersXY<-data.frame(marker=1:10,X=c(marker01X,marker02X,marker03X,marker04X,marker05X,
                                        marker06X,marker07X,marker08X,marker09X,marker10X),Y=c(marker01Y,marker02Y,marker03Y,
                                                                                               marker04Y,marker05Y,marker06Y,marker07Y,marker08Y,marker09Y,marker10Y))
  
  dfmarkersXY <- data.frame(marker=c(markersXY$marker),
                            frequencyXY=c(markersXY$X,markersXY$Y),
                            country=c("X","X","X","X","X","X","X","X","X","X",
                                      "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"))
  
  # Bar chart for Number of Positive Screens by Marker in Countries X and Y
  (markersbycountryXY <-ggplot(dfmarkersXY, aes(x=marker,y=frequencyXY, fill=country))+
      geom_bar(stat="identity", position = "dodge2")+theme_bw()+ggtitle("Distribution of Markers in Country X and Y") +
      xlab("Marker Number") + ylab("Positive Screen") + scale_x_continuous(name="Marker Number", breaks = 1:10))
  
  # Percentage of Screens that were Infected
  onlypositives <- nrow(alldata$positives==1)
  percentinfected <- onlypositives/screensrun
  
  
  ### AGE ###
  
  # Density Plot of Age Distribution (0-100 ONLY)
  agedistribution <- ggplot(data=alldata,aes(x=age))+
    geom_density()+
    scale_x_continuous(limits = c(0,100))+
    theme_bw()+ggtitle("Distribution of Age in Countries X and Y") +
    xlab("Age") + ylab("Density")
  
  
  ### GENDER ###
  # Number of females and males
  nfemales <- nrow(alldata[alldata$gender=="female",])
  nmales <- nrow(alldata[alldata$gender=="male",])
  
  
  print(markersbycountryXY)
  print(agedistribution)
  print(DailyCaseLoad)
  return(c("Total Screens Run:", screensrun, "Percentage Infected:", percentinfected, "Number of Female Patients:",
           nfemales, "Number of Male Patients:", nmales))
}