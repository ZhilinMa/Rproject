#my notes on setting up the system as required (unzipping file, etc) are 
#given in notes_log.txt.  Then in R I did:
#setwd("~/Desktop/biocomputing/git_exercises/Rproject/Rproject2022")


#Write a function that converts all files in a directory with space- or 
#tab-delimited data (.txt) into comma-separated value files
#I want this to be able to accept another country directory if one is 
#added, so I will loop through all directories in the pwd that start with 
#"country" and convert all of their txt's to csv's (rather than looking for
#countryX and countryY specifically)

#country X uses csv, Y uses txt
#days go from 120-175, but code should (and will) be compatible with any dates

#the first for loop and if statement ID countries
convert_txt_to_csv <- function(){
  for(filename in list.files(".")){
    if(grepl("country", filename)){
      #then within each country, this goes through all available files and 
      #checks if they are txt's
      for(scan in list.files(filename)){
         if(grepl(".txt", scan)){
           #if they are txt's, this lets them read in as data frames (with some 
           #string finagling to account for the location of the files in a 
           #subdirectory of the pwd)
           tableinput <- paste("./", filename, "/", scan, sep = "")
           tempdf <- read.table(tableinput, header = TRUE)
           #then this gives them a new name as .csv  (with same finagling for 
           #directories, and further finagling to switch the txt endings to csv)
           newname <- paste("./", filename, "/", sub("txt", "csv", scan), sep ="")
           write.csv(tempdf, file = newname, row.names = FALSE)
         }
      }
    }
  }
  return("done")
}



#Write a function to compile data from all .csv files in a directory into a 
#single .csv file. The compiled data should have the original twelve columns 
#from daily data sheets, but also country and dayofYear columns. The user 
#should be able to choose whether they want to remove rows with NA’s in any 
#columns, include NAs in the compiled data but be warned of their presence, 
#or include NAs in the compiled data without a warning

#since this function only compiles csv's, the conversion function must be run 
#first if all data is to be included

#To deal with NA's
#pass an int to the function:
#Pass 1 to remove rows with NA's
#Pass 2 to include NA's with a warning
#Pass 3 to include NA's without a warning
compile_data <- function(handling){
  allDat<- matrix(nrow = 0, ncol = 14)
  colnames(allDat) <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country", "dayofYear")

  for(filename in list.files(".")){
    if(grepl("country", filename)){
      #make dataframe with country name as last column and dayofyear as second to last
      countryName <- gsub("[a-z]+", "", filename)
      countryDat <- matrix(nrow = 0, ncol = 14)
      colnames(countryDat) <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10",  "country", "dayofYear")
      
      for(scan in list.files(filename)){
        if(grepl(".csv", scan)){
          scanPath <- paste("./", filename, "/", scan, sep = "")
          #add data to data frame
          data_holder <- as.data.frame(read.csv(scanPath, header = TRUE))
          
          if(handling == 1){
            if(length(which(is.na.data.frame(data_holder)))>0){
              print("At least one NA was present, but any NA's present have been removed")
            }
            data_holder <- na.omit(data_holder)
          }
          else if(handling == 2){
            if(length(which(is.na.data.frame(data_holder)))>0){
              print("Warning: At least one NA was present and has been incorporated into the all-data data set!")
            }
            else if(handling == 2){}
          }
          
          #get scan number/day of year and make last column that
          scanNum <- gsub("[A-Z,a-z,_,.]+", "", scan)
          data_holder[,13] <- countryName
          data_holder[,14] <- scanNum
          colnames(data_holder) <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country", "dayofYear")
          
          #add that to countryDat
          countryDat <- rbind(countryDat, data_holder)
        }
      }
      allDat <- as.data.frame(rbind(allDat, countryDat))
    }
  }
  colnames(allDat) <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country", "dayofYear")
  
  write.csv(allDat, "allDataFromR.csv", row.names = FALSE)
}



#I am writing a function to isolate all of the positive cases from allData.csv 
#and put them into their own data frame and csv, positives and positivesR.csv 
#in case they are wanted for future analysis (and to use to analyze the data 
#in my next function). This must be run after allDat is compiled
get_positives <- function(){
  #make a copy of allDat so it can be modified  to make the positives without 
  #affecting anything else
  datHolder <- allDat

  #add a column containing the row index
  for(rownum in 1:nrow(datHolder)){
    datHolder[rownum,15] <- rownum
  }
  
  #create the data frame of negatives and load in everything with 0 for marker01
  negatives <- datHolder[datHolder[,3]==0,]
  #narrow the negatives data frame down to the negative patients, those with all
  #0's for the markers
  for(i in 4:12){
    negatives <- negatives[negatives[,i]==0,]
  }
  #get what the original row number indexes were of all the negatives
  rows_to_remove <- as.numeric(negatives$V15)
  #make a data frame of positives by removing all the rows with those indexes
  positives <- datHolder[-c(rows_to_remove), ]
  #remove the extra column with the row numbers
  positives <- positives[,1:14]
  #write a csv of the data for future use
  write.csv(positives, "positivesR.csv", row.names = FALSE)
}



#Write a function to summarize the compiled data set in terms of number of 
#screens run, percent of patients screened that were infected, male vs. female 
#patients, and the age distribution of patients. Note that we provide a file 
#with the data compiled (allData.csv), so that this task is not dependent on 
#completion of the other tasks.  This must be run after all positives are found

#this will not work if there are NA's in the data, because I do not know
#how we are supposed to have it handle NA's (aka if you want it to error, 
#print a warning, or simply exclude NA's from calculations)
summarize_data <- function(){
  #make matrix to hold numscreens, %infected, male/female, age distribution
  summary_mat <- matrix(nrow = 6, ncol = 1)
  
  #num of screens (terminal checking says should be 39888)
  summary_mat[1,1] <- nrow(allDat)
  
  #%infected (terminal checking says should be 56.55%)
  summary_mat[2,1] <- paste(100*nrow(positives)/nrow(allDat), sep = "")
  
  #male vs female (all) (terminal checking says female should be 19896)
  summary_mat[3,1] <-  paste(nrow(allDat[allDat[,1]=="male",]), "Males :", nrow(allDat[allDat[,1]=="female",]), "Females", sep = " ")
  
  #male vs female (infected) (terminal checking says female should be 11265)
  summary_mat[4, 1] <-  paste(nrow(positives[positives[,1]=="male",]), "Males :", nrow(positives[positives[,1]=="female",]), "Females", sep = " ")
    
  #age distribution (all)
  summary_mat[5, 1] <- paste("Mean:", mean(allDat$age), "Median:", median(allDat$age), "Lowest:", min(allDat$age), "Highest:", max(allDat$age), sep = " ")
  
  #age distribution (infected)
  summary_mat[6, 1] <- paste("Mean:", mean(positives$age), "Median:", median(positives$age), "Lowest:", min(positives$age), "Highest:", max(positives$age), sep = " ")
  
  summarydf <- as.data.frame(x = summary_mat, row.names = c("Screens Run: ", "Percentage of Positive Screens: ", "Male:Female Ratio (All Patients): ", "Male:Female (Positive Patients): ", "Age Distribution (All Patients): ", "Age Distribution (Postive Patients): "))
  #set column name
  colnames(summarydf) <- "Results Summary:"
  
  #print summary
  print(summarydf)
}



# This function will graph the number of positive screens (cases) for each 
# country at each date 
graph.cases.by.country <- function(){
  #get list of all countries that need to be graphed to cycle through
  countries <- unique(positives$country)
  #get list of all dates to cycle through and count the cases
  dates <- unique(positives$dayofYear)
  #create a matrix to store the dates, case counts, and countries,
  #making it long enough to hold the data from all dates and all countries
  casecounts <- matrix(nrow = length(countries)*length(dates), ncol = 3)
  colnames(casecounts) <- c("DayofYear", "Cases", "Country")
  #i will serve as the row index so data can be added to a new row for each
  #unique date/country combo
  i <- 0
  
  #this cycles through all of the countries and all of the dates and counts 
  #the cases on each date for each country
  for(countryName in countries){
    countryData <- positives[positives[,13]==countryName,]
    caseSum <- 0
    for(date in dates){
      #adding 1 to i increases the row index so data is stored in a new row
      #of the matrix
      i <- i + 1
      
      #this stores the date, number of cases on that date, and the country
      #those cases were in in the matrix
      caseSum <- nrow(countryData[countryData[,14]==date,])
      casecounts[i, 1] <- date
      casecounts[i, 2] <- caseSum
      casecounts[i, 3] <- countryName
    }
  }

  #this makes casecounts into a data frame so it can be graphed with ggplot2
  casecounts <- as.data.frame(casecounts)
  
  #this changes the dates and case counts to numerics so they can be 
  #graphed properly
  casecounts$DayofYear <- as.numeric(casecounts$DayofYear)
  casecounts$Cases <- as.numeric(casecounts$Cases)
  
  #this graphs the case counts in each country vs the day of year, with a 
  #different line and color for each country
  ggplot(data = casecounts,  aes(x=DayofYear, y = Cases, color = Country)) +
    xlab("Day of Year") + ylab("Number of Cases") + ggtitle("Cases Per Day") +
    geom_line()+ theme_classic()
}


#This function will graph the occurrence of each disease marker in each 
#country as a histogram to demonstrate whether a vaccine developed in one
#place would be likely to work in the other
graph.average.marker.incidences <- function(){
  #find a list of all countries in the data
  countries <- unique(positives$country)
  #Make a matrix to hold the disease markers, country, and the average of the
  #markers (aka how many times the marker occurred in that country's cases,
  #divided by the total cases in that country).  This average will be used to 
  #show how prevalent the different disease markers are in each country, which
  #will show whether a vaccine developed in one country will be effective
  #in the other country
  diseaseMarkers <- matrix(nrow = 10*length(countries), ncol = 3)
  #name the columns
  colnames(diseaseMarkers) <- c("Marker", "Country", "Average_of_Markers")
  
  #fill in the first column with the marker names
  markerNames <- c("Marker 01", "Marker 02", "Marker 03", "Marker 04", "Marker 05", "Marker 06", "Marker 07", "Marker 08", "Marker 09", "Marker 10")
  diseaseMarkers[,1] <- markerNames
  
  #this variable counts the row index to help fill in the matrix
  rowindex<- 0
  #this loops through the countries, making a data frame of each country's cases
  for(countryName in countries){
    countryData <- positives[positives[,13]==countryName,]
    #this loops through the markers within the sub-data frame from each country
    for(i in 3:12){
      #set the row index so the data is added in the right place
      rowindex <- rowindex + 1
      #input the country name
      diseaseMarkers[rowindex, 2] <- countryName
      #input the mean of the disease markers for that country
      diseaseMarkers[rowindex, 3] <- mean(countryData[,i])
    }
  }
  #make the matrix into a data frame so it can be graphed with ggplot
  diseaseMarkers <- as.data.frame(diseaseMarkers)
  #convert the marker averages into a numeric so they can be graphed properly
  diseaseMarkers$Average_of_Markers <- as.numeric(diseaseMarkers$Average_of_Markers)

  #plots the averages for all the markers in each country
  ggplot(data = diseaseMarkers, aes(x = Marker, y = Average_of_Markers, fill = Country)) +
    geom_bar(stat = "identity", position = position_dodge()) + ylab("Average Incidence of Marker") +
    xlab("") + theme_classic() + ggtitle("Average Marker Incidence")
}


