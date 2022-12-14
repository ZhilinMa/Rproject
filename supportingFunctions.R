#Country X and Y have different traditions for the delimiter in their data files. Write a function that
#converts all files in a directory with space- or tab-delimited data (.txt) into comma-separated value
#files.

#usage:convert_files_in_directory("path of directory")

function(dir) {
  # Get a list of all .txt files in the specified directory
  files <- list.files(dir, pattern="*.txt", full.names=TRUE)
  
  # Loop through each file in the list
  for (f in files) {
    # Read the file into a data frame, using the specified delimiter
    df <- read.table(f, header=TRUE, sep=" ",stringsAsFactors = FALSE)
    
    # Write the data frame to a new file with the same name, but with a .csv extension
    write.csv(df, file=gsub(".txt$", ".csv", f),row.names = FALSE)
  }
}

#Write a function to compile data from all .csv files in a directory into a single .csv file. The compiled
#data should have the original twelve columns from daily data sheets, but also country and dayofYear
#columns. The user should be able to choose whether they want to remove rows with NA’s in any
#columns, include NAs in the compiled data but be warned of their presence, or include NAs in the
#compiled data without a warning
function(dir, naOption) {
  # Get list of all .csv files in the specified directory
  files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
  
  # Initialize empty data frame to store compiled data
  compiledData <- data.frame()
  
  # Loop through all files in the directory and compile data
  for (file in files) {
    
    # Read data from current file
    data <- read.csv(file)
    
    # Add country and dayOfYear columns to data
    data$country <- ifelse(grepl("countryX", dir), "X", "Y")
    data$dayOfYear <- substr(file, nchar(file) - 6, nchar(file) - 4)
    # Convert dayOfYear column to integer
    data$dayOfYear <- as.integer(data$dayOfYear)
    
    
    # Handle NAs according to specified option
    if (naOption == "remove") {
      # Remove rows with NA values
      data <- data[complete.cases(data), ]
    } else if (naOption == "warn") {
      # Check for NAs and print warning if present
      if (any(is.na(data))) {
        print("WARNING: NAs present in data")
      }
    }
    
    # Append current data to compiled data frame
    compiledData <- rbind(compiledData, data)
  }
  
  
  # Return compiled data frame
  return(compiledData)
}

#Write a function to summarize the compiled data set in terms of number of screens run, percent of
#patients screened that were infected, male vs. female patients, and the age distribution of patients.
#Note that we provide a file with the data compiled (allData.csv), so that this task is not dependent
#on completion of the other tasks.

summary_stats <- function(file){
  data<-read.csv(file)
  #sum all the rows to find out if the patient is infected, 
  #if all_marker is greater than 0, then this patient is infected
  data$all_marker = rowSums(data[,3:12])
  data$infection_status = NA
  
  #non infected screens are 0 and infected patients are 1, for easy calculation
  for (i in 1:nrow(data)) {
    if (data$all_marker[i] == 0)
      data$infection_status[i] = 0
    else 
      data$infection_status[i] = 1
  }
  #get the total screen number and total infected number with the marker of infection_status set above
  total_screen = as.numeric(nrow(data))
  total_infected = as.numeric(length(data$infection_status[data$infection_status== 1]))
  #count infected percentage
  infected_percentage = (total_infected/total_screen)
  #count patient count seperately for male and female
  male_patient_count= sum(data$infection_status[data$gender=="male"])
  female_patient_count= sum(data$infection_status[data$gender=="female"])
  #filter out rows with age that are too big
  data$age[data$age>110] <- NA
  
  #make a data frame with patient only
  patients = subset(data, data$infection_status == 1)
  
  #using ggplot2 to draw the Age distribution
  library(ggplot2)
  age_hist <- ggplot(patients,aes(x = age)) +
    geom_histogram()+
    theme_bw() +
    xlab("Age") +
    ylab("Count")+ggtitle("Age distribution ")
  
  #print out the information
  print(c("Total screen count is",total_screen))
  print(c("Percent of patients screened that were infected is",infected_percentage))
  print(c("Count of male patient is",male_patient_count))
  print(c("Count of female patient is",female_patient_count))
  
  return(age_hist)
}
