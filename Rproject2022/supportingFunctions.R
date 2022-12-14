setwd("~/Desktop/finalproj/Rproject/Rproject2022")


######## FUNCTION CONVERT  ##########
# function to convert .txt files to csv 
convert_csv <- function(dir) {
  # get all the files from the directory
  files <- list.files(dir, pattern="*.txt")
  
  print("Files being converted to CSV files...")
  
  for(i in 1:length(files)){
    # get the path of the file
    curr <- paste(dir, files[i], sep="/")
    # read the file
    csv_curr <- read.table(file = curr, sep = " ", header = TRUE)
    # create csv file 
    write.csv(csv_curr, file = (paste(dir, gsub("txt", "csv", files[i]), sep = "/")), row.names = FALSE)
  }
  
  print("Files converted.")
}


######## FUNCTION COMPILE  ##########
# function to compile all .csv files in a dir to a single file
compile <- function(dir, toWarn){
  # get all the files in dir
  files <- list.files(dir, pattern = "*.csv")
  # make the data frame for the new csv file
  df <- data.frame(matrix(data = NA, ncol=14, nrow=0))
  for(i in 1:length(files)){
    # get path of current file
    curr <- paste(dir, files[i], sep="/")
    # read the curr file
    curr_csv <- read.csv(curr)
    # set country to whichever country directory passed in
    country <- dir
    # find the day of the file 
    day <- strtoi(substr(files[i],8,10))
    # make the two extra cols
    curr_csv <- cbind(curr_csv, country, day)
    # add the row to the data frame
    df <- rbind(df, curr_csv)
  }
  
  # user options
  # 1. remove rows with NAs in any column
  # 2. include NAs but be warned of their presence
  # 3. include NAs but don't be warned
  
  
  # handle NAs based on user input
  if(toWarn == 1){
    if(sum(is.na(df))) {
      df <- na.omit(df)
    }
  }
  else{
    if(sum(is.na(df))) {
      if(toWarn == 2) {
        print("Warning: NAs are present")   # only warn if 2 is selected
      }
    }
  }
  return(df)
}


###### FUNCTION SUMMARIZE  ##########
summarize <- function(file) {
  # number of screens run
  print(paste("The numbers of screens run:", nrow(file)))
  df <- data.frame(matrix(data = NA, ncol=14, nrow=0))
  # percent of patients screened that were infected
  num_infected = 0
  num_males = 0
  num_females = 0
  min_age = 150
  max_age = 0  
  # go through each row of file
  for(i in 1:nrow(file)){
    # check if any of the markers did not equal 0
    if(sum(file[i, 3:12]) > 0) {
      num_infected = num_infected + 1
      df <- rbind(df, file[i,])
    }
    if(file[i, 1] == "male"){
      num_males = num_males + 1
    }
    else {
      num_females = num_females + 1
    }
    if(file[i, 2] > max_age){ 
      max_age = file[i, 2]
    }
    if(file[i, 2] < min_age){ 
      min_age = file[i, 2]
    }
  }
  percent_infected = num_infected/(nrow(file))
  print(paste("The percent of patients screened that were infected:", percent_infected))
  
  # male vs female patients
  print(paste("The percent of patients screened that were male:", num_males/(num_males + num_females)))
  print(paste("The percent of patients screened that were female:", num_females/(num_males + num_females)))
 
  # age distribution of patients
  print(paste("The age distribution of the patients was", min_age, "to", max_age)) # not sure why 423 is one of the ages?
  return(df)
}


get_markers <- function(file ) {
  x_markers <- c(0,0,0,0,0,0,0,0,0,0)  # initalize all marker sums to 0
  y_markers <- c(0,0,0,0,0,0,0,0,0,0)
  
  marker_range <- 3:12
  
  for (i in 1:nrow(file)) {
    for(m in marker_range) {
      if (file[i,m] == 1) {
        if (file[i,13] == "X") {
          x_markers[m-2] = x_markers[m-2] + 1
        }
        else {
          y_markers[m-2] = y_markers[m-2] + 1
        }
      }
    }
  }
  
  #print(x_markers)
  #print(y_markers)
  
  marker_summary <- data.frame(x_markers, y_markers)
  
  return(marker_summary)

}









