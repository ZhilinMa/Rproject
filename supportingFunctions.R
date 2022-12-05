

# converts files from .txt to .csv
file_conversion <- function(dir) {
  files <- list.files(path = dir, pattern = "*.txt")
  for (f in files) {
    file <- sprintf("%s/%s",dir,f)
    data <- read.table(file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
    if (ncol(data) == 1) {
      data <- read.table(file, header = TRUE, stringsAsFactors = FALSE, sep = " ")
    }
    name <- substr(file,1,19)
    file <- sprintf("%s.csv", name)
    write.csv(data, file = file, row.names = FALSE)
  }
}

# just to make reruns more efficient
unlink_file <-function(file) {
  if (file.exists(file) == TRUE) {
    unlink(file)
  }
}

# turns all csv files into one large csv file
compile_data <- function(dir) {
  print("What do you want to do about the variables containg NA in the data?")
  print("Type 1 to remove the rows with NA")
  print("Type 2 to keep the rows with NA and have a warning displayed.")
  print("Type 3 to keep the rows with NA and with no warning.")
  type <- readline(prompt = "Type: ")
  # asks user if what they want to do with rows containing NA
  path <- sprintf("%s%s/","~/Biocomputing/Rproject/Rproject2022/",dir)
  files <- list.files(path = path,pattern = "*.csv")
  filepath <- "allDataTest2.csv"
  country <- substr(path,45,45)
  if (file.exists("allDataTest2.csv") == TRUE) {
    count <- 0
  }
  else {
    count <- 1
  }
  checker <- 0
  if (type == 3 || type == 2) {
    for (f in files) {
      file <- sprintf("%s%s",path,f)
      dayofYear <- strtoi(substr(file,54,56))
      data2 <- read.csv(file = file, header = TRUE, sep = ",")
      if (checker == 0) {
        data2$country <- country
        data2$dayofYear <- dayofYear
        data <- data2
      }
      else if (checker == 1) {
        data2$country <- country
        data2$dayofYear <- dayofYear
        data <- rbind(data,data2)
      }
      checker <- 1
    }
    checkNA(data,type)
    if (count == 0) {
      write.table(data,file = filepath, append = TRUE, sep = ",",col.names = FALSE,row.names = FALSE)
    }
    else {
      write.table(data,file = filepath, append = TRUE, sep = ",",col.names = TRUE,row.names = FALSE)
    }
  }
  else if (type == 1) {
    check <- 0
    for (f in files) {
      file <- sprintf("%s%s",path,f)
      dayofYear <- strtoi(substr(file,54,56))
      data <- read.csv(file = file, header = TRUE, sep = ",")
      data$country <- country
      data$dayofYear <- dayofYear
      if (file.exists("allDataTest2.csv") == TRUE) {
        check <- 1
      }
      if (count == 1 & check == 0) {
        write.table(data,file = filepath, append = TRUE, sep = ",",col.names = TRUE,row.names = FALSE)
        count <- 0
      }
      else {
        write.table(data,file = filepath, append = TRUE, sep = ",",col.names = FALSE,row.names = FALSE)
      }
    }
  }
  else {
    print("Invalid Type")
  }
}

# provides a summary in terms of a file containing different statistics and a graph for the large csv file
summarize_data <- function(file) {
  sumtable <- read.csv(file,header = TRUE, stringsAsFactors = FALSE, sep = ",")
  numofscreens <- nrow(sumtable)
  numinfected <- 0
  numofmales <- 0
  numoffemales <- 0
  femalesinfected <- 0
  malesinfected <- 0
  infected <- c()
  for (row in 1:numofscreens) {
    col <- 3
    while (sumtable[row,col] != 1 & col < 13) {
      col = col + 1
    }
    if (sumtable[row,col] == 1) {
      numinfected = numinfected + 1;
      if (sumtable[row,1] == "male") {
        malesinfected = malesinfected + 1
      }
      else if (sumtable[row,1] == "female") {
        femalesinfected = femalesinfected + 1
      }
      infected <- append(infected,1)
    }
    else {
      infected <- append(infected,0)
    }
    if (sumtable[row,1] == "male") {
      numofmales = numofmales + 1
    }
    else if (sumtable[row,1] == "female") {
      numoffemales = numoffemales + 1
    }
  }
  percentinfected <- numinfected/numofscreens
  summary <- sprintf("Number of Screens: %d\nPercent of Patients Screened that were Infected: %f%%\nNumber of Males Screened: %d\nNumber of Females Screened: %d\nNumber of Males Infected: %d\nNumber of Females Infected: %d\n",numofscreens,percentinfected * 100,numofmales,numoffemales,malesinfected,femalesinfected)
  writeLines(summary,"summarizeddata.txt")
  Countries <- c(sumtable[,13])
  Days <- c(sumtable[,14])
  startingdisease <- data.frame(Countries,infected,Days)
  sumtable$csum <- ave(infected,sumtable$country, FUN=cumsum)
  graph <- ggplot(data = sumtable, aes(x = age)) + geom_histogram(binwidth = 1) + theme_classic() + ggtitle("Distribution of Age") +
    xlab("Age") + ylab("Count")
  show(graph)
  graph2 <- ggplot(data = sumtable, aes(x = dayofYear, y = csum, color = as.factor(country))) + geom_line() + xlab("Day of Year") +
    ylab("Number of Infected") + ggtitle("Where did the Disease Start?") + theme_classic() + labs(color = "Countries")
  show(graph2)
}

# creates a graph which shows what markers were more relevant for people of each country
immune_check <- function(file) {
  sumtable2 <- read.csv(file,header = TRUE, stringsAsFactors = FALSE, sep = ",")
  vecX <- c(0,0,0,0,0,0,0,0,0,0)
  vecY <- c(0,0,0,0,0,0,0,0,0,0)
  markers <- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
  for (b in 3:12) {
    for (a in 2:nrow(sumtable2)) {
      if (sumtable2[a,b] == 1) {
        if (sumtable2[a,13] == "X") {
          vecX[b-2] = vecX[b-2] + 1
        }
        else if (sumtable2[a,13] == "Y") {
          vecY[b-2] = vecY[b-2] + 1
        }
      }
    }
  }
  vecXY <- append(vecX,vecY)
  countries <- c("X","X","X","X","X","X","X","X","X","X","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y")
  markers <- c("Marker01","Marker02","Marker03","Marker04","Marker05","Marker06","Marker07","Marker08","Marker09","Marker10","Marker01","Marker02","Marker03","Marker04","Marker05","Marker06","Marker07","Marker08","Marker09","Marker10")
  vaccine <- data.frame(vecXY,countries,markers)
  ggplot(data = vaccine, aes(x = markers, y = vecXY, fill = countries)) + geom_bar(stat = "identity") + 
    ggtitle("Markers for Infected People in each Country") + xlab("Disease Markers") + ylab("Total Number of People") + labs(color = "Countries")
}


# checks to see if there are any NA in the large csv file
checkNA <- function(data,type) {
  delete <- c()
  for (j in 1:nrow(data)){
    for (k in 1:ncol(data)) {
      if (is.na(data[j,k]) == TRUE) {
        delete <- append(delete,j)
        break;
      }
    }
  }
  if (length(delete) > 0) {
    data <- data[-c(delete),]
  }
  if (type == 2) {
    output <- sprintf("%d rows were removed because they had at least 1 column containing NA.",length(delete))
    print(output)
  }
  return(data)
}