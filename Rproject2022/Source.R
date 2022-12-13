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

### BELOW IS WORKSPACE ###

#Country X can be imported readily using .csv and a for loop
  #csv Data (countryX) looks like: gender, age, Markers 1-10, comma-separated
#Country Y needs to have space-delimited .txt file converted to .csv file
  #txt Data (countryY) looks like: gender, age, Markers 1-10, space-delimited >> need to convert to csv
#For orthogonality purposes, might want to generate a function to do this where:
    #delimiter is one of the inputs ("\t", " ")


#Compile Function
Compile <- function(dir, Country){
  setwd(dir)
  filelist <-list.files(path = dir, pattern = ".csv")
  manual_input_1 <- readline(prompt = "Would you like to remove NAs (Y/N)")
  if (manual_input_1 == "Y")
    for (i in 1:length(filelist)){
      if (i==1){
        alldata<-read.csv(filelist[i], header=TRUE)}
      else if (i>1) {
        alldata<-rbind(alldata,read.table(filelist[i], header=TRUE, sep=','))
      }
      }
    }
}



#Goal 2: summarize individual country data
  #To determine if patient is positive: sum markers 1-10
    #if sum >=1; patient is infected
  #Determine earliest date in which individuals are infected in each country


CSVConverter<-function(dir){
  filelist <- list.files(pattern = '.txt')
  CSVfiles <- lapply(filelist, function(x){
    textfile <-read.table(x, header=TRUE, sep=" ", stringsAsFactors=FALSE)
    write.csv(textfile, file =gsub(pattern="\\.txt$", replacement = ".csv", x=x))
    newfiles <-list.files(pattern = ".csv")
  })
  return(newfiles)
  }

y120 = read.table("/Users/chris_turlo/Desktop/RProject/RProject2022/CountryY/screen_120.txt", header=TRUE, sep=" ", stringsAsFactors=FALSE)


#NOTE FROM STUART: zip file has a git repo within it; go in terminal and delete the directory with Rproject2022, delete

#Which country (X/Y) did the disease outbreak likely begin
  #Determine which country has a case earlier than the other (can determine from compiled file)


#If country Y develops a vaccine for the disease, is it likely to work for Country X
  #compare microsatellite values across individual countries, look at Y, compare to X


#microsatellite analysis (10 markers)
  #if 1+ markers present, patient was infected


#airborne bacteria


#each country is screening a large number of patients with symptoms
  #screening_NNN.txt > day, gender, microsatellite markers

#provide answers and supporting info for the two questions > provide code for future analyses

#converts data files into csv
#compile all data > all original 12 columns, plus country column and "dayofYear" column
#write function to summarize compiled data; #screens run, %patients infected, male v. female stats, age distribution)



#what we need to do here, open .csv > store data; try for csv

#need to add which date and which country columns to each file





#sum(x$age%in%(10:20)) >> i assume this goes somewhere down the line

#to work with all data:
provided_alldata<-read.csv("/Users/chris_turlo/Desktop/RProject/RProject2022/allData.csv", header=TRUE, sep=',')
  

##From rubric: (6 points)
#Converts .txt to .csv, and changes file extension
#Compiles the files
#Generate a summary graph

##Question 1 (answer question with reasonable rationale, graphical support) (4 points)
##Question 2 (answers question with reasonable rationale, graphical support) (4 points)
#uses all support functions created (2 points)
#well commented and efficient (4 points)

#files requested:
  #supportingFunctions.R > contain the assorted scripts to process data
    #convert all files to .csv
    #compile data, including country and dayofYear columns
    #user can choose to remove NAs in any columns, include but be warned of presence, or include and do not warn
    #write summary file: allData.csv
        #Number of screens run
        #Percent of patients screened that were infected
        #Male v. Female
        #Age Distribution

  #analysis.R > use source() to utilize the functions within supportingFunctions.R
    #compile data into a single .csv
    #process the data
    #provide graphical insight
    #have commented answers to these questions


#WARNING - function that outputs warning messages