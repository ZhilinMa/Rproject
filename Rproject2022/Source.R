#contacted by US CDC to evaluate EID outbreak

#Goal 1: import data
  #Country X can be imported readily using .csv and a for loop
  #Country Y needs to have space-delimited .txt file converted to .csv file
      #For orthogonality purposes, might want to generate a function to do this where:
          #delimiter is one of the inputs ("\t", " ")

#Goal 2: summarize individual country data
  #To determine if patient is positive: sum markers 1-10
    #if sum >=1; patient is infected
  #Determine earliest date in which individuals are infected in each country

#csv Data (countryX) looks like: gender, age, Markers 1-10, comma-separated
#txt Data (countryY) looks like: gender, age, Markers 1-10, space-delimited >> need to convert to csv


y120 = read.table("/Users/chris_turlo/Desktop/RProject/RProject2022/CountryY/screen_120.txt", header=TRUE, sep=" ", stringsAsFactors=FALSE)
for(file in '/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryY'){
  
}

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

for(file in )

screen120x<-read.csv("screen_120.csv", header=TRUE, sep=',')

#what we need to do here, open .csv > store data; try for csv

#need to add which date and which country columns to each file
daterange = c(120:175)
for (i in daterange){
  xscreen[i]<-read.csv("screen_[i].csv", header=TRUE, sep=",")}

#then we repeat this with countryY for .txt files

screen120y<-as.table("screen_120.txt", header=TRUE, sep='\t')

for (i in daterange){
  yscreen_[i]<-as.data.frame("screen_[i].txt", header=TRUE, sep="\t")
}
  
#conversion of screen120.txt>csv
  #ways to replace .txt with .csv > find function to do it in R
  #google replace text with other text in R >> sub()/gsub()**

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

