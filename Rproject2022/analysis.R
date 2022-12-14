#first:
#setwd("~/Desktop/biocomputing/git_exercises/Rproject/Rproject2022")
#install.packages("ggplot2")
#library(ggplot2)
#apologies if this is way too many comments but hey the instructions said to 
#comment I'm gonna comment haha

#load the supporting functions so they can be used
source('supportingFunctions.R')
#you can also run supportingFunctions.R in R to load the functions if you want


#first, convert any txt screens to csv's with:
#convert_txt_to_csv() 

#then read in the data from both countries to one data frame and csv file
#the instructions for regulating handling of NA's are given in the 
#supportingFunctions script, in this case I have used the option to remove rows
#with NA's so that if they were present they would be removed and not cause 
#later errors in data analysis, but it doesn't really matter ofc this time since 
#there are no NA's present in the data set
compile_data(1)


#I will then read in the compiled data from the allDataFromR.csv as a data frame
#here, rather than having each subsequent function have to repeat that process
allDat <- as.data.frame(read.csv("allDataFromR.csv",header = TRUE))


#Having gathered all the data in one file, I will isolate the positive patients
#from that file for further analysis
get_positives()


#I will load that positivesR.csv in here as a data frame rather than having
#any subsequent functions repeat it
positives <- as.data.frame(read.csv("positivesR.csv", header = TRUE))


#I will graph the cases/day for each country to determine where the illness
#originated using another function I made in supportingFunctions.R
graph.cases.by.country()

#This graph shows the cases found in each country on each day.  This reveals
#that the outbreak almost certainly began in country X, as country X begins
#with more cases and increases, while country Y begins with 0 cases and takes
#several days to begin showing cases

#I will then graph the occurrence of each disease marker in each country as a
#histogram to demonstrate whether a vaccine developed in one place would 
#be likely to work in the other
graph.average.marker.incidences()

# This graph shows the average of each marker's incidence in each country (by
# showing the average value for each marker across all positive patients in each
#country).  It shows that the markers, on average, vary greatly from country to 
#country.  As a result, it is very unlikely that a vaccine developed in 
#Country Y would work in Country X, since the microsatellite profiles differ 
#greatly between the countries and have very little overlap

#I will summarize the data and print its analysis
summarize_data()
