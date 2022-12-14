#process the data
#provide graphical insight
#have commented answers to these questions

#load in supplementary functions
source("/Users/chris_turlo/Desktop/Rproject/Rproject2022/supportingFunctions.R")

#converts .txt files in CountryY to .csv files
CSVConverter("/Users/chris_turlo/Desktop/Rproject/Rproject2022/CountryY"," ")
#compiles .csv files for CountryX, CountryY
Compile("/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryX", "X")
Compile("/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryY", "Y")

#load in the data frames for alldata for country X and Y
alldataX<-read.csv("/Users/chris_turlo/Desktop/Rproject/Rproject2022/countryX/allDATA.csv")
alldataY<-read.csv("/Users/chris_turlo/Desktop/RProject/Rproject2022/countryY/allDATA.csv")

#bind those data frames and write it to a new file called ALLDATA.csv
alldata<-rbind(alldataX,alldataY)
write.csv(alldata, "/Users/chris_turlo/Desktop/Rproject/Rproject2022/ALLDATA.csv")

###Due to issues with indexing and additional clunkiness with our compiled file, data analysis was performed on the given allData.csv file

#Which country (X/Y) did the disease outbreak likely begin?
#ANSWER
#.....


#If country Y develops a vaccine for the disease, is it likely to work for Country X:

#ANSWER
#No - The microsatellite data for the samples from Country X and Country Y show distinct patterns:
#When a person from Country X is infected: Biomarkers 1-5 are present
#When a person from Country Y is infected: Biomarkers 6-10 are present
#This discrepancy is clearly shown in the graph comparing the sum of traces of each biomarker in each country
#The vaccine may be helpful locally to Country Y or to other countries with similar biomarker traces
#Though not distinctly helpful to Country X; it may be useful moving forward if local outbreaks demonstrate similar biomarker identities
#It is wise to know about it for control measures, in the event of a sudden outbreak caused by a traveler from CountryY
