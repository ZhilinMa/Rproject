#contacted by US CDC to evaluate EID outbreak

#csv Data (countryX) looks like: gender, age, Markers 1-10, comma-separated
#txt Data (countryY) looks like: gender, age, Markers 1-10, tab-delimited


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
  