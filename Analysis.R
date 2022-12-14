#Script
setwd("~/Desktop/Rproject-1/Rproject2022")
source("supportingFunctions.R")

convert_tocsv("./countryY")

data_from_dir_X <- compileData("./countryX", "remove")
data_from_dir_Y <- compileData("./countryY", "remove")
compiled_data <- rbind(data_from_dir_X, data_from_dir_Y)
write.csv(compiled_data, "Rprojectdata.csv",row.names = FALSE)

summary_stats("Rprojectdata.csv")

#1. In which country (X or Y) did the disease outbreak likely begin?

#load the data
alldata <- read.csv("Rprojectdata.csv")
#add a new column for infection status, and sum the amount of markers to get this
alldata$all_marker = rowSums(alldata[,3:12])
alldata$infection_status = NA

#non infected screens are 0 and infected patients are 1 
for (i in 1:nrow(alldata)) {
  if (alldata$all_marker[i] == 0)
    alldata$infection_status[i] = 0
  else 
    alldata$infection_status[i] = 1
}

#take the subset of patients
patients = subset(alldata, alldata$infection_status == 1)
#When the patient is too old, we consider this data might be incorrect
patients$age[patients$age>110] <-NA
patients <- na.omit(patients)

#Create another dataframe to draw the outbreak plot
outbreak <- data.frame(matrix(ncol = 3, nrow = length(unique(alldata$dayOfYear))))
x <- c("screen_day","country_X_patient_count","country_Y_patient_count")
colnames(outbreak) <- x

#get all the screen_day
outbreak$screen_day = unique(alldata$dayOfYear)

#loop through to get country_X_patient_count and country_Y_patient_count
for (i in outbreak$screen_day){
  #filter out the data in patients(dataframe) that country = X, dayofYear = i
  x_pat= patients[which(patients$dayOfYear==i&patients$country=="X"),]
  outbreak[i-119,2]=nrow(x_pat)
  #filter out the data in patients(dataframe) that country = X, dayofYear = i
  y_pat= patients[which(patients$dayOfYear==i&patients$country=="Y"),]
  outbreak[i-119,3]=nrow(y_pat)}

#get the cumsum of X_patient_count and Y_patient_count to draw the plot
outbreak$X_cumsum <- cumsum(outbreak$country_X_patient_count)
outbreak$Y_cumsum <- cumsum(outbreak$country_Y_patient_count)

library(ggplot2)

#drawing the plot
ggplot(outbreak, aes(x=screen_day , y = "")) + 
  geom_line(aes(y = X_cumsum, col = "Country X")) + 
  geom_line(aes(y = Y_cumsum, col = "Country Y"))+
  theme_bw() +
  xlab("Day of Year") +
  ylab("Cumulative patient number")+
  theme(legend.title=element_blank())

#Answer: Based on the data and visual evidence, it appears that the 
#disease outbreak originated in Country X. The graph shows that on 
#Day 140, Country X had approximately 4000 infected patients, while 
#Country Y still had no reported cases. Since Country Y only began 
#reporting cases after Country X, it is likely that the outbreak 
#began in Country X and then spread to Country Y.


#2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#2. This requires an analysis of the markers 

#First, load in the data to be used 

#Subset the dataframe so only markers and country are left 
X_markers <- subset(alldata,country=="X", select = marker01:marker10)

Y_markers <- subset(alldata, country == "Y", select = marker01:marker10)

#Run colsum for each of the markers in each dataset 

#For Country X
Xmarker_sums <- colSums(X_markers, na.rm=FALSE)

#For Country Y 
Ymarker_sums <- colSums(Y_markers, na.rm = FALSE)

#create a new dataframe to draw the plot
marker<- data.frame(matrix(ncol = 3, nrow = 10))
y <- c("Marker_name","X_sum","Y_sum")
colnames(marker) <- y

#get marker_name, Xmarker_sums and Ymarker_sums
marker$Marker_name <-1:10
marker$X_sum<-Xmarker_sums
marker$Y_sum<-Ymarker_sums

#drawing the plot
ggplot(marker, aes(x=Marker_name , y = "")) + 
  geom_point(aes(y = X_sum, col = "Country X")) + 
  geom_point(aes(y = Y_sum, col = "Country Y"))+
  theme_bw() +
  xlab("Marker Number") +
  ylab("Marker Counts")+
  theme(legend.title=element_blank())

#Answer:The presence of different markers can be used to identify 
#the disease in question. Because these markers can trigger different
#immune responses in patients, it is likely that different variants of
#the disease will require different vaccines to be effective. The data
#and associated plot show that patients in Country X are more likely
#to have variants of the disease with markers 1-5, while patients in
#Country Y are more likely to have variants 6-10. This difference
#suggests that a vaccine developed for patients in Country X may not
#be effective for patients in Country Y.
