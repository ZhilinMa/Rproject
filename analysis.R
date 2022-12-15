##Biocomputing 2022 - R Project
#Coded by Lauren Sallay, 12/14/2022
#Analysis

#Source for the functions that will be used:
source("supportingFunctions.R")

#convert .txt to .csv:
convert_txt("countryY")
convert_txt("countryX")
#Country X should already have all data in .csv format

#compile into one large .csv file
compiled.data("countryX","countryY")

#run the data summary
summary_data("allData.csv")

#Question 1: In which country (X or Y) did the disease outbreak likely begin?
#According to the data, it looks like there are infections prevalent in earlier stages in country X, 
#as there are infections present on the first day of screening there but not in country Y.
#Therefore, I believe the outbreak began in country X.


#Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#I do not believe this vaccine would work - 
#according to the data, Country X and Country Y have their peaks at different times
#the markers were present in patients earlier on for Country X, and later on for Country Y
#therefore, these could likely be different variants or the infection likely changed in one way
#so the vaccine would not be as effective on the original strain than the likely weaker one
