# In order to accept input for the second function "compile_data", this script must be run line by line instead of just highlighting
# the functions and then pressing "Run".

setwd("~/Biocomputing/Rproject/Rproject2022")
library(ggplot2)
library(cowplot)
source('supportingFunctions.R')

unlink_file("allDataTest2.csv")

file_conversion("countryX")
file_conversion("countryY")

compile_data("countryX")
compile_data("countryY")

summarize_data("allDataTest2.csv")

# Question 1: Where did the disease outbreak likely begin?

# Based on the second plot we created, the disease outbreak likely began in Country X because people in Country X 
# started getting infected between days 120 and 130. On the other hand, people in Country Y didn't start getting infected until 
# around day 141. The graph titled "Where did the Disease Start?" provides a comparison between the days people were infected for 
# each country.

# Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?

immune_check("allDataTest2.csv")

# Based on the third plot we created ("Markers for Infected People in each Country"), a vaccine would likely not work if it 
# was shared between the two countries. This is because the members of each country possess different markers of the disease which 
# means there are differences in the protein in the disease that warrant different responses by a patient's immune system. 
# Our graph shows that infected Country X citizens possess a high rate of markers 1-5 (low rate for 6-10) while infected Country Y 
# citizens possess high rates of markers 6-10 (low rate for 1-5).
