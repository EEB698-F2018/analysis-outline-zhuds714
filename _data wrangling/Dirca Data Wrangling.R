##  ----------------------------------------------------------  ##
# DIRCA ANATOMY DATA WRANGLING ####
##  ----------------------------------------------------------  ##

##Install Libraries and Packages EX:install.packages("tidyverse")
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(readxl) #hadley wickham's package for reading in excel files. not in Tidyverse.
library(lubridate) #garett grolemund & hadley wickham's package for dates

setwd("/Users/Dirca/Desktop/GitHub-RStudio/Dirca Anatomy Mock Data Analysis/_data/raw")
#Load .csv dataset.  Default: Header=TRUE
#dirca <- read.csv("_data/raw/dirca.csv", na.strings = c("", "NA", "na", " "))
dirca <- read.csv("dirca.csv", na.strings = c("", "NA", "na", " "))

#Explore dataset
summary(dirca)
names(dirca)

###STEP 3###
#Standardize/clean inconsistencies in column names, class etc.
#Rename column headings
#Use the rename function in the dplyr package
#New name is on left, old name is on right.
dirca <- rename(dirca, Species = species)
dirca <- rename(dirca, Population = population)
dirca <- rename(dirca, Specimen.Number = specimen_num)
dirca <- rename(dirca, Cell.Type = cell_type)
dirca <- rename(dirca, Observation = observation)
dirca <- rename(dirca, Primarywall.Thickness.um = primary_wall_thickness)
dirca <- rename(dirca, Secondarywall.Thickness.um = secondary_wall_thickness)
dirca <- rename(dirca, Length.um = length)
dirca <- rename(dirca, Lumen.Diameter.um = lumen_diameter)
dirca <- rename(dirca, Cell.Total.Diameter.um = cell_total_diameter)
dirca <- rename(dirca, Lignified.Y.N = lignified.)
dirca <- rename(dirca, Number.of.Cell.Width = num_of_cell_width)
dirca <- rename(dirca, Seriate.Number = seriate_num)

#Explore dataset
str(dirca) #check variable class assignment

#Check Variable Class Assignment
#Correct specimen_num & observation Class Assignments
dirca$Specimen.Number <- as.factor(dirca$Specimen.Number)
dirca$Observation <- as.factor(dirca$Observation)
str(dirca) #check assignments have changed

#Create tidy database for analysis
setwd("/Users/Dirca/Desktop/GitHub-RStudio/Dirca Anatomy Mock Data Analysis/_data/tidy")
#write.csv(dirca, "_data/tidy/dirca_tidy.csv", row.names=F)
write.csv(dirca, "dirca_tidy.csv", row.names=F)
