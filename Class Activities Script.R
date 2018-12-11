#Class Activites Script with Comments, Using Dirca Data

#load libraries
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
library(readxl) #hadley wickham's package for reading in excel files. not in Tidyverse.
library(lubridate) #garett grolemund & hadley wickham's package for dates
library(ggplot2)
library(vegan) #ordination/multivariate package

#OR check to see if you have libraries, and if not, install them.
source("_library/Functions/ipak_fx.R") #load ipak function
ipak(c("tidyverse", "lubridate", "readxl"))

#Install devtools & easyGgplot2
#http://www.sthda.com/english/wiki/ggplot2-histogram-easy-histogram-graph-with-ggplot2-r-package
install.packages #devtools
library(devtools)
install_github #easyGgplot2, kassambara
library(easyGgplot2)

###STEP 1### 
#Load .csv dataset.  Default: Header=TRUE
dirca <- read.csv("_data/tidy/dirca_tidy.csv", na.strings = c("", "NA", "na", " "))

###STEP 2###
#Explore dataset
summary(dirca)
names(dirca)

###STEP 3###
#Standardize/clean inconsistencies in column names, class etc.
#Rename column headings
#Use the rename function in the dplyr package
#new name is on left, old name is on right.
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

###STEP 4###
#Exploration and Visualization
#library(ggplot2) allows creation of graphics

str(dirca)

#Correct Variable Class Assignments
dirca$Specimen.Number <- as.factor(dirca$Specimen.Number)


#Tables: tables & ftables
#table shows the number of measurements taken for each Specimen.Number within a species, including all Cell.Types
with(dirca, table(Species, Specimen.Number))

#table shows the number of measurements taken for each Cell.Type type per species
with(dirca, table(Species, Cell.Type))

#Graphics: boxplot, histogram, 
#boxplot shows range, mean, and first & third quantile for the seriate number of rays within all species
#$ selects a single element of a list (ex. dirca$Seriate.Number = Seriate.Number data points from the dirca dataset)
boxplot(dirca$Cell.Total.Diameter.um) #variability within a single variable across all species
boxplot(Lumen.Diameter.um~Species, data = dirca) #variability for a pair of variables
boxplot(Cell.Total.Diameter.um~Species, data = dirca) #variability for a pair of variables
boxplot(dirca$Cell.Total.Diameter.um~Species) #FAIL dirca$ does not work for 2 variables

#histogram (frequency of each value of continuous variable)
hist(dirca$Cell.Total.Diameter.um) #single variable across all species
hist(dirca$Secondarywall.Thickness.um) #single variable across all species
hist(dirca$Cell.Total.Diameter.um) #single variable across all species

#frequency of Cell.Total.Diameter.um (including all Cell.Type) per species
hist(Cell.Total.Diameter.um~Species, data = dirca) #FAIL: 'x' must be numeric
hist(dirca$Cell.Total.Diameter.um, breaks = "Species") #FAIL: "arg" should be one of “sturges”, “fd”, “freedman-diaconis”, “scott”
ggplot2.histogram(data=dirca, xName='Cell.Total.Diameter.um',
                  groupName = 'Species', legendPosition = "top",
                  binwidth = 20, alpha = .5 ) #PASS: but colors blend, hard to differentiate species
#alpha is used to set color transparency but I do not see any changes when used
#binwidth changes the width of the histogram bars

#ggplot(dataset, aes(X variable, Y variable, color=factor level))
#boxplot shows range, mean, and first & third quantile for Cell.Total.Diameter.um separated by population and species
ggplot(dirca, aes(Population, Cell.Total.Diameter.um, color=Species))+
  geom_boxplot()

#aes & geom Different styles
#histogram (frequency of each value of continuous variable)
ggplot(dirca, aes(Cell.Total.Diameter.um))+
  geom_histogram()

ggsave("Cell.Total.Diameter.um.png")

##data=cells[rows,colums], aes...
#rows would be celltype==""
#or subset or filter function

#boxplot of Cell.Total.Diameter.um of bark.fiber divided by population and species for all Cell.Type
#ggplot(dataset, aes(X variable, Y variable, color=factor level))
ggplot(dirca, aes(Population, Cell.Type, Cell.Total.Diameter.um, color=Species))+
  geom_boxplot() #FAIL, Cell.Total.Diameter.um is not longer included in graph

#use subset function to select bark.fiber
#https://www.statmethods.net/management/subset.html
#use filter

bark.fiber.subset <- subset(dirca, Cell.Type == bark_fiber, ##FAIL: Cell.Type bark.fiber was not selected
                            select=c(Species, Population, Cell.Total.Diameter.um)) #c(order must match that of the dataset)
bark.fiber.subset <- filter(dirca, Cell.Type == "bark_fiber") #PASS: use filter to find rows/cases where conditions are true
ggplot(bark.fiber.subset, aes(Population, Cell.Total.Diameter.um, color=Species))+
  geom_boxplot()

head(bark.fiber.subset)
summary(bark.fiber.subset)

bark.fiber.subset %>% #bark_fiber average Cell.Total.Diameter.um by Population
  group_by(Population) %>%
  summarize (avg = mean(Cell.Total.Diameter.um))

bark.fiber.subset %>% #bark_fiber average Cell.Total.Diameter.um by Species
  group_by(Species) %>%
  summarize (avg = mean(Cell.Total.Diameter.um))


###STEP 5###

#Identify Variables (response=dependent, predictor=independent)
#response=cell.total.diameter.um (gaussian distribution, prolly) (binomial for yes/no, 1/0 measurements)
#predictor=species(factor), population(factor), specimen#(factor)..... for factor predictors, prolly use rep()
#response~predictor 1 = predictor 2, family=____

#(Species)We collected data for four species: mexicana, occidentalis, decipiens, palustris. 
#(Population)We chose 2 populations for mexicana, 1 population for occidentalis, 2 populations for decipiens, and 3 populations for palustris
#(Specimen.Number)At each population we collected samples from three independent plants
#(Observation)Four replicates were taken for each independent plant
################################REPLICATES?
#mexicana 1x2x3x4=24
#occidentalis 1x1x3x4=12
#decipiens 1x2x3x4=24
#palustris 1x3x3x4=36
#Total of 24+12+24+36=96 data points for Cell.Total.Diameter.um

#table shows the number of measurments taken per observation, per specimen number, per population, per species.
#are able to identify mis-entered data because no two species share the same population
with(dirca, ftable(Species, Population, Specimen.Number, Cell.Type, Observation))

#Simulate response (Cell.Total.Diameter.um of bark.fiber)
mexicana.celltotal <- rnorm(n=24, mean=(115), sd = (20)) #simulate mexicana with Cell.Total.Diameter.um
occidentalis.celltotal <- rnorm(n=12, mean=(122), sd = (20)) #simulate occidentalis with Cell.Total.Diameter.um
decipiens.celltotal <- rnorm(n=24, mean=(114), sd = (20)) #simulate decipiens with Cell.Total.Diameter.um
palustris.celltotal <- rnorm(n=36, mean=(106), sd = (20)) #simulate palustris with Cell.Total.Diameter.um
response.celltotal <- c(mexicana.celltotal, occidentalis.celltotal, decipiens.celltotal, palustris.celltotal)

#Simulate predictor (species, pop, specimen#)
species <- rep(c("mexicana", "occidentalis", "decipiens", "palustris"), times = c(24, 12, 24, 36))
mexicanapop <- factor(rep (c("1", "2"), each = 3, times = 4))
occidentalispop <- factor(rep ("3", each = 3, times = 4)) #no ( ) around 3 because of single vector
decipienspop <- factor(rep (c("4", "5"), each = 3, times = 4))
palustrispop <- factor(rep (c("6", "7", "8"), each = 3, times = 4))
predictor.population <- c(mexicanapop, occidentalispop, decipienspop, palustrispop) #just need to use c() because all are vectors
specimen.number <- factor(rep (c("1", "2", "3", "4"), each = c(6, 3, 6, 9, times = 1))) #error on 'each' argument, try times argument
specimen.number <- factor(rep (c("1", "2", "3", "4"), times = c(6, 3, 6, 9))) #PASS: times argument

#combine all into a dataframe 
sim.bark.fiber.diameter.dirca <- data.frame(species, predictor.population, specimen.number, response.celltotal)
write.csv(sim.bark.fiber.diameter.dirca, file("_data/tidy/sim.bark.fiber.diameter.dirca.csv"))

#look at response
ggplot(sim.bark.fiber.diameter.dirca, aes(species, response.celltotal, fill=specimen.number))+
  geom_boxplot()

###Visualization with Ordination Plot, Multivariate Analysis###
#requires vegan r-package
#rda = redundancy analysis (runs as principle component analysis (pca) in vegan when data has no predictors)

bark.fiber.subset.rda <- bark.fiber.subset[,-c(1:5, 11:13)] #creates dataset for bark.fiber without columns 1 thru 5 & 11 thru 13
head(bark.fiber.subset.rda)
dirca.bark.fiber.rda <- rda(bark.fiber.subset.rda) #runs rda on bark.fiber.subset.rda dataset

summary(rda(bark.fiber.subset.rda))

#creates paired scatterplot matrices, bivariate relationships
#FAILED: PASSED but does not separate by Species
pairs(bark.fiber.subset.rda,
      lower.panel = NULL, 
      col = as.numeric(dirca$Species))

biplot(dirca.bark.fiber.rda) #creates weighted principle component plot
#biplot options
#sites, species, text, points are the options, case sensitive
biplot(dirca.bark.fiber.rda, display = "sites") #removed weighted lines
biplot(dirca.bark.fiber.rda, display = "species") #removed points
biplot(dirca.bark.fiber.rda, type = "text") #replaced points with text
biplot(dirca.bark.fiber.rda, type = "points") #no visual change, default
biplot(dirca.bark.fiber.rda, display = c("sites", "species"), type = c("text", "points"))

spp.names <- levels(dirca$Species) #pulls Species names for legend

#adds hulls, convex polygons around groups of points
ordihull(dirca.bark.fiber.rda, group = dirca$Species, col = c(1,2,3,4)) #FAILED: subscript out of bound
ordihull(dirca.bark.fiber.rda, group = dirca$species, col = c(1,2,3,4)) #FAILED: PASSED but no visual change appeared
ordihull(bark.fiber.subset.rda, group = dirca$species, col = c(1,2,3,4)) #FAILED: PASSED but no visual change appeared
ordihull(bark.fiber.subset.rda, group = dirca$Species) #FAILED: subscript out of bound
ordihull(bark.fiber.subset.rda, group = dirca$species) #FAILED: PASSED but no visual change appeared
ordihull(bark.fiber.subset.rda, groups = species, draw = "polygon", col = "grey90",label=F) #FAILED: PASSED but no visual change appeared

#Vegan Tutorial using ordiplot instead of biplot
#FAILED: created scatterplot of Secondarywall.Thickness.um and Primarywall.Thickness.um
ordiplot(bark.fiber.subset.rda, type = "n")
orditorp(bark.fiber.subset.rda, display = "species",col = "red", air = 0.01)
orditorp(bark.fiber.subset.rda, display = "sites",cex = 1.25, air = 0.01)

#adds legend to plot
legend("topright", col = c(1,2,3,4), lty = 1, legend = spp.names)

#prcomp instead of RDA or PCA
prcomp(bark.fiber.subset.rda)
summary(prcomp(bark.fiber.subset.rda))

#ANALYSIS: MANOVA - is the variation between groups significantly different than within groups?
dplyr::sample_n(bark.fiber.subset.rda, 10) #shows 10 random samples from dataset

bark.fiber.manova <- manova(cbind(bark.fiber.subset.rda$Primarywall.Thickness.um, bark.fiber.subset.rda$Secondarywall.Thickness.um, bark.fiber.subset.rda$Length.um, bark.fiber.subset.rda$Lumen.Diameter.um, bark.fiber.subset.rda$Cell.Total.Diameter.um) ~ Species, data = bark.fiber.subset)
manova(cbind(Primarywall.Thickness.um, Secondarywall.Thickness.um, Length.um, Lumen.Diameter.um, Cell.Total.Diameter.um) ~ Species, data = bark.fiber.subset)

#Multivariate Morphometrics Book
#Discriminate Analysis (to characterize differences between groups)
#MANOVA because two+ independent variables (Species, Population, Specimen.Number, Cell.Type, and Observation) with 2+ dependent variables (Cell.Type Measurements)
library(MASS)
data


##Clear environment (right corner window) to reduce error chances
rm(list = ls())
