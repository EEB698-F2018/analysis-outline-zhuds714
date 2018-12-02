# Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
# DIRCA ANATOMICAL COMPARISION ANALYSIS ####
##  ----------------------------------------------------------  ##

##  ----------------------------------------------------------  ##
# DATA WRANGLNG ####
##  ----------------------------------------------------------  ##

##Install Libraries and Packages EX:install.packages("RRPP")
library(RRPP)
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats

#Load .csv dataset.  Default: Header=TRUE
dirca <- read.csv("_data/raw/dirca.csv", na.strings = c("", "NA", "na", " "))

#Explore dataset
summary(dirca)
names(dirca)

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

#Write tidy dataset .csv file
write.csv(dirca, "_data/tidy/dirca_tidy.csv", row.names=F)

#Load .csv dataset.  Default: Header=TRUE
dirca.tidy <- read.csv("_data/tidy/dirca_tidy.csv", na.strings = c("", "NA", "na", " "))

#Explore tidy dataset
summary(dirca.tidy)
names(dirca.tidy)

##  ----------------------------------------------------------  ##
# CREATING DATA SUBSETS ####
##  ----------------------------------------------------------  ##

#Bark.Fiber Subset
subset.bark.fiber <- filter(dirca.tidy, Cell.Type == "bark_fiber") #find rows/cases where conditions are true
#Bark.Fiber Subset minus NA columns (Cell.Type, Lignified Y.N, Number.of.Cell.Width & Seriate.Number)
bark.fiber.subset <- subset.bark.fiber[,-c(4, 11:13)]

#Sieve.Tube.Mem Subset
subset.sieve.tube.mem <- filter(dirca.tidy, Cell.Type == "sieve_tube_mem")
#Sieve.Tube.Mem Subset minus NA columns (Lignified Y.N, Number.of.Cell.Width & Seriate.Number)
sieve.tube.mem.subset <- subset.sieve.tube.mem[,-c(11:13)]

#Phloem.Parenchyma Subset
subset.phloem.parenchyma <- filter(dirca.tidy, Cell.Type == "phloem_parenchyma")
#Phloem.Parenchyma Subset minus NA columns (Secondarywall.Thickness.um, Lignified Y.N, Number.of.Cell.Width & Seriate.Number)
phloem.parenchyma.subset <- subset.phloem.parenchyma[,-c(7, 11:13)]

#Companion.Cell Subset
subset.companion.cell <- filter(dirca.tidy, Cell.Type == "companion_cell")
#Companion.Cell Subset minus NA columns (Secondarywall.Thickness.um, Lignified Y.N, Number.of.Cell.Width & Seriate.Number)
companion.cell.subset <- subset.companion.cell[,-c(7, 11:13)]

#Ray Subset
subset.ray <- filter(dirca.tidy, Cell.Type == "ray")
#Ray Subset minus NA columns (Secondarywall.Thickness.um & Lignified Y.N)
Ray.subset <- subset.ray[,-c(7, 11)]

##  ----------------------------------------------------------  ##
# DATA EXPLORATION AND VISUALIZATION ####
##  ----------------------------------------------------------  ##

#Histograms to check for normality
ggplot(bark.fiber.subset, aes(x = Primarywall.Thickness.um, fill = Species)) +
  geom_histogram()

ggplot(bark.fiber.subset, aes(x = Secondarywall.Thickness.um, fill = Species)) +
  geom_histogram()

ggplot(bark.fiber.subset, aes(x = Length.um, fill = Species)) +
  geom_histogram()

ggplot(bark.fiber.subset, aes(x = Lumen.Diameter.um, fill = Species)) +
  geom_histogram()

ggplot(bark.fiber.subset, aes(x = Cell.Total.Diameter.um, fill = Species)) +
  geom_histogram()

##  ----------------------------------------------------------  ##
# DATA ANALYSIS ####
##  ----------------------------------------------------------  ##

#Model Fitting: Permutational univariate ANOVA (perANOVA)

#Bark.fiber.subset & Pairwise
bark.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.primary)
anova(bark.primary)
bark.primary.pairwise <- pairwise(bark.primary, groups = bark.fiber.subset$Species)
summary(bark.primary.pairwise, confidence = .95)

bark.secondary <- lm.rrpp(Secondarywall.Thickness.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.secondary)
anova(bark.secondary)
bark.secondary.pairwise <- pairwise(bark.secondary, groups = bark.fiber.subset$Species)
summary(bark.secondary.pairwise, confidence = .95)

bark.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.length)
anova(bark.length)
bark.length.pairwise <- pairwise(bark.length, groups = bark.fiber.subset$Species)
summary(bark.length.pairwise, confidence = .95)

bark.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.lumen)
anova(bark.lumen)
bark.lumen.pairwise <- pairwise(bark.lumen, groups = bark.fiber.subset$Species)
summary(bark.lumen.pairwise, confidence = .95)

bark.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.diameter)
anova(bark.diameter)
bark.diameter.pairwise <- pairwise(bark.diameter, groups = bark.fiber.subset$Species)
summary(bark.diameter.pairwise, confidence = .95)

