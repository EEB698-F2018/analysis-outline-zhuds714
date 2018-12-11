##  ----------------------------------------------------------  ##
# PUBLICATION GRAPHICS ####
##  ----------------------------------------------------------  ##

##Install Libraries and Packages EX:install.packages("tidyverse")
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats
install.packages("maps")
library(maps) #package of maps
install.packages("ggrepel")
library(ggrepel) #geom_text_repel allows text to be moved away from plotted point
install.packages("pixiedust")
library(pixiedust)
install.packages("psych") #describeBy reports mean, min, max, SE, SD, and other basic statistics by species and cell dimension
library(psych)

#Load tidy.csv dataset.  Default: Header=TRUE
dirca.tidy <- read.csv("_data/tidy/dirca_tidy.csv", na.strings = c("", "NA", "na", " "))

#Explore dataset
str(dirca.tidy) #check variable class assignment

#Check Variable Class Assignment
#Correct specimen_num & observation Class Assignments
dirca.tidy$Specimen.Number <- as.factor(dirca.tidy$Specimen.Number)
dirca.tidy$Observation <- as.factor(dirca.tidy$Observation)
str(dirca.tidy) #check assignments have changed

##  ----------------------------------------------------------  ##
# POPULATION MAP ####
##  ----------------------------------------------------------  ##
states <- map_data("state") #identify map

#Identify coordinates for populations (lat, long)
#dm.isu (ISU Arboretum) = 42.0195701, -93.6576003
#dm.ri (Kingston) = 41.4728761, -71.5406606
#do.jasper (San Mateo County)= 37.4031161, -122.244288
#dd.eureka (Eureka Springs)= 36.465955, -93.760285
#dd.overland (Overland Arboretum)= 38.795838, -94.691634
#dp.me (Aroostook County)= 45.931283, -68.320000
#dp.fl (Torreya State Park)= 30.590637, -84.935303
#dp.nd (Cavalier)= 48.964722, -98.100278

coordinates <- data.frame(
  long = c(-93.6576003, -71.5406606, -122.244288, -93.760285, -94.691634, -68.320000, -84.935303, -98.100278),
  lat = c(42.0195701, 41.4728761, 37.4031161, 36.465955, 38.795838, 45.931283, 30.590637, 48.964722),
  names = c("D.mexicana", "D.mexicana", "D.occidentalis", "D.decipiens", "D.decipiens", "D.palustris", "D.palustris", "D.palustris"),
  stringsAsFactors = TRUE)

pop.map <- ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, fill = region, group = group), color = "white") + #color=border color
  coord_fixed(1.3) +
  guides(fill = FALSE)

pop.map +
  geom_point(data = coordinates, aes(x = long, y = lat), color = "black", size = 2) +
  geom_point(data = coordinates, aes(x = long, y = lat), color = "green", size = 1) +
  geom_text_repel(data = coordinates, mapping = aes(x = long, y = lat, label = names)) + #replaced points with text
  theme_void() #removes all background aes noise

ggsave("_graphics/Population.Sampling.Map.png")

##  ----------------------------------------------------------  ##
# GRAPHS ####
##  ----------------------------------------------------------  ##

ggplot(bark.fiber.subset, aes(Species, Primarywall.Thickness.um))+
  geom_boxplot()+
  labs(x="", y="Primary Wall Thickness (um)")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Primary.Boxplot.png")

ggplot(bark.fiber.subset, aes(Species, Secondarywall.Thickness.um))+
  geom_boxplot()+
  labs(x="", y="Secondary Wall Thickness (um)")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Secondary.Boxplot.png")

ggplot(bark.fiber.subset, aes(Species, Length.um))+
  geom_boxplot()+
  labs(x="", y="Cell Length (um)")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Length.Boxplot.png")

ggplot(bark.fiber.subset, aes(Species, Lumen.Diameter.um))+
  geom_boxplot()+
  labs(x="", y="Lumen Diameter (um)")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Lumen.Boxplot.png")

ggplot(bark.fiber.subset, aes(Species, Cell.Total.Diameter.um))+
  geom_boxplot()+
  labs(x="", y="Cell Total Diameter (um)")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Diameter.Boxplot.png")

##  ----------------------------------------------------------  ##
# TABLES ####
##  ----------------------------------------------------------  ##
#pixiedust table making package is not compatible with RRPP so tables will be made in excel
#Pull summmary statistics to enter into excel
describeBy(bark.fiber.subset, bark.fiber.subset$Species)
#describeBy reports mean, min, max, SE, SD, and other basic statistics by species and cell dimension
