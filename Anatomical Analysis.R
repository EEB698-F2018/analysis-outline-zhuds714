#Clear environment to reduce error chances
rm(list = ls())

##  ----------------------------------------------------------  ##
# DIRCA ANATOMICAL COMPARISION ANALYSIS ####
##  ----------------------------------------------------------  ##

##Install Libraries and Packages EX:install.packages("RRPP")
library(RRPP)
library(tidyverse) #loads ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, forcats

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
# CREATE DATA SUBSETS BY CELL TYPE ####
##  ----------------------------------------------------------  ##

#Bark.Fiber Subset
subset.bark.fiber <- filter(dirca.tidy, Cell.Type == "bark_fiber") #find rows/cases where conditions are true
#Bark.Fiber Subset minus NA columns (Lignified Y.N, Number.of.Cell.Width & Seriate.Number)
bark.fiber.subset <- subset.bark.fiber[,-c(11:13)]

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
ray.subset <- subset.ray[,-c(7, 11)]

##  ----------------------------------------------------------  ##
# DATA EXPLORATION AND VISUALIZATION ####
##  ----------------------------------------------------------  ##
#Histograms to check for normality
#Only checking bark.fiber to learn how to transform data

#Bark Fiber
ggplot(bark.fiber.subset, aes(x = Primarywall.Thickness.um, fill = Species)) +
  geom_histogram()+
  labs(x="Primary Wall Thickness (um)", y="Frequency")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Primary.png")
#no need to transform data

ggplot(bark.fiber.subset, aes(x = Secondarywall.Thickness.um, fill = Species)) +
  geom_histogram()+
  labs(x="Secondary Wall Thickness (um)", y="Frequency")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Secondary.png")
#no need to transform data

ggplot(bark.fiber.subset, aes(x = Length.um, fill = Species)) +
  geom_histogram()+
  labs(x="Cell Length (um)", y="Frequency")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Length.png")
#no need to transform data

ggplot(bark.fiber.subset, aes(x = Lumen.Diameter.um, fill = Species)) +
  geom_histogram()+
  labs(x="Lumen Diameter (um)", y="Frequency")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Lumen.png")
#needs transformation
bark.fiber.subset.Ln <- mutate(bark.fiber.subset, Ln.Lumen.Diameter.um = log(Lumen.Diameter.um)) #add column and values for Ln.Lumen.Diameter.um
ggplot(bark.fiber.subset.Ln, aes(x = Ln.Lumen.Diameter.um, fill = Species)) +
  geom_histogram()+
  labs(x="Ln(Lumen Diameter)", y="Frequency")+
  theme_bw()
#transformation did not benefit normality

ggplot(bark.fiber.subset, aes(x = Cell.Total.Diameter.um, fill = Species)) +
  geom_histogram()+
  labs(x="Cell Total Diameter (um)", y="Frequency")+
  theme_bw()

ggsave("_graphics/Bark.Fiber.Diameter.png")
#no need to transform data

##  ----------------------------------------------------------  ##
# DATA ANALYSIS ####
##  ----------------------------------------------------------  ##

#Model Fitting: Permutational univariate ANOVA (perANOVA)
#Since 12/3/18 met with Lynn Clark (committee member) and stats consultant and an ANOVA was deterined to answer my question most directly

#bark.fiber.subset & Pairwise
bark.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.primary)
anova(bark.primary) #p = 0.215
bark.primary.pairwise <- pairwise(bark.primary, groups = bark.fiber.subset$Species)
summary(bark.primary.pairwise, confidence = .95) #occidentalis:palustris p = 0.0370

bark.secondary <- lm.rrpp(Secondarywall.Thickness.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.secondary)
anova(bark.secondary) #p = 0.215
bark.secondary.pairwise <- pairwise(bark.secondary, groups = bark.fiber.subset$Species)
summary(bark.secondary.pairwise, confidence = .95) #occidentalis:palustris p = 0.0370

bark.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.length)
anova(bark.length) #p = 0.348
bark.length.pairwise <- pairwise(bark.length, groups = bark.fiber.subset$Species)
summary(bark.length.pairwise, confidence = .95)

bark.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.lumen)
anova(bark.lumen) #p = 0.861
bark.lumen.pairwise <- pairwise(bark.lumen, groups = bark.fiber.subset$Species)
summary(bark.lumen.pairwise, confidence = .95)

bark.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = bark.fiber.subset, iter = 999)
summary(bark.diameter)
anova(bark.diameter) #p = 0.560
bark.diameter.pairwise <- pairwise(bark.diameter, groups = bark.fiber.subset$Species)
summary(bark.diameter.pairwise, confidence = .95)

#sieve.tube.mem.subset & Pairwise
sieve.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = sieve.tube.mem.subset, iter = 999)
summary(sieve.primary)
anova(sieve.primary) #p = 0.196
sieve.primary.pairwise <- pairwise(sieve.primary, groups = sieve.tube.mem.subset$Species)
summary(sieve.primary.pairwise, confidence = .95) #decipiens:occidentalis p = 0.0250

sieve.secondary <- lm.rrpp(Secondarywall.Thickness.um ~ Species, SS.type = "I", data = sieve.tube.mem.subset, iter = 999)
summary(sieve.secondary)
anova(sieve.secondary) #p = 0.196
sieve.secondary.pairwise <- pairwise(sieve.secondary, groups = sieve.tube.mem.subset$Species)
summary(sieve.secondary.pairwise, confidence = .95) #decipiens:occidentalis p = 0.0250

sieve.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = sieve.tube.mem.subset, iter = 999)
summary(sieve.length)
anova(sieve.length) #p = 0.004
sieve.length.pairwise <- pairwise(sieve.length, groups = sieve.tube.mem.subset$Species)
summary(sieve.length.pairwise, confidence = .95)
  #decipiens:mexicana p = 0.039
  #decipiens:occidentalis p = 0.025
  #mexicana:palustris p = 0.007
  #occidentalis:palustris p = 0.003

sieve.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = sieve.tube.mem.subset, iter = 999)
summary(sieve.lumen)
anova(sieve.lumen) #p = 0.158
sieve.lumen.pairwise <- pairwise(sieve.lumen, groups = sieve.tube.mem.subset$Species)
summary(sieve.lumen.pairwise, confidence = .95) #decipiens:occidentalis p = 0.023

sieve.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = sieve.tube.mem.subset, iter = 999)
summary(sieve.diameter)
anova(sieve.diameter) #p = 0.840
sieve.diameter.pairwise <- pairwise(sieve.diameter, groups = sieve.tube.mem.subset$Species)
summary(sieve.diameter.pairwise, confidence = .95)

#phloem.parenchyma.subset & Pairwise
parenchyma.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = phloem.parenchyma.subset, iter = 999)
summary(parenchyma.primary)
anova(parenchyma.primary) #p = 0.963
parenchyma.primary.pairwise <- pairwise(parenchyma.primary, groups = phloem.parenchyma.subset$Species)
summary(parenchyma.primary.pairwise, confidence = .95)

parenchyma.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = phloem.parenchyma.subset, iter = 999)
summary(parenchyma.length)
anova(parenchyma.length) #p = 0.002
parenchyma.length.pairwise <- pairwise(parenchyma.length, groups = phloem.parenchyma.subset$Species)
summary(parenchyma.length.pairwise, confidence = .95)
  #decipiens:mexicana p = 0.036
  #mexicana"palustris p = 0.001
  #occidentalis:palustris p = 0.023

parenchyma.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = phloem.parenchyma.subset, iter = 999)
summary(parenchyma.lumen)
anova(parenchyma.lumen) #p = 0.158
parenchyma.lumen.pairwise <- pairwise(parenchyma.lumen, groups = phloem.parenchyma.subset$Species)
summary(parenchyma.lumen.pairwise, confidence = .95) #decipiens:occidentalis p = 0.027

parenchyma.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = phloem.parenchyma.subset, iter = 999)
summary(parenchyma.diameter)
anova(parenchyma.diameter) #p = 0.109
parenchyma.diameter.pairwise <- pairwise(parenchyma.diameter, groups = phloem.parenchyma.subset$Species)
summary(parenchyma.diameter.pairwise, confidence = .95) #decipiens:occidentalis p = 0.0100

#companion.cell.subset & Pairwise
companion.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = companion.cell.subset, iter = 999)
summary(companion.primary)
anova(companion.primary) #p = 0.963
companion.primary.pairwise <- pairwise(companion.primary, groups = companion.cell.subset$Species)
summary(companion.primary.pairwise, confidence = .95)

companion.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = companion.cell.subset, iter = 999)
summary(companion.length)
anova(companion.length) #p = 0.002
companion.length.pairwise <- pairwise(companion.length, groups = companion.cell.subset$Species)
summary(companion.length.pairwise, confidence = .95)
  #decipiens:mexicana p = 0.036
  #mexicana"palustris p = 0.001
  #occidentalis:palustris p = 0.023

companion.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = companion.cell.subset, iter = 999)
summary(companion.lumen)
anova(companion.lumen) #p = 0.158
companion.lumen.pairwise <- pairwise(companion.lumen, groups = companion.cell.subset$Species)
summary(companion.lumen.pairwise, confidence = .95) #decipiens:occidentalis p = 0.024

companion.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = companion.cell.subset, iter = 999)
summary(companion.diameter)
anova(companion.diameter) #p = 0.109
companion.diameter.pairwise <- pairwise(companion.diameter, groups = companion.cell.subset$Species)
summary(companion.diameter.pairwise, confidence = .95) #decipiens:occidentalis p = 0.010

#ray.subset & Pairwise
ray.primary <- lm.rrpp(Primarywall.Thickness.um ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.primary)
anova(ray.primary) #p = 0.443
ray.primary.pairwise <- pairwise(ray.primary, groups = ray.subset$Species)
summary(ray.primary.pairwise, confidence = .95)

ray.length <- lm.rrpp(Length.um ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.length)
anova(ray.length) #p = 0.003
ray.length.pairwise <- pairwise(ray.length, groups = ray.subset$Species)
summary(ray.length.pairwise, confidence = .95)
  #decipiens:mexicana p = 0.028
  #mexicana:palustris p = 0.002
  #occidentalis:palustris p = 0.049

ray.lumen <- lm.rrpp(Lumen.Diameter.um ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.lumen)
anova(ray.lumen) #p = 0.158
ray.lumen.pairwise <- pairwise(ray.lumen, groups = ray.subset$Species)
summary(ray.lumen.pairwise, confidence = .95) #decipiens:occidentalis p = 0.025

ray.diameter <- lm.rrpp(Cell.Total.Diameter.um ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.diameter)
anova(ray.diameter) #p = 0.170
ray.diameter.pairwise <- pairwise(ray.diameter, groups = ray.subset$Species)
summary(ray.diameter.pairwise, confidence = .95) #occidentalis:palustris p = 0.042

ray.cell.num <- lm.rrpp(Number.of.Cell.Width ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.cell.num)
anova(ray.cell.num) #p = 0.9995
ray.cell.num.pairwise <- pairwise(ray.cell.num, groups = ray.subset$Species)
summary(ray.cell.num.pairwise, confidence = .95)

ray.seriate <- lm.rrpp(Seriate.Number ~ Species, SS.type = "I", data = ray.subset, iter = 999)
summary(ray.seriate)
anova(ray.seriate) #p = 0.5005
ray.seriate.pairwise <- pairwise(ray.seriate, groups = ray.subset$Species)
summary(ray.seriate.pairwise, confidence = .95)

##  ----------------------------------------------------------  ##
# DATA RESUTLS & GRAPHICS ####
##  ----------------------------------------------------------  ##

ggplot(bark.fiber.subset, aes(Species, Primarywall.Thickness.um))+
  geom_boxplot()+
  labs(x="", y="Primary Wall Thickness (um)")+
  theme_bw()

ggsave("Bark.Fiber.Primary.Boxplot.png")

ggplot(bark.fiber.subset, aes(Species, Secondarywall.Thickness.um))+
  geom_boxplot()+
  labs(x="", y="Secondary Wall Thickness (um)")+
  theme_bw()

ggsave("Bark.Fiber.Secondary.Boxplot.png")
  
ggplot(bark.fiber.subset, aes(Species, Length.um))+
  geom_boxplot()+
  labs(x="", y="Cell Length (um)")+
  theme_bw()

ggsave("Bark.Fiber.Length.Boxplot.png")
  
ggplot(bark.fiber.subset, aes(Species, Lumen.Diameter.um))+
  geom_boxplot()+
  labs(x="", y="Lumen Diameter (um)")+
  theme_bw()

ggsave("Bark.Fiber.Lumen.Boxplot.png")
  
ggplot(bark.fiber.subset, aes(Species, Cell.Total.Diameter.um))+
  geom_boxplot()+
  labs(x="", y="Cell Total Diameter (um)")+
  theme_bw()

ggsave("Bark.Fiber.Diameter.Boxplot.png")
