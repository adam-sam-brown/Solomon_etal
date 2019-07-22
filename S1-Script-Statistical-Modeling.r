#######################################################
#Scripts used to conduct logistic regression analyses #
#on data in S1 Table and S2 Table, easily modified to #
#test effects of particular animals, vector species,  #
#and other variables.                                 #
#######################################################

# Directory
setwd('/Your/Working/Directory/')

# Library
library(data.table)
#library(ROCR)

# Read
KHdata <- fread('S1 Table. Kafta Humera data.csv', data.table = F)

# Naming and Recoding
names(KHdata) <- c('Method-ID','Date','Trap','Sand_Fly_Species','Locality','Habitat','Blood_Meal_Source') 


KHdata$animal <- KHdata$Blood_Meal_Source

KHdata$Blood_Meal_Source[KHdata$Blood_Meal_Source == 'human'] <- 1
KHdata$Blood_Meal_Source[grep('human', KHdata$Blood_Meal_Source)] <- 1
KHdata$Blood_Meal_Source[KHdata$Blood_Meal_Source != '1'] <- 0
KHdata$Blood_Meal_Source <- as.integer(KHdata$Blood_Meal_Source)

KHdata$animal[KHdata$animal %in% c('human','negative')] <- 0
KHdata$animal[KHdata$animal != '0'] <- 1
KHdata$animal <- as.integer(KHdata$animal)

KHdata$Habitat[KHdata$Habitat %in% c('in_village','periphery_of_village','domestic_habitat','peridomestic','indoors')] <- 'village'

############
# Modeling #
############

# GLM
mod <- glm(formula = Blood_Meal_Source ~ Habitat + animal, family = binomial(), data = KHdata)
summary(mod)


##############################################
# Adding data from Gebresilassie et al. 2015 #
##############################################

human_only_village <- c('0','Phlebotomus orientalis','0','village',1,0)
human_animal_village <-  c('0','Phlebotomus orientalis','0','village',1,1)
animal_only_village <-  c('0','Phlebotomus orientalis','0','village',0,1)
negative_village <-  c('0','Phlebotomus orientalis','0','village',0,0)

human_only_ff <- c('0','Phlebotomus orientalis','0','farm_fields',1,0)
human_animal_ff <-  c('0','Phlebotomus orientalis','0','farm_fields',1,1)
animal_only_ff <-  c('0','Phlebotomus orientalis','0','farm_fields',0,1)
negative_ff <-  c('0','Phlebotomus orientalis','0','farm_fields',0,0)

gebrtable <- as.data.frame(rbind(human_only_village, human_animal_village, animal_only_village, negative_village, human_only_ff, human_animal_ff, animal_only_ff, negative_ff), stringsAsFactors = F)
gebrtable[,5] <- as.numeric(gebrtable[,5])
gebrtable[,6] <- as.numeric(gebrtable[,6])
names(gebrtable) <- names(KHdata)


######################################################
# Build table using counts (in order defined above)  #
# from S2 Table. Tahtay Adiyabo data.csv"            #
######################################################

gebrtable2 <- c(40, 47, 360, 55, 13, 7, 92, 23)

for (i in 1:length(gebrtable2)) {
  for (j in 1:gebrtable2[i]) {
    KHdata <- rbind(KHdata, gebrtable[i,])
  }
}

# Modeling

mod <- glm(formula = Blood_Meal_Source ~ Habitat + animal family = binomial(), data = KHdata)
summary(mod)

non_meta <- glm(formula = Blood_Meal_Source ~ Habitat + animal, family = binomial(), data = KHdata[747:1383,])
summary(non_meta)
