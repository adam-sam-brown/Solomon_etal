# Directory
setwd('/Users/Ron/Dropbox/A-Clouse_working_files/56-Solomon_material/blood-meal_ms/analysis/')

# Library
library(data.table)
#library(ROCR)

# Read
elisa <- fread('Bloodmeal_ELISA-Result.csv', data.table = F)
pcr <- fread('Bloodmeal_PCR-data.csv', data.table = F)

#############
# Harmonize #
#############

# Naming
elisa$V8 <- NULL
elisa$V9 <- NULL
names(elisa) <- c('id','spp','locality','habitat','method','date','host')
names(pcr) <- c('id','locality','habitat','method','date','spp','pcr','host')

# Combine
combo <- merge(elisa,pcr,all=T)
combo$pcr <- NULL
combo$date <- NULL

# Harmonize columns
combo$host[combo$host == ''] <- 'negative'
combo$host[combo$host == '---'] <- 'negative'
combo$host[combo$host == '----'] <- 'negative'
combo$host <- tolower(combo$host)
combo$animal <- combo$host

combo$host[combo$host == 'human'] <- 1
combo$host[grep('human', combo$host,ignore.case = T)] <- 1
combo$host[combo$host != '1'] <- 0
combo$host <- as.integer(combo$host)

combo$animal[combo$animal %in% c('human','negative')] <- 0
combo$animal[combo$animal != '0'] <- 1
combo$animal <- as.integer(combo$animal)

combo$habitat <- tolower(combo$habitat)
combo$habitat[combo$habitat %in% c('iv','pv','dh','pd','ind')] <- 'v'

combo$spp <- tolower(combo$spp)
spp_p <- unique(grep('p\\.', combo$spp, value = T))

combo$locality <- tolower(combo$locality)
combo$method <- tolower(combo$method)

combo <- combo[complete.cases(combo),]
combo <- subset(combo, 
                habitat %in% c('ff','v')
                #& spp %in% spp_p #names(table(combo$spp)[table(combo$spp) > 9])
                #& locality %in% names(table(combo$locality)[table(combo$locality) > 9]) 
                #& method == 'cdc'
                )

############
# Modeling #
############

# GLM
mod <- glm(formula = host ~ habitat + animal, family = binomial(), data = combo)
summary(mod)

logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(prob)
}

# ROC
#p <- predict(mod, newdata=combo, type="response")
#pr <- prediction(p, combo$host)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)
#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc

#################
# Meta-analysis #
#################

# Gebr Data

hum_v <- c('0','p.orientalist',NA,'v',NA,1,0)
combo_v <-  c('0','p.orientalist',NA,'v',NA,1,1)
animal_v <-  c('0','p.orientalist',NA,'v',NA,0,1)
non_v <-  c('0','p.orientalist',NA,'v',NA,0,0)

hum_f <- c('0','p.orientalist',NA,'ff',NA,1,0)
combo_f <-  c('0','p.orientalist',NA,'ff',NA,1,1)
animal_f <-  c('0','p.orientalist',NA,'ff',NA,0,1)
non_f <-  c('0','p.orientalist',NA,'ff',NA,0,0)

vtab <- as.data.frame(rbind(hum_v, combo_v, animal_v, non_v, hum_f, combo_f, animal_f, non_f), stringsAsFactors = F)
vtab[,1] <- as.numeric(vtab[,1])
vtab[,6] <- as.numeric(vtab[,6])
vtab[,7] <- as.numeric(vtab[,7])
names(vtab) <- names(combo)

repv <- c(40, 47, 360, 55, 13, 7, 92, 23)

for (i in 1:length(repv)) {
  for (j in 1:repv[i]) {
    combo <- rbind(combo, vtab[i,])
  }
}

# Modeling

mod <- glm(formula = host ~ habitat + animal, family = binomial(), data = combo)
summary(mod)

non_meta <- glm(formula = host ~ habitat + animal, family = binomial(), data = combo[747:1383,])
summary(non_meta)
