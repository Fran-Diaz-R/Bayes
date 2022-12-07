################################################################################
########################## AREA NUMBER INTEGRABILITY ###########################
############################## Training Data ###################################
################################################################################

## Clears all existing variables
rm(list=ls(all=TRUE))

## Install/load relevant packages
# List of relevant packages (add new ones here)
packages <- c('car', 'lme4', 'ggplot2', 'languageR', 'readr', 'tidyr',
              'stringr', 'psych', 'dplyr', 'stats')
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

####Load plot theme
source("C:/Users/franc/OneDrive - University of Iowa/Wasserman's Lab/R scripts/theme_myGG.R")


## Sets working directory
setwd("C:/Users/franc/OneDrive - University of Iowa/Wasserman's Lab/Num - Area Integrability Separability/Shift 1")
path = ("C:/Users/franc/OneDrive - University of Iowa/Wasserman's Lab/Num - Area Integrability Separability/Shift 1")
file.names = dir(path, pattern=".txt")

##################################
########### DataFRAMES ###########
##################################

#### sHIFT 1 ####
birdname = c("43R","48W","54Y","56Y")

Data = NULL
## Cycles through all birds listed above... 
for (k in 1:length(birdname)){
  namelength = nchar(birdname[k])
  numsessions = 0
  for (i in 1:length(file.names)){
    if (str_sub(file.names[i], 1, namelength) == birdname[k]){
      data = read_delim(file.names[i],"\t", col_names=FALSE, escape_double = FALSE, trim_ws = TRUE)
      colnames(data) = c('bird', 'date', 'task', 'trial', 'numLeft', 'numRight',
                         'areaLeft', 'areaRight', 'correct', 'choice', 'accuracy', 'RT')
      Data = rbind(Data, data)
    }
  }
}

## Sort data by date
Data$date = as.Date(Data$date, format = '%m/%d/%y')
Data = Data[order(Data$bird, Data$date), ]

## Make session variable
Data$session = NA
trials = 160
for (b in unique(Data$bird)){
  session = 1
  for (d in unique(Data$date[Data$bird == b])){
    fullcheck = length(Data$session[Data$bird == b & Data$date == d]) == trials
    if (fullcheck){
      Data$session[Data$bird == b & Data$date == d] = session
      session = session + 1
    }
  }
}

Data = na.omit(Data)

## Separate values for S+ and S-
Data$areaIncorrect = ifelse(Data$correct == 'R', Data$areaLeft,
                     ifelse(Data$correct == 'L', Data$areaRight, NA))

Data$numIncorrect = ifelse(Data$correct == 'R', Data$numLeft,
                    ifelse(Data$correct == 'L', Data$numRight, NA))

Data$areaCorrect = ifelse(Data$correct == 'R', Data$areaRight,
                   ifelse(Data$correct == 'L', Data$areaLeft, NA))

Data$numCorrect = ifelse(Data$correct == 'R', Data$numRight,
                  ifelse(Data$correct == 'L', Data$numLeft, NA))

## Transform values to calculate difference in matrix blocks units rather than numerical differences
Data$T_areaInc = factor(Data$areaIncorrect, levels = c(4,5,6,7,9,11,14,17,21),
                                            labels = c(1,2,3,4,5,6,7,8,9))
Data$T_areaInc = as.numeric(Data$T_areaInc)

Data$T_numInc = factor(Data$numIncorrect, levels = c(6,7,8,10,12,14,16,19,22),
                                        labels = c(1,2,3,4,5,6,7,8,9))
Data$T_numInc = as.numeric(Data$T_numInc)

Data$T_areaCorr = factor(Data$areaCorrect, levels = c(4,5,6,7,9,11,14,17,21),
                                            labels = c(1,2,3,4,5,6,7,8,9))
Data$T_areaCorr = as.numeric(Data$T_areaCorr)

Data$T_numCorr = factor(Data$numCorrect, levels = c(6,7,8,10,12,14,16,19,22),
                                         labels = c(1,2,3,4,5,6,7,8,9))
Data$T_numCorr = as.numeric(Data$T_numCorr)

## Calculate distances
# Euclidean distance
Data$Euclidean = sqrt(abs(Data$T_numCorr -Data$T_numInc)^2 + abs(Data$T_areaCorr -Data$T_areaInc)^2)

# City Block distance
Data$CityBlock = abs(Data$T_numCorr -Data$T_numInc) + abs(Data$T_areaCorr -Data$T_areaInc)

Data = subset(Data, Data$session < 41)


##################################
############ ANALYSIS ############
##################################

## Model comparison
# Euclidean distance
Euc_Model = glm(accuracy ~ Euclidean, data = Data, family = "binomial")
summary(Euc_Model)

# City Block distance
CityB_Model = glm(accuracy ~ CityBlock, data = Data, family = "binomial")
summary(CityB_Model)

## Data frame for storing model fits
modmat = data.frame(Bird = rep(c('Aggregate', unique(Data$bird)), each = 2),
                    Model = rep(c('Euclidean', 'CityBlock'), times = 5),
                    AIC = rep(99999, 10),
                    Winner = rep('', 10))
modmat$Winner = as.character(modmat$Winner)
predictors = c('Euclidean', 'CityBlock')

## Model Comparison -- average + individual birds
for (b in unique(modmat$Bird)){
  for (p in unique(predictors)){
    if (b == 'Aggregate'){ # All birds
      m1 = glm(accuracy ~ get(p), data = subset(Data, Data$Block == '4'), family = 'binomial')
    } else { # Individual birds
      m1 = glm(accuracy ~ get(p), data = subset(Data, Data$Block == '4' & Data$bird == b), family = 'binomial')
    }
    modmat$AIC[which(modmat$Model == p & modmat$Bird == b)] = AIC(m1)
  }
  modmat$Winner[which(modmat$Bird == b)] = as.character(modmat$Model[which(modmat$Bird == b & modmat$AIC == min(modmat$AIC[which(modmat$Bird == b)]))])
}


##################################
############# GRAPHS #############
##################################

## Agregated data (all pigeons)
# Reformat data for plot
blockagg = aggregate(Data$accuracy, by=list(Data$numDist,
                                            Data$areaDist),
                     FUN=sum)
colnames(blockagg) = c("numDist", "areaDist","acc")

# Heatmap 
ggplot(blockagg,
       aes(x = factor(numDist),
           y = factor(areaDist),
           fill= acc)) + 
  geom_tile() +
  geom_text(aes(label = round(acc, 1))) +
  coord_fixed() +
  labs(title = 'Aggregate accross sessions',x='Distance in Number', y='Distance in Area') +
  theme_myGG() +
  theme(legend.position = 'right') +
  guides(fill = guide_colourbar(title = 'Proportion correct',
                                barwidth = 0.5,
                                barheight = 30,
                                ticks = FALSE))



## Bird data
# Reformat data for plot
blockagg = aggregate(Data$accuracy, by=list(Data$bird,
                                             Data$task,
                                             Data$numIncorrect,
                                             Data$areaIncorrect),
                     FUN=sum)

colnames(blockagg) = c("bird", "task", "numIncorrect", "areaIncorrect","acc")

# Heatmap 
  ggplot(blockagg,
         aes(x = factor(numIncorrect),
             y = factor(areaIncorrect),
             fill= acc)) + 
    geom_tile() +
    geom_text(aes(label = round(acc, 1))) +
    coord_fixed() +
    scale_y_discrete(breaks = c(4,5,6,7,9,11,14,17,21)) +
    labs(title = '',x='Number', y='Area') +
    facet_wrap(~task) +
    theme_myGG() +
    theme(legend.position = 'right') +
    guides(fill = guide_colourbar(title = 'Proportion correct',
                                  barwidth = 0.5,
                                  barheight = 30,
                                  ticks = FALSE))
  

## Training curves
ggplot(Data,
            aes(x=session, y=accuracy*100,
                group=task, fill=task, color=task)) +                                                                           
  stat_summary(geom='line', fun='mean', size=.9,
               aes(linetype=task)) +
  stat_summary(geom='point', fun='mean', size=3,
               aes(shape=task)) +
  stat_summary(fun.data = mean_se,  
               geom = "errorbar",
               width=.2) +
  #geom_vline(xintercept=10, size=.8, linetype="dashed", color="black") +
  labs(x='Session', y='Percent correct') +
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  theme_myGG() +
  theme(line = element_line(size = .5),
        text = element_text(size = 22),
        legend.position = c(.1, .1))+
  coord_cartesian(ylim=c(30, 100))
