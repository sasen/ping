library(dplyr)
library(ggplot2)
library(magrittr)

GeneticData <- read.csv("Data/DAD2Data.csv",stringsAsFactors=T)
BehaveData <- read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE)

Data <- GeneticData %>%
  left_join(BehaveData,by=c("SubjID"))

table(GeneticData[,2:3])

Data %>%
  group_by(rs2440390) %>% 
  summarize(Age = mean(Age_At_PhenX_Completion,na.rm=T),
            AgeSD = sd(Age_At_PhenX_Completion,na.rm=T),
            AgeN = length(na.omit(Age_At_PhenX_Completion)),
            AgeInitiation = mean(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeInitiationSD = sd(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeUnititationN = length(na.omit(PHX_Alcohol_Initiation_1))
            )


summary(lm(PHX_IMP_SNSEEK~+PHX_IMP_POSURG+rs2440390*TBX_attention_score,data=Data))
summary(aov(PHX_IMP_NEGURG~PHX_IMP_TOTAL*rs2440390,data=Data))


