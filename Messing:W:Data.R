library(dplyr)
library(ggplot2)
library(magrittr)

DAD2Data <- read.csv("Data/DAD2Data.csv",stringsAsFactors=T)
COMTData <- read.csv("Data/COMT_RS4680.csv",stringsAsFactors=F)
BehaveData <- read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE)

Data <- GeneticData %>%
  left_join(BehaveData,by=c("SubjID")) %>%
  left_join(COMTData,by=c("SubjID"))


table(GeneticData[,2:3])

names(GeneticData)


#by DAD2 data 
Data %>%
  group_by(rs2440390) %>% 
  summarize(Age = mean(Age_At_PhenX_Completion,na.rm=T),
            AgeSD = sd(Age_At_PhenX_Completion,na.rm=T),
            AgeN = length(na.omit(Age_At_PhenX_Completion)),
            AgeInitiation = mean(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeInitiationSD = sd(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeInitiationN = length(na.omit(PHX_Alcohol_Initiation_1))
  )
#by COMT
PlotCOMT <-Data %>%
  group_by(rs4680,rs2440390) %>% 
  summarize(Flanker = mean(TBX_flanker_score,na.rm=T),
            FlankerSD = sd(TBX_flanker_score,na.rm=T),
            FlankerN = length(TBX_flanker_score)) %>%
  mutate(FlankerUpCI = Flanker + 1.96*FlankerSD/sqrt(FlankerN),
         FlankerDownCI = Flanker - 1.96*FlankerSD/sqrt(FlankerN))
PlotCOMT

qplot(rs4680,Flanker,data=PlotCOMT,facets=.~rs2440390) + geom_errorbar(aes(ymax=FlankerUpCI ,ymin = FlankerDownCI ))

PlotCOMT <-Data %>%
  group_by(rs4680,) %>% 
  filter(Age_At_PhenX_Completion  < 30,
         PHX_Alcohol_Initiation_1 < 30) %>%
  summarize(Age = mean(Age_At_PhenX_Completion,na.rm=T),
            AgeSD = sd(Age_At_PhenX_Completion,na.rm=T),
            AgeN = length(na.omit(Age_At_PhenX_Completion)),
            AgeInitiation = mean(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeInitiationSD = sd(PHX_Alcohol_Initiation_1,na.rm=T),
            AgeInitiationN = length(na.omit(PHX_Alcohol_Initiation_1))) %>%
  mutate(AgeInitiationUPCI = AgeInitiation+ 1.96 * (AgeInitiationSD/sqrt(AgeInitiationN)),
         AgeInitiationDownCI = AgeInitiation- 1.96 * (AgeInitiationSD/sqrt(AgeInitiationN))
  )

PlotCOMT

qplot(rs4680, AgeInitiation,data=PlotCOMT)+geom_errorbar(aes(ymin=AgeInitiationUPCI,ymax=AgeInitiationDownCI,width=0))


summary(lm(PHX_IMP_SNSEEK~PHX_IMP_POSURG+rs2440390+TBX_attention_score,data=Data))
summary(aov(PHX_IMP_NEGURG~PHX_IMP_TOTAL*rs2440390,data=Data))

summary(aov(PHX_Alcohol_Initiation_1~Age_At_PhenX_Completion+rs4680,data=Data))






