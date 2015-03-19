library(dplyr)
library(ggplot2)
library(magrittr)

BehaveData <- tbl_df(read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE))

## I know we're going to subset on phenx survey age at some point, so i'm mocking up some data frames of who did and didn't take the survey to get ballpark 
phx = subset(BehaveData, Age_At_PhenX_Completion>1)
no_phx = subset(BehaveData, is.na(Age_At_PhenX_Completion))
range(no_phx$Age,na.rm=TRUE)  # 3 21
range(phx$Age,na.rm=TRUE)  # 7.08 21.00
range(phx$Age_At_PhenX_Completion,na.rm=TRUE)  # 8.25 22.67

table(phx$PHX_Alcohol_LifeUse_1)
table(phx$PHX_Substance_LifeUse_5)  # marijuana
table(phx$PHX_Substance_MJ_3)  ## marijuana last use
table(phx$PHX_Substance_Coc_1)
table(phx$PHX_Substance_Stims_1)

table(no_phx$PHX_Substance_Meth_1)
table(phx$PHX_Substance_Sedtv_1)
table(phx$PHX_Substance_Tranq_1)
table(phx$PHX_Substance_Painkiller_1)
table(phx$PHX_Substance_Halluc_1)
table(phx$PHX_Substance_Inhalnt_1)
table(phx$PHX_Substance_Heroin_1)

## some age deltas in our set... enrolled vs completed PhenX
alc = subset(BehaveData, !is.na(PHX_Alcohol_LifeUse_1) & !is.na(Age_At_PhenX_Completion))
younguns = subset(alc, Age < 14)
yng = younguns[c("Age","Age_At_PhenX_Completion")]
yng$phx_delay = yng$Age_At_PhenX_Completion - yng$Age
  
allp = phx[c("Age","Age_At_PhenX_Completion")]
allp = allp[complete.cases(allp)]
allp$phx_delay = allp$Age_At_PhenX_Completion - allp$Age

## OK, PHX_Alcohol_LifeUse_1==1 includes healthy drinking habits in college and parental guidance on how to drink wine with dinner. 
## who gets drunk? let's use that as the substance abuse DV
first_drunk = sapply(BehaveData$PHX_Alcohol_LifeAbuse_2, as.integer)
first_drunk.tab = table(first_drunk)
sum(first_drunk.tab) - first_drunk.tab["0"] ## OK, 72 kids got drunk
## just make sure they started getting drunk AFTER they started drinking!
init_drink = sapply(BehaveData$PHX_Alcohol_Initiation_1, as.integer)
table(init_drink, first_drunk)
confused_drinkers = subset(BehaveData, (first_drunk != 0) & (init_drink != 99) & (init_drink > first_drunk))  # you started drinking at age 20 and first got drunk at age 14??? REJECTED
confusedID = confused_drinkers$SubjID


BehaveData %<>% 
  mutate(got_drunk =ifelse(PHX_Alcohol_LifeAbuse_2=="",0,
                           ifelse(substr(PHX_Alcohol_LifeAbuse_2,1,1)==0,0,1)))
## verify that nobody got put somewhere strange
table(BehaveData$got_drunk,BehaveData$PHX_Alcohol_LifeAbuse_2)

Druggies <- BehaveData %>%
  filter(  got_drunk == 1             # we created            
         | PHX_Tobacco_Status_1==1
         | PHX_Substance_LifeUse_5==1  # marijuana
         | PHX_Substance_Coc_1==1    # crack or cocaine
         | PHX_Substance_Stims_1==1
         | PHX_Substance_Meth_1==1
         | PHX_Substance_Sedtv_1==1
         | PHX_Substance_Tranq_1==1
         | PHX_Substance_Painkiller_1==1
         | PHX_Substance_Halluc_1==1
         | PHX_Substance_Inhalnt_1==1
         | PHX_Substance_Heroin_1==1
) %>%
  filter(Age_At_PhenX_Completion > 14 | Age > 14, # age protocol violation
         SubjID != confusedID) ## that one confused drunk

Naives <- BehaveData %>%
  filter( got_drunk == 0, 
           PHX_Tobacco_Status_1==2,
           PHX_Substance_LifeUse_5==2,  # marijuana
           PHX_Substance_Coc_1==2,    # crack or cocaine
           PHX_Substance_Stims_1==2,
           PHX_Substance_Meth_1==2,
           PHX_Substance_Sedtv_1==2,
           PHX_Substance_Tranq_1==2,
           PHX_Substance_Painkiller_1==2,
           PHX_Substance_Halluc_1==2,
           PHX_Substance_Inhalnt_1==2,
           PHX_Substance_Heroin_1==2  ) %>%
  filter(Age_At_PhenX_Completion > 14 | Age > 14, # age protocol violation
         SubjID != confusedID) ## that one confused drunk

## this is just a quick check
## how many people said "Don't know / Decline to state" on any question?
Dunnos <- BehaveData %>%
  filter( PHX_Tobacco_Status_1==99  ## don't know/won't state
          | PHX_Substance_LifeUse_5==99)  # marijuana don't 
dim(Dunnos)[1]  ## 8 people, OK, whatever
          
DruggieID <- Druggies %>%
  select(  SubjID, 
           Alcohol = got_drunk,
           Tobacco = PHX_Tobacco_Status_1,
           Marijuana = PHX_Substance_LifeUse_5,  # marijuana
           Coca = PHX_Substance_Coc_1,
           Stims = PHX_Substance_Stims_1,
           Meth = PHX_Substance_Meth_1,
           Sedtv = PHX_Substance_Sedtv_1,
           Tranq = PHX_Substance_Tranq_1,
           Paink = PHX_Substance_Painkiller_1,
           Halluc = PHX_Substance_Halluc_1,
           Inhal = PHX_Substance_Inhalnt_1,
           Heroin = PHX_Substance_Heroin_1  )

NaiveID <- Naives %>%
  select(  SubjID, 
           Alcohol = got_drunk,
           Tobacco = PHX_Tobacco_Status_1,
           Marijuana = PHX_Substance_LifeUse_5,  # marijuana
           Coca = PHX_Substance_Coc_1,
           Stims = PHX_Substance_Stims_1,
           Meth = PHX_Substance_Meth_1,
           Sedtv = PHX_Substance_Sedtv_1,
           Tranq = PHX_Substance_Tranq_1,
           Paink = PHX_Substance_Painkiller_1,
           Halluc = PHX_Substance_Halluc_1,
           Inhal = PHX_Substance_Inhalnt_1,
           Heroin = PHX_Substance_Heroin_1  )


intersect(Druggies,Naives)$SubjID  # should be zero
intersect(Druggies,Dunnos)$SubjID  # 4 people
intersect(Naives,Dunnos)$SubjID  # 0 people ... 
## that means 4 Dunnos were otherwise naive, and said 99 on Tobacco/MJ

table(DruggieID$Tobacco,DruggieID$Alcohol) # 14 have smoked but never drink
smokers = subset(DruggieID, Tobacco == 1)
smokers = select(smokers, -Tobacco)
filter(smokers, Alcohol==0,
       Marijuana==2 | Marijuana==99,
       Coca==2, Stims==2, Meth==2,Sedtv==2,Tranq==2,Paink==2,
       Halluc==2, Inhal==2,Heroin==2)$SubjID  ### this is just 6 people


filter(Druggies, PHX_Alcohol_LifeAbuse_5.1==5)$SubjID
table(Druggies$got_drunk,Druggies$PHX_Alcohol_LifeAbuse_5.1)
table(Druggies$PHX_Tobacco_Status_4)

########### HERE ARE THE QUESTIONS
#----PHX_Alcohol_LifeAbuse_2,  --> became got_drunk (1=Ever, 0 =Never)
#"How old were you the first time you got drunk, that is, your speech was slurred or you were unsteady on your feet?"
#----PHX_Tobacco_Status_1,
#"Have you ever smoked part or all of a cigarette? (1=Yes, 2=No, 99=Don't Know/Decline to state)"
#----PHX_Substance_LifeUse_5,
#"In your entire life, have you EVER tried marijuana (pot, weed, hash, bud, doobie, reefer, mary-jane, puff)? (1=Yes, 2=No, 99=Don't know/Decline to State)
#----PHX_Substance_Coc_1,
#"Have you EVER used cocaine or crack? (1=Yes, 2=No)"
#----PHX_Substance_Stims_1,
#"Have you EVER used stimulants, for example, Preludin, Benzedrine, Methedrine, Ritalin, uppers, or speed? (1=Yes, 2=No)"
#----PHX_Substance_Meth_1,
#"Have you EVER used meth-amphetamines (crystal meth, ice, batu, crank, tine, tweak, glass, junk)? (1=Yes, 2=No)"
#----PHX_Substance_Sedtv_1,
#"Have you EVER used sedatives, for example, sleeping pills, barbiturates, Seconal, Quaaludes, or Chloral Hydrate? (1=Yes, 2=No)"
#----PHX_Substance_Tranq_1,
#"Have you EVER used tranquilizers or anti-anxiety drugs, for example, Valium, Librium, muscle relaxants, or Zanax (1=Yes, 2=No)"
#----PHX_Substance_Painkiller_1,
#"Have you EVER used painkillers, for example, Codeine, Darvon, Percodan, OxyContin, Dilaudid, Demerol, Celebrex, or Vioxx? (1=Yes, 2=No)"
#----PHX_Substance_Halluc_1,
#"Have you EVER used hallucinogens, for example, Ecstasy/MDMA, GHB, Ketamine, LSD, mescaline, psilocybin, PCP, angel dust, or peyote? (1=Yes, 2=No)
#----PHX_Substance_Inhalnt_1,
#"Have you EVER used inhalants or solvents, for example, amyl nitrite, nitrous oxide, glue, toluene or gasoline? (1=Yes, 2=No)
#----PHX_Substance_Heroin_1,
#"Have you EVER used heroin? (1=Yes, 2=No)"

