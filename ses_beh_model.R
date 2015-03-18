library(dplyr)
library(ggplot2)
library(magrittr)

BehaveData <- tbl_df(read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE))

phx = subset(BehaveData, Age_At_PhenX_Completion>1)
no_phx = subset(BehaveData, is.na(Age_At_PhenX_Completion))
range(no_phx$Age,na.rm=TRUE)  # 3 21
range(phx$Age,na.rm=TRUE)  # 7.08 21.00
range(phx$Age_At_PhenX_Completion,na.rm=TRUE)  # 8.25 22.67

#sum(with(BehaveData, PHX_Alcohol_LifeUse_1==1,na.rm=TRUE)) ## broken, NA

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

alc = subset(BehaveData, !is.na(PHX_Alcohol_LifeUse_1) & !is.na(Age_At_PhenX_Completion))
younguns = subset(alc, Age < 14)
yng = younguns[c("Age","Age_At_PhenX_Completion")]
yng$phx_delay = yng$Age_At_PhenX_Completion - yng$Age
  
allp = phx[c("Age","Age_At_PhenX_Completion")]
allp = allp[complete.cases(allp)]
allp$phx_delay = allp$Age_At_PhenX_Completion - allp$Age

Druggies <- BehaveData %>%
  filter(  PHX_Alcohol_LifeUse_1==1 
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
)

DruggieID <- Druggies %>%
  select(  SubjID, 
           Alcohol = PHX_Alcohol_LifeUse_1,
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
           Heroin = PHX_Substance_Heroin_1
           )

filter(BehaveData, PHX_Substance_Sedtv_1==1) %>% select(SubjID)
filter(BehaveData, PHX_Substance_Meth_1==1) %>% select(SubjID)

#----PHX_Alcohol_LifeUse_1,
#"In your entire life, have you had at least 1 drink of any kind of alcohol, not counting small tastes or sips? (1=Yes, 2=No)"
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


## SES measures
inc = BehaveData$FDH_3_Household_Income

## inhibitory control measures, flanker task performance

## subset on those who've taken phenx

## substance abuse issues (phenx)

## do substance abuse stuff correlate with inhib?

## is it modulated by SES? (maybe as covariate or interaction)


