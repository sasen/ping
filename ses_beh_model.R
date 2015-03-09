library(dplyr)
library(ggplot2)
library(magrittr)

BehaveData <- read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE)

phx = subset(BehaveData, Age_At_PhenX_Completion>1)
no_phx = subset(BehaveData, is.na(Age_At_PhenX_Completion))
range(no_phx$Age,na.rm=TRUE)  # 3 21
range(phx$Age,na.rm=TRUE)  # 7.08 21.00
range(phx$Age_At_PhenX_Completion,na.rm=TRUE)  # 8.25 22.67



## SES measures
inc = BehaveData$FDH_3_Household_Income

## inhibitory control measures, flanker task performance

## subset on those who've taken phenx

## substance abuse issues (phenx)

## do substance abuse stuff correlate with inhib?

## is it modulated by SES? (maybe as covariate or interaction)


