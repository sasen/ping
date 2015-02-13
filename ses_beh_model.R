library(dplyr)
library(ggplot2)
library(magrittr)

BehaveData <- read.csv("Data/PING_Behavior.csv",stringsAsFactors=FALSE)

## SES measures
inc = BehaveData$FDH_3_Household_Income

## inhibitory control measures, flanker task performance

## subset on those who've taken phenx

## substance abuse issues (phenx)

## do substance abuse stuff correlate with inhib?

## is it modulated by SES? (maybe as covariate or interaction)


