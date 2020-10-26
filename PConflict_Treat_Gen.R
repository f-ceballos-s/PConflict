setwd("C:/Users/fc3/Box/PConflict2")

library(dplyr)
library(tidyverse)
library(raster)
library(ggplot2)
library(rgdal)
library(maptools)

# Treatment data sets

arm_gr = read.csv("arm_gr.csv")
arm_gr[is.na(arm_gr)] = 0
arm_gr$gr = ifelse(arm_gr$FARC != 0 & arm_gr$ELN == 0 & arm_gr$AUC == 0, 3,
                   ifelse(arm_gr$FARC == 0 & arm_gr$ELN != 0 & arm_gr$AUC == 0, 2,
                          ifelse(arm_gr$FARC ==0 & arm_gr$ELN == 0 & arm_gr$AUC != 0, 4,
                                 ifelse(arm_gr$FARC != 0 & arm_gr$ELN != 0 & arm_gr$AUC == 0, 5,
                                        ifelse(arm_gr$FARC !=0 & arm_gr$ELN == 0 & arm_gr$AUC != 0, 6,
                                               ifelse(arm_gr$FARC ==0 & arm_gr$ELN != 0 & arm_gr$AUC != 0, 7, 
                                                      ifelse(arm_gr$FARC !=0 & arm_gr$ELN != 0 & arm_gr$AUC != 0, 8, 1)))))))

arm_gr$gr = ifelse(arm_gr$FARC != 0 & arm_gr$ELN == 0, 3,
                   ifelse(arm_gr$FARC == 0 & arm_gr$ELN != 0, 2,
                          ifelse(arm_gr$FARC != 0 & arm_gr$ELN != 0, 4, 1)))
# colnames(arm_gr)[1] = "id"

write.csv(arm_gr,"ttm_gr.csv",row.names = F)
