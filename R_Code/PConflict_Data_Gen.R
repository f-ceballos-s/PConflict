setwd("C:/Users/fc3/Box/PConflict2")

library(MatchIt)
library(stringdist)
library(dplyr)
library(foreach)
library(raster)
library(maptools)
library(ggplot2)
library(rgdal)
library(cartography)
library(psych)
library(twang)
library(tidyverse)
library(raster)
library(stringi)

yrs = c(2002:2010,2012)

ClosestMatch2 = function(string, stringVector){
  stringVector[amatch(string, stringVector, maxDist=Inf)]
}

trim.leading <- function (x)  sub("^\\s+", "", x)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

join.sp.df <- function(x, y, xcol, ycol) {
  x$sort_id <- 1:nrow(as(x, "data.frame"))  
  x.dat <- as(x, "data.frame")  
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  
  x2.dat <- as(x2, "data.frame") 
  row.names(x.dat2.ord) <- row.names(x2.dat)  
  x2@data <- x.dat2.ord  
  return(x2)
}

baseR.sbst.rssgn   = function(x) { x[is.na(x)] <- 0; x }

my.mean = function(x){mean(x,na.rm=T)}

## Violence
rohv = read.csv("rohv.csv")
rohv = rohv[which(rohv$ANIO.OCURRENCIA %in% 2008:2018),]
rohv = rohv[-which(rohv$MUNICIPIO.OCURRENCIA=="Sin Informacion"),]
rohv = rohv %>%
  group_by(ANIO.OCURRENCIA,DANE.OCURRENCIA) %>%
  dplyr::summarise(incidents = n(), victims = sum(TOTAL))
colnames(rohv) = c("time","id","incidents","victims")
rohv[,c("id","time")] = apply(rohv[,c("id","time")],2,as.character)

## Light
light = read.csv("lights.csv",strip.white=TRUE)
light = reshape(light[,c("id","X2012","X2016")], 
                direction='long', 
                varying=c("X2012","X2016"), 
                timevar='time',
                times=c('2012', '2016'),
                v.names="light",
                idvar='id')
light[,c("id","time")] = apply(light[,c("id","time")],2,as.character)
light$id = trim.leading(light$id)

## Coca
coca = read.csv("coca.csv")
coca = coca[!duplicated(coca$id),]
coca = reshape(coca, 
               direction='long', 
               varying=colnames(coca)[-1], 
               timevar='time',
               times=2008:2018,
               v.names="coca",
               idvar='id')
coca$id = as.character(coca$id)
coca$time = as.character(coca$time)
coca$coca[is.na(coca$coca)] = 0

## Treatment
pl_gr = read.csv("ttm_gr.csv")
pl_gr$id = as.character(pl_gr$id)
pl_gr$gr = as.factor(pl_gr$gr)
# a           <- as.data.frame(model.matrix(~pl_gr$gr-1))
# colnames(a) <- (substring( names(a), 7, 9))
# pl_gr <- cbind(pl_gr, a)

##################################
####### Light data set ###########
##################################

# Merge

rohv_pl = left_join(coca,rohv,by=c("id","time"), all=T)
rohv_pl = left_join(rohv_pl,light,by=c("id","time"),all=T)
rohv_pl = left_join(rohv_pl,pl_gr,by="id")

# Generate datasets

## FARC-ELN Data set

write.csv(rohv_pl,"farc_eln.csv",row.names = F)

# ## ELN-BCO Data set
# 
# ds = rohv_pl[which(rohv_pl$gr==3|rohv_pl$gr==4),]
# 
# write.csv(ds,"bco_eln.csv",row.names = F)

## All comprehensive data set

write.csv(rohv_pl,"multi.csv",row.names = F)

