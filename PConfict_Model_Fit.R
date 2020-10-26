library(lmtest)
library(car)
library(tidyverse)
library(plm)
library(magrittr)
library(Jmisc)
library(stargazer)
library(knitr)
library(AER)
library(ivpack)
library(kableExtra)
library(huxtable)
library(jtools)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

pd <- position_dodge(width = 0.4)

##############################
##Read DiD Df#################
##############################

setwd("C:/Users/fc3/Box/PConflict2")
did_df = read.csv("multi.csv")
did_df$time = as.numeric(did_df$time)
did_df = did_df[which(did_df$gr %in% 2:4),]
did_df = pdata.frame(did_df, index = c("id", "time"))
did_df$dpto = gsub('.{3}$', '', did_df$id)

did_df$light = did_df$light/1000
did_df$coca = did_df$coca/100
did_df = did_df[which(did_df$gr != 0),]

summary(did_df)

###############################
##Differences-in-differences###
###############################

# DiD - discrete factor(gr)ment

reg <- plm(light~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = did_df[which(did_df$time %in% c(2012,2016)),], effect = "individual",
           index = c("id", "time"), 
           model = "within")
vd1 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
reg <- plm(incidents~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = did_df[which(did_df$time %in% c(2012,2016)),], effect = "individual",
           index = c("id", "time"), 
           model = "within")
vd2 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
reg <- plm(coca~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = did_df[which(did_df$time %in% c(2012,2016)),], effect = "individual",
           index = c("id", "time"), 
           model = "within")
vd3 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))

# Coefficient trends

did_df = within(did_df, time <- relevel(time, ref = 5))
y2012 = data.frame(lower = rep(0,3),
                   upper = rep(0,3),
                   mean = rep(0,3),
                   year = rep(2012,3),
                   Group = c("ELN","FARC","FARC-ELN"))

## Violence

reg <- plm(incidents~factor(time) + factor(gr) + factor(time):factor(gr) + factor(dpto):factor(time), data = did_df, effect = "individual",
           index = c("id", "time"), 
           model = "within")
vt2 = coefci(reg,level = .95, vcov=vcovHC(reg,type="HC0",cluster="group"))

vt2 = as.data.frame(vt2[1:30,])
colnames(vt2) = c("lower","upper")
vt2$mean = (vt2$lower+vt2$upper)/2
vt2$year = rep(c(2008:2011,2013:2018),3)
vt2$Group = c(rep("ELN",10),rep("FARC",10),rep("FARC-ELN",10))

vt2 = rbind(vt2,y2012)

bar_vln = ggplot(vt2, aes(x=year, y=mean, group=Group, color=Group, shape = Group)) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15, position = pd) +
  geom_point(aes(x = year, y = mean), position = pd)+
  xlab("Year") +
  ylab("Slope coefficient (??)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

ggsave("gph_slopev.png", width = 30, height = 20, units = "cm")

## Coca cultivation

reg <- plm(coca~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = did_df, effect = "individual",
           index = c("id", "time"), 
           model = "within")
vt3 = coefci(reg,level = .95, vcov=vcovHC(reg,type="HC0",cluster="group"))

vt3 = as.data.frame(vt3[1:30,])
colnames(vt3) = c("lower","upper")
vt3$mean = (vt3$lower+vt3$upper)/2
vt3$year = rep(c(2008:2011,2013:2018),3)
vt3$Group = c(rep("ELN",10),rep("FARC",10),rep("FARC-ELN",10))

vt3 = rbind(vt3,y2012)

bar_coca = ggplot(vt3, aes(x=year, y=mean, group=Group, color=Group, shape = Group)) +
  geom_line(position = pd) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.15, position = pd) +
  geom_point(aes(x = year, y = mean), position = pd)+
  xlab("Year") +
  ylab("Slope coefficient (??)")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

ggsave("gph_slopec.png", width = 30, height = 20, units = "cm")


#########################
##### DiD - Placebo #####
#########################

## Fake treatment

pl_df = read.csv("bco_eln.csv")
pl_df$dpto = gsub('.{3}$', '', pl_df$id)
pl_df[is.na(pl_df)] = 0
pl_df$light = pl_df$light/1000

reg <- plm(light~time + factor(gr) + time:factor(gr)+ factor(dpto):time, data = pl_df, effect = "individual",
           index = c("id", "time"), 
           model = "within")
vp1 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
reg <- plm(victims~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = pl_df, effect = "individual",
           index = c("id", "time"), 
           model = "within")
vp2 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
reg <- plm(coca~time + factor(gr) + time:factor(gr) + factor(dpto):time, data = pl_df, effect = "individual",
           index = c("id", "time"), 
           model = "within")
vp3 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))

## Theft

pl_tf = read.csv("tf_pl.csv")
pl_tf$dpto = gsub('.{3}$', '', pl_tf$id)
pl_tf$id = as.character(pl_tf$id)

reg <- lm(thefts~time + factor(gr) + time:factor(gr)+ factor(dpto):time, data = pl_tf)
vp4 = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))

# Prof. Crost Triple DiD

## Aggregate violence

did_df = did_df %>% 
  group_by(id) %>%
  dplyr::mutate(
    incidents_bl = dplyr::first(incidents),
    coca_bl = dplyr::first(coca),
    light_bl = dplyr::first(light)
  )

## Aggregate violence

vh1 = list()
vln = c("light_bl","incidents_bl","coca_bl")
vln2 = c("light","incidents","coca")
for (i in 1:length(vln)) {
  reg = plm(as.formula(paste0(vln2[i], "~factor(gr)+time+time:factor(gr)+",vln[i],"+ factor(gr):",vln[i],"+time:",vln[i],"+time:factor(gr):",vln[i],"+ factor(dpto):time")), 
            index = c("id","time"), data = did_df, effect = "individual",model = "within")
  vh1[[i]] = coeftest(reg, vcov=vcovHC(reg,type="HC2",cluster="group"))
}

# Rural activity

vv1 = list()
for (i in 1:length(vln)) {
  reg = plm(as.formula(paste0("light~factor(gr)+time+time:factor(gr)+",vln[i],"+ factor(gr):",vln[i],"+time:",vln[i],"+time:factor(gr):",vln[i],"+ factor(dpto):time")), 
            index = c("id","time"), data = did_df, effect = "individual",model = "within")
  vv1[[i]] = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
}

# Intensity of Conflict CERAC

cerac = read.csv("CERAC - Tipología de los municipios de Colombia según el conflicto armado interno - Datos.csv")
colnames(cerac)[1] = "id"

did_df = merge(did_df,cerac[,c(1,4)],by="id",all=F)
did_df$pr = ifelse(did_df$Presencia == "Conflicto permanente",1,0)
did_df = pdata.frame(did_df, index = c("id", "time"))

pr = list()
vln2 = c("light","incidents","coca")
for (i in 1:length(vln2)) {
  reg = plm(as.formula(paste0(vln2[i], "~factor(gr)+ time + time:factor(gr) + pr + factor(gr):pr + time:factor(gr):pr + factor(dpto):time")), 
            data = did_df, effect = "individual",model = "within")
  pr[[i]] = coeftest(reg, vcov=vcovHC(reg,type="HC0",cluster="group"))
}

# Tables

export_summs(vd2, vh1[[2]],pr[[2]],
             coefs = c("time" = "time2016",
                       "timexFARC" = "time2016:factor(gr)3",
                       "timexFARC/ELN" = "time2016:factor(gr)4",
                       "timexFARC" = "factor(gr)3:time2016",
                       "timexFARC/ELN" = "factor(gr)4:time2016",
                       "time x Baseline Violence " = "time2016:incidents_bl",
                       "FARC x time x Baseline Violence" = "factor(gr)3:time2016:incidents_bl",
                       "FARC/ELN x time x Baseline Violence" = "factor(gr)4:time2016:incidents_bl",
                       "time x Presence 2000-2012" = "time2016:pr",
                       "FARC x time x Presence 2000-2012" = "factor(gr)3:time2016:pr",
                       "FARC/ELN x time x Presence 2000-2012" = "factor(gr)4:time2016:pr"),
             # model.names = c("Violence"),
             statistics = c("nobs","r.squared"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1 ),
             caption = "Table 2. Difference-in-differences",
             to.file = "docx",
             file.name = "eq1.docx"
)

export_summs(vp1,vp2,vp3,
             scale = TRUE,
             coefs = c("treat" = "time2016",
                       "treatxFARC" = "time2016:factor(gr)4"),
             # model.names = c("Rural activity","Violence","Coca cultivation"),
             statistics = c("nobs","r.squared"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.05),
             to.file = "docx",
             file.name = "eq1p.docx"
)

export_summs(vd3,vh1[[3]],pr[[3]], 
             coefs = c("time" = "time2016",
                       "timexFARC" = "time2016:factor(gr)3",
                       "timexFARC/ELN" = "time2016:factor(gr)4",
                       "timexFARC" = "factor(gr)3:time2016",
                       "timexFARC/ELN" = "factor(gr)4:time2016",
                       "time x Baseline Violence " = "time2016:coca_bl",
                       "FARC x time x Baseline coca cultivation" = "factor(gr)3:time2016:coca_bl",
                       "FARC/ELN x time x Baseline coca cultivation" = "factor(gr)4:time2016:coca_bl",
                       "time x Presence 2000-2012" = "time2016:pr",
                       "FARC x time x Presence 2000-2012" = "factor(gr)3:time2016:pr"),
             # model.names = c("Coca cultivation","Coca cultivation ","Coca cultivation  "),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             caption = "Table 5. Triple difference - changes in rural activity, violent incidents and coca cultivation with respect to rebel group presence between 2000-2012",
             to.file = "docx",
             file.name = "eq2.docx"
)

export_summs(vd1,vv1[[1]],vv1[[2]],vv1[[3]],pr[[1]],
             coefs = c("time" = "time2016",
                       "timexFARC" = "time2016:factor(gr)3",
                       "timexFARC/ELN" = "time2016:factor(gr)4",
                       "timexFARC" = "factor(gr)3:time2016",
                       "timexFARC/ELN" = "factor(gr)4:time2016",
                       "time x Baseline rural activity " = "time2016:light_bl",
                       "FARC x time x Baseline rural activity" = "factor(gr)3:time2016:light_bl",
                       "FARC/ELN x time x Baseline rural activity" = "factor(gr)4:time2016:light_bl",
                       "time x Baseline Violence " = "time2016:incidents_bl",
                       "FARC x time x Baseline Violence" = "factor(gr)3:time2016:incidents_bl",
                       "FARC-ELN x time x Baseline Violence" = "factor(gr)4:time2016:incidents_bl",
                       "time x Baseline Violence " = "time2016:coca_bl",
                       "FARC x time x Baseline coca cultivation" = "factor(gr)3:time2016:coca_bl",
                       "FARC-ELN x time x Baseline coca cultivation" = "factor(gr)4:time2016:coca_bl",
                       "time x Presence 2000-2012" = "time2016:pr",
                       "FARC x time x Presence 2000-2012" = "factor(gr)3:time2016:pr",
                       "FARC-ELN x time x Presence 2000-2012" = "factor(gr)4:time2016:pr"),
             # model.names = c("Rural activity"),
             statistics = c("nobs","r.squared"),
             stars = c(`***` = 0.001, `**` = 0.01, `*` = 0.102062),
             caption = "Table 3. Triple difference - changes in violence and coca cultivation with respect to baseline values",
             to.file = "docx",
             file.name = "eq3.docx"
)
