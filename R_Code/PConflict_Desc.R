library(car)
library(plm)
library(tidyverse)
library(dummies)
library(dplyr)
library(raster)
library(maptools)
library(ggplot2)
library(rgdal)
library(ggthemes)
library(ggalt)
library(ggpubr)
library(scales)
library(broom)
library(plm)
library(cartography)
library(rgeos)
library(psych)
library(ggplot2)
library(xtable)
library(huxtable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

slope.farc = function(x){
  y = 5.988 + 0.94*x
  return(y)}
slope.eln = function(x){
  y = 5.5104 + 0.577*x
  return(y)}

slope.eln.farc = function(x){
  y = 5.122588 + 0.972*x
  return(y)}

confidence_interval <- function(vector) {
  vec_sd <- sd(vector, na.rm = T)
  n <- length(!is.na(vector))
  vec_mean <- mean(vector, na.rm = T)
  error <- qt((.95 + 1)/2, df = n - 1) * vec_sd / sqrt(n)
  result <- c("mean" = vec_mean,"lower" = vec_mean - error, "upper" = vec_mean + error)
  return(result)
}

setwd("C:/Users/fc3/Box/PConflict2")

# Geog
my_spdf=readOGR( dsn= "C:/Users/fc3/Box/PConflict2/Municipality_shp" , layer="MGN_MPIO_POLITICO", verbose = F)
my_spdf@data$id = rownames(my_spdf@data)
spdf = fortify(my_spdf, region = "MPIO_CCDGO", id  = "MPIO_CCDGO")
spdf$id = gsub("(^|[^0-9])0+", "\\1", spdf$id, perl = TRUE)
CENTROIDS = gCentroid(my_spdf,byid=TRUE)
centroids= as.data.frame(coordinates(CENTROIDS))
centroids$id = my_spdf@data$MPIO_CCDGO
centroids$id = gsub("(^|[^0-9])0+", "\\1", centroids$id, perl = TRUE)

# DS
gr = read.csv("ttm_gr.csv")
gr = gr[which(gr$gr %in% 2:4),]
gr$id = as.character(gr$id)
light = read.csv("lights.csv")
did_df = read.csv("farc_eln.csv")
did_df = pdata.frame(did_df,index = c("id","time"))
map_df = did_df
map_df = map_df %>%
  dplyr::select(id,gr,time,light,incidents,coca) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(chg_vln = incidents - dplyr::lag(incidents,1)) %>%
  dplyr::mutate(chg_coca = coca - dplyr::lag(coca,1)) %>%
  dplyr::mutate(chg_light = light - dplyr::lag(light,1))
map_df = map_df[which(map_df$time != 2012),]
cerac = read.csv("CERAC - Tipología de los municipios de Colombia según el conflicto armado interno - Datos.csv")
cerac$Divipola = as.factor(cerac$Divipola)
map_df = left_join(map_df,cerac[,c("Divipola","Presencia")],by=c("id"="Divipola"))
demo = read.csv("demo.csv")
demo$id = as.character(demo$id)
demo = demo[!duplicated(demo[,c('id', 'Año')]),]
demo$Dato.Numérico = demo$Dato.Numérico/1000
gob = read.csv("gob.csv")
gob$id = as.character(gob$id)
gob$Income = gob$Income/1000
gob$Expenditure = gob$Expenditure/1000
serv = read.csv("serv.csv")
serv$id = as.character(serv$id)

####################################################### Descriptive statistics ###############################################################

descr = did_df
descr = Reduce(function(x, y) left_join(x, y, by = "id", all=F), 
               list(descr[which(descr$time == 2012),],
                    demo[which(demo$Año == 2012),c("id","Dato.Numérico")],
                    gob[which(gob$ano == 2012),c("id","Income","Expenditure")],
                    serv[which(serv$ano == 2012),c("id","water","garbage","sewer")]))

p.val = ""
vars = c("light","incidents","coca","Dato.Numérico","Income","Expenditure","water","garbage","sewer")
for (i in 1:length(vars)) {
  anv <- lm(as.formula(paste0(vars[i], "~ gr")), data = descr)
  aov = anova(anv)
  p.val[i] = aov[1,5]
}
p.val = round(as.numeric(p.val),3)

desc = describeBy(descr[,c("light","incidents","coca","Dato.Numérico","Income","Expenditure","water","garbage","sewer")], 
                  descr$gr,mat=TRUE, digits = 3)
desc$var = rownames(desc)
desc$var = gsub('[[:digit:]]+', '', desc$var)

desc = reshape(desc[,c("var","group1","mean")], 
               idvar = "var", 
               timevar = "group1", 
               direction = "wide")

desc = cbind(desc,p.val)

quick_docx(desc, file = "descr.docx")

############################################################# Box plot ######################################################################

df_bx = did_df
df_bx$coca[is.na(df_bx$coca)] = 0
df_bx[df_bx == 0] = 0.001
df_bx$gr = ifelse(df_bx$gr == 2, "ELN", 
                  ifelse(df_bx$gr == 3, "FARC", "FARC-ELN"))
df_bx = df_bx %>%
  group_by(id) %>%
  dplyr::mutate(chg_vln = (incidents - dplyr::lag(incidents,1))/dplyr::lag(incidents,1),
                chg_lgt = (light - dplyr::lag(light,1))/dplyr::lag(light,1),
                chg_coca = (coca - dplyr::lag(coca,1))/dplyr::lag(coca,1))
df_bx = as.data.frame(df_bx[which(df_bx$time == 2016),c("id","gr","chg_vln","chg_lgt","chg_coca")])
df_bx = reshape(df_bx, direction='long',
                varying=colnames(df_bx)[3:5],
                timevar='var',
                times=c("Violent incidents","Rural activity","Coca cultivation"),
                v.names="Change",
                idvar='id')
gph_bxp = ggplot(df_bx, aes(x=factor(gr), y=Change, group=factor(gr))) +
  geom_boxplot(aes(fill=factor(gr))) +
  facet_grid(. ~ var) +
  scale_fill_discrete(name = "Armed \ngroup")+
  ylim(c(-2,5))+
  theme(legend.position="right",
        legend.direction="vertical",
        legend.box = "horizontal",
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

ggsave("gph_bxp.png", width = 30, height = 20, units = "cm")

###################################################### Light time trend #####################################################################

light = read.csv("light.csv")
light$id = as.character(light$id)
light = Reduce(function(x, y) left_join(x, y, by = "id", all=F), list(gr[,c("id","gr")],light[,c("id","light_16","light_12")]))

light = light %>% 
  group_by(gr) %>%
  dplyr::summarise(light_16 = mean(light_16), light_12 = mean(light_12))

pr_lg = read.csv("light_pre.csv")
pr_lg$id = as.factor(pr_lg$id)
pr_lg = Reduce(function(x, y) left_join(x, y, by = "id", all=F), list(gr[,c("id","gr")],pr_lg))

pr_lg = reshape(pr_lg, direction='long', 
                varying=colnames(pr_lg)[3:8], 
                timevar='time',
                times=2008:2013,
                v.names="light",
                idvar='id')

pr_lg = pr_lg %>% 
  group_by(gr,time) %>%
  dplyr::summarise( light = mean(light))

xtr = data.frame(gr = rep(c(2,3,4),3),
                 time = c(rep(2014,3),rep(2015,3),rep(2016,3)),
                 light = c(slope.eln(2),slope.farc(2),slope.eln.farc(2),
                            slope.eln(3),slope.farc(3),slope.eln.farc(3),
                            pr_lg$light[which(pr_lg$gr == 2 & pr_lg$time == 2012)]*1.4,pr_lg$light[which(pr_lg$gr == 3 & pr_lg$time == 2012)]*1.6271,pr_lg$light[which(pr_lg$gr == 4 & pr_lg$time == 2012)]*1.759))

pr_lg = rbind(pr_lg,xtr)


light_trend = ggplot(data=pr_lg, aes(x=factor(time), y=light, group=factor(gr), linetype = factor(gr))) +
  geom_line(aes(color=factor(gr)), size = 1)+
  geom_point(aes(color=factor(gr)))+
  theme(legend.position="bottom")+
  ylab("Economic activity")+
  xlab("Year")+
  geom_vline(xintercept = 5, linetype="dashed") +
  scale_color_manual(name = "Armed \ngroup",
                     values=c("gray10", "gray40","gray70"),
                     labels = c("ELN","FARC","FARC-ELN")) +
  scale_linetype_manual(name = "Armed \ngroup",
                        values=c("dotted","dotdash","solid"),
                        labels = c("ELN","FARC","FARC-ELN")) +
  scale_x_discrete(breaks=c(2008,2010,2012,2014,2016), labels=c(2008,2010,2012,2014,2016))+
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.box = "horizontal",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

################################################## Violence time trend #####################################################################

rohv = read.csv("rohv.csv")
rohv = rohv[which(rohv$ANIO.OCURRENCIA %in% 2008:2016),]
rohv = rohv[-which(rohv$MUNICIPIO.OCURRENCIA=="Sin Informacion"),]
rohv = rohv %>%
  dplyr::group_by(ANIO.OCURRENCIA,DANE.OCURRENCIA) %>%
  dplyr::summarise(incidents = n(), victims = sum(TOTAL))
colnames(rohv) = c("time","id","incidents","victims")
rohv = Reduce(function(x, y) left_join(x, y, by = "id", all=F), list(gr[,c("id","gr")],rohv))

rohv = rohv %>% 
  group_by(gr,time) %>%
  dplyr::summarise(incidents = mean(incidents), victims = mean(victims))

gph_vln0 = ggplot(data=rohv, aes(x=time, y=incidents, group=factor(gr), linetype = factor(gr))) +
  geom_line(aes(color=factor(gr)), size = 1)+
  geom_point(aes(color=factor(gr)))+
  geom_vline(xintercept = 2012, linetype = "dashed", colour = "black")+
  scale_color_manual(name = "Armed \ngroup",
                     values=c("gray10", "gray40","gray70"),
                     labels = c("ELN","FARC","FARC-ELN")) +
  scale_linetype_manual(name = "Armed \ngroup",
                        values=c("dotted","dotdash","solid"),
                        labels = c("ELN","FARC","FARC-ELN")) +
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

####################################################### Illicit crops time trend ###########################################################

coca = read.csv("coca.csv",stringsAsFactors = FALSE)
coca = as.data.frame(sapply(coca, as.numeric))
colnames(coca)[1] = "id"
coca$id = as.character(coca$id)
coca = Reduce(function(x, y) left_join(x, y, by = "id", all=F), list(gr[,c("id","gr")],coca))

coca = reshape(coca, direction='long', 
                varying=colnames(coca)[3:11], 
                timevar='time',
                times=2008:2016,
                v.names="hectares",
                idvar='id')

coca = coca %>% 
  group_by(gr,time) %>%
  dplyr::summarise(coca = mean(hectares, na.rm = T))

gph_coca = ggplot(data=coca, aes(x=time, y=coca, group=factor(gr), linetype = factor(gr))) +
  geom_line(aes(color=factor(gr)), size = 1)+
  geom_point(aes(color=factor(gr)))+
  geom_vline(xintercept = 2012, linetype = "dashed", colour = "black")+ 
  scale_color_manual(name = "Armed \ngroup",
                     values=c("gray10", "gray40","gray70"),
                     labels = c("ELN","FARC","FARC-ELN")) +
  scale_linetype_manual(name = "Armed \ngroup",
                        values=c("dotted","dotdash","solid"),
                        labels = c("ELN","FARC","FARC-ELN")) +
  guides(linetype=FALSE,color=FALSE)+
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray70"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "gray70"))

ggline = ggpubr::ggarrange(light_trend,gph_vln0,gph_coca,
                           common.legend = T,
                           legend = "bottom",
                           nrow = 1, ncol = 3,
                           labels = c("A","B","C"),
                           align = "h")

ggsave("ggline.png", width = 40, height = 20, units = "cm")

############################################################ Treatment map #################################################################

tmt = spdf
tmt_sp = map_df
tmt_sp = Reduce(function(x, y) left_join(x, y, by = "id", all=TRUE), list(tmt,tmt_sp[,c("id","gr")]))
tmt_sp$gr = ifelse(tmt_sp$gr == "3","FARC",ifelse(tmt_sp$gr == "2","ELN","FARC-ELN"))

# plot

ggtmt = ggplot() + 
  geom_polygon(data = tmt_sp,
               aes(long, lat, group = group, fill = factor(gr)),
               color="#2b2b2b", size=0.15) + 
  scale_fill_manual(name = "Armed \ngroup",
                    values = c("firebrick3", "cyan3","darkolivegreen3","white"),
                    labels = c("ELN","FARC","FARC-ELN","None")) + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), legend.position="left") + 
  labs(color="Treatment map with intensity of violence")+
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.box = "horizontal",
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"))

ggsave("ggtmt.png", width = 40, height = 40, units = "cm")

################################################## Changes in night lights map #############################################################

tmt = spdf
light = read.csv("light.csv")
light$chg_light = light$light_16 - light$light_12
tmt_sp = light[,c("id","chg_light")]
tmt_sp$id = as.character(tmt_sp$id)
tmt_sp = Reduce(function(x, y) left_join(x, y, by = "id", all=TRUE), list(tmt,tmt_sp))
tmt_sp$brks = cut(tmt_sp$chg_light,
                  breaks=c(-31876,-577,427,3366,95839),
                  labels=c("[-31876, -577)", "[-577, 427)", "[427,3366)",
                           "[3366, 95839]"))
#plot
gglg = ggplot() + 
  geom_polygon(data = tmt_sp,
               aes(long, lat, group = group, fill = brks),
               color="#2b2b2b", size=0.15) + 
  scale_fill_manual(name = "Change in \neconomic activity",
                    values = c("darkslateblue","deeppink","goldenrod3","gold","white"))+
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.box = "horizontal",
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"))

##################################################### Change in illicit crops map ###########################################################

tmt = spdf
coca = read.csv("coca.csv")
coca[is.na(coca)] = 0
coca$chg_coca = coca$X2016 - coca$X2012
tmt_sp = coca[,c("id","chg_coca")]
tmt_sp$id = as.character(tmt_sp$id)
tmt_sp = Reduce(function(x, y) left_join(x, y, by = "id", all=TRUE), list(tmt,tmt_sp))
tmt_sp$brks = cut(tmt_sp$chg_coca,
                  breaks=c(-537.15,-6.00,0.00,326.29,18082.95),
                  labels=c("[-537.16, -6.00)", "[-6.00, 0)", "[0,326.29)",
                           "[326.29, 18082.96]"))

## plot
ggcoca = ggplot() + 
  geom_polygon(data = tmt_sp,
               aes(long, lat, group = group, fill = brks),
               color="#2b2b2b", size=0.15) + 
  scale_fill_manual(name = "Change in number \nof hectares \nplanted in coca",
                    values = c("darkslateblue","deeppink","goldenrod3","gold","white")) +
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.box = "horizontal",
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"))

################################################## Change in violence map ###################################################################

tmt = spdf
rohv = read.csv("rohv.csv")
rohv = rohv[which(rohv$ANIO.OCURRENCIA %in% c(2012,2016)),]
rohv = rohv[-which(rohv$MUNICIPIO.OCURRENCIA=="Sin Informacion"),]
rohv = rohv %>%
  dplyr::group_by(ANIO.OCURRENCIA,DANE.OCURRENCIA) %>%
  dplyr::summarise(incidents = n(), victims = sum(TOTAL)) %>%
  dplyr::mutate(chg_vln = incidents - dplyr::lag(incidents,1))
colnames(rohv) = c("time","id","incidents","victims","chg_vln")
rohv = rohv[which(rohv$time != 2012),]
rohv$id = as.character(rohv$id)
rohv = Reduce(function(x, y) left_join(x, y, by = "id", all=T), list(tmt,rohv[,c(2,5)]))
rohv$brks = cut(rohv$chg_vln, 
                  breaks=c(-189,-11,1,22,196), 
                  labels=c("[-189.1, -11)", 
                           "[-11, 1)",
                           "[1, 22)",
                           "[22, 196]"))

ggvln = ggplot() + 
  geom_polygon(data = rohv,
               aes(long, lat, group = group, fill = brks),
               color="#2b2b2b", size=0.15) + 
  scale_fill_manual(name = "Change in \nnumber of \nviolent incidents",
                    values = c("black","darkslateblue","deeppink","gold","white")) + 
  theme(legend.position="bottom",
        legend.direction="vertical",
        legend.box = "horizontal",
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        panel.background = element_rect(fill = "#FFFFFF", colour = "black",size = 1, linetype = "solid"))

ggmap2 = ggpubr::ggarrange(ggtmt,gglg,ggvln,ggcoca,
                           common.legend = F,
                           legend = "bottom",
                           nrow = 2, ncol = 2,
                           labels = c("A","B","C","D"),
                           align = "h")

ggsave("ggmap.png", width = 40, height = 60, units = "cm")

 ggmap2 = ggpubr::ggarrange(gglg,ggvln,ggcoca,
                           common.legend = F,
                           legend = "bottom",
                           nrow = 1, ncol = 3,
                           labels = c("A","B","C"),
                           align = "h")

ggsave("ggpres.png", width = 70, height = 40, units = "cm")
