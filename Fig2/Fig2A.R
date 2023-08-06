library(ggplot2)
library("readxl")
library(tidyverse)
library(dplyr)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library(ggalluvial)
library("lubridate")

library(ggtree)
library(tidytree)
#library(ape)
library(treeio)


tree<-read.newick('timetree_v2.nwk')
#tree<-read.newick('tree.nwk')

metadata_df <- read_excel("metadata.xlsx")


custom3<-c('antiquewhite2',"tan4",'peachpuff3','palegreen3','dodgerblue1','hotpink2','mediumorchid3','blue3','skyblue1','purple4',
           'mediumpurple','darkseagreen4','cadetblue3','thistle3','deeppink3',
           #custom2<-c("darkolivegreen",'darkseagreen3','darkseagreen2','antiquewhite2','peachpuff3','tan4',
           'red3','grey30','darkolivegreen','slategray1','plum2',
           'white','white')


tree <- groupClade(tree,.node=c(14074,14670,15293,18694,22129))

p<-ggtree(tree,mrsd="2021-12-15", as.Date=TRUE,aes(color=group),size=0.15) + theme_tree2()+
  scale_x_date(date_labels = "%Y",date_breaks = "year")+
  #expand_limits(y = 21000)+
  theme(axis.text.x = element_text(size=10))

p

panelA <- p %<+% metadata_df + 
  geom_tippoint(aes(
    subset=(region=='Africa'), fill=group, color=group),size=1.5,align=F,stroke=0.2, shape=21)+
  #geom_tippoint(aes(
  # subset=(pangolin_lineage=='C.1.2'), fill=group, color=group),size=1.5,align=F,stroke=0.05, shape=21)+
  #geom_tippoint(aes(
  #  subset=(pangolin_lineage=='B.1.1.7' | Nextstrain_clade == '20I (Alpha, V1)'), fill=group, color=group),size=1.5,align=F,stroke=0.05, shape=21)+
  #geom_tippoint(aes(
  #  subset=(pangolin_lineage=='B.1.351' | Nextstrain_clade == '20H (Beta, V2)'), fill=group, color=group),size=1.5,align=F,stroke=0.05, shape=21)+
  #geom_tippoint(aes(
  #  subset=(pangolin_lineage=='B.1.617.2' | Nextstrain_clade == '21I (Delta)' | Nextstrain_clade == '21J (Delta)' | Nextstrain_clade == '21A (Delta)'), fill=group, color=group),size=1.5,align=F,stroke=0.05, shape=21)+
  #geom_tippoint(aes(
  #  subset=(pangolin_lineage=='B.1.1.529' | Nextstrain_clade == '21K (Omicron)'), fill=group, color=group),size=1.5,align=F,stroke=0.05, shape=21)+
  scale_color_manual(values=c('grey70','bisque2','deeppink2','grey40','dodgerblue2','mediumseagreen', 'black'))+
  scale_fill_manual(values=c('grey60','bisque3','deeppink3','grey20','dodgerblue3','seagreen4','black'))+
  
  #scale_color_manual(values=c('grey70','deeppink2','grey40','bisque2','mediumseagreen', 'black'))+
#scale_fill_manual(values=c('grey60','deeppink3','grey20','bisque3','seagreen4','black'))+
  theme(axis.text.x = element_text(size=8, hjust = 1,vjust=0.5,angle=90))
  #geom_tiplab(aes(color=group))
  #geom_text(aes(label=node), hjust=-.3, size=2)


panelA

ggsave('tree_labelled_11Dec_large.pdf', width = 30, height = 3000, units = "cm",limitsize = FALSE)
#scale_fill_manual(values=c('mediumseagreen','bisque2','grey40','dodgerblue3','deeppink2','grey90'), name='Variants', labels=c('Delta (B.1.617.2/AY.x)','Beta (B.1.351)','Alpha (B.1.1.7)','C.1.2','Omicron (B.1.1.529)','Other Lineages'))+

