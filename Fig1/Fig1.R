
library(ggplot2)
library(ape)
library(repr)
library("readxl")
library('gridExtra')
#library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(cowplot)
library(ggthemes)
library(viridis)
library(ggrepel)
library("ggsci")
library(ggalt)
library("Hmisc")
library("scales")
library(ggpattern)

require(tidyverse)
library("readxl")
library("lubridate")
require("ggalt")
library("wesanderson")



#################################################
###### READING ALL DATA  ########################
#################################################
data2<-read_excel('Updated_SA_all data_10Dec21.xlsx')
data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)","Delta",'C.1.2', '21K (Omicron)'))

data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)","Delta",'C.1.2','21K (Omicron)'))
data2$division<-factor(data2$division,levels = c("Mpumalanga","North West","Northern Cape","Free State","Eastern Cape","Limpopo","Western Cape","Gauteng","KwaZulu-Natal"))

data2$days<-as.Date(cut(data2$date,breaks = "day",start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date,breaks = "week",start.on.monday = FALSE))
data2$date2<-as.Date(cut(data2$date,breaks = "2 week",start.on.monday = FALSE))
data2$date4<-as.Date(cut(data2$date,breaks = "1 month",start.on.monday = FALSE))

#data2<- data2 %>% filter(Nextstrain_variants!="Other lineages")
data2<- data2 %>% filter(division!="South Africa")
#data2<- subset(data2, !is.na(lineage_cluster))
##this will force the lineages to come first on the y axis =)
#data2$lineage_cluster[which(data2$lineage_cluster==unique(data2$lineage_cluster)[1])]<- paste(" ",unique(data2$lineage_cluster)[1]," ")
#data2$lineage_cluster[which(data2$lineage_cluster==unique(data2$lineage_cluster)[2])]<- paste(" ",unique(data2$lineage_cluster)[2]," ")
#data2$lineage_cluster[which(data2$lineage_cluster==unique(data2$lineage_cluster)[3])]<- paste(" ",unique(data2$lineage_cluster)[3]," ")



EC_df<-subset(data2,division=='Eastern Cape')
KZN_df<-subset(data2,division=='KwaZulu-Natal')
WC_df<-subset(data2,division=='Western Cape')
#CPT_df<-subset(metadata_df, location=='Cape Town Metro')
GP_df<-subset(data2, division=='Gauteng')
FS_df<-subset(data2, division=='Free State')
LP_df<-subset(data2, division=='Limpopo')
MP_df<-subset(data2, division=='Mpumalanga')
NC_df<-subset(data2, division=='Northern Cape')
NW_df<-subset(data2, division=='North West')




library (readr)

urlfile="https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"

mydata<-read_csv(url(urlfile))
provincial_cases<-mydata
#provincial_cases<-read_excel('SA_cases_provincial_25Nov.xlsx')
provincial_cases$days<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "day",
                                   start.on.monday = FALSE))

provincial_cases$date<-as.Date(cut(as.Date(provincial_cases$date,format='%d-%m-%Y'),
                                   breaks = "week",
                                   start.on.monday = FALSE))

provincial_cases <- within(provincial_cases,GP_daily <- ave(GP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,KZN_daily <- ave(KZN,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,WC_daily <- ave(WC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,EC_daily <- ave(EC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,LP_daily <- ave(LP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,MP_daily <- ave(MP,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NC_daily <- ave(NC,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,NW_daily <- ave(NW,FUN = function(x) c(x[1],diff(x))))
provincial_cases <- within(provincial_cases,FS_daily <- ave(FS,FUN = function(x) c(x[1],diff(x))))

provincial_cases <- within(provincial_cases,total_daily <- ave(total,FUN = function(x) c(x[1],diff(x))))


###Fixing artefact in case numbers for 23rd Nov 2021
provincial_cases[provincial_cases$days=="2021-11-23", "GP_daily"] <- 1823
provincial_cases[provincial_cases$days=="2021-11-23", "EC_daily"] <- 22
provincial_cases[provincial_cases$days=="2021-11-23", "FS_daily"] <- 24
provincial_cases[provincial_cases$days=="2021-11-23", "KZN_daily"] <- 62
provincial_cases[provincial_cases$days=="2021-11-23", "LP_daily"] <- 44
provincial_cases[provincial_cases$days=="2021-11-23", "MP_daily"] <- 57
provincial_cases[provincial_cases$days=="2021-11-23", "NW_daily"] <- 98
provincial_cases[provincial_cases$days=="2021-11-23", "NC_daily"] <- 26
provincial_cases[provincial_cases$days=="2021-11-23", "WC_daily"] <- 74
provincial_cases[provincial_cases$days=="2021-11-23", "total_daily"] <- 2230


provincial_cases[provincial_cases$days=="2021-12-12", "total_daily"] <- 15000


library(tidyverse)
library(zoo)

rollspan <- 7 # span of rolling average, in days.

provincial_cases <- provincial_cases %>% 
  dplyr::mutate(GP_daily_7day = zoo::rollmean(GP_daily, k = 7, fill = NA),
                KZN_daily_7day = zoo::rollmean(KZN_daily, k = 7, fill = NA),
                WC_daily_7day = zoo::rollmean(WC_daily, k = 7, fill = NA),
                EC_daily_7day = zoo::rollmean(EC_daily, k = 7, fill = NA),
                LP_daily_7day = zoo::rollmean(LP_daily, k = 7, fill = NA),
                MP_daily_7day = zoo::rollmean(MP_daily, k = 7, fill = NA),
                NC_daily_7day = zoo::rollmean(NC_daily, k = 7, fill = NA),
                total_daily_7day = zoo::rollmean(total_daily, k = 7, fill = NA)
                
  ) %>% 
  dplyr::ungroup()



urlfile2="https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_timeline_testing.csv"

mydata2<-read_csv(url(urlfile2))
testing_data<-mydata2

testing_data$days<-as.Date(cut(as.Date(testing_data$date,format='%d-%m-%Y'),
                                   breaks = "day",
                                   start.on.monday = FALSE))

testing_data$date<-as.Date(cut(as.Date(testing_data$date,format='%d-%m-%Y'),
                                   breaks = "week",
                                   start.on.monday = FALSE))
testing_data$date4<-as.Date(cut(as.Date(testing_data$date,format='%d-%m-%Y'),
                               breaks = "4 weeks",
                               start.on.monday = FALSE))

testing_data <- within(testing_data,daily_tests <- ave(cumulative_tests,FUN = function(x) c(x[1],diff(x))))
testing_data <- within(testing_data,daily_deaths <- ave(deaths,FUN = function(x) c(x[1],diff(x))))



urlfile3="https://raw.githubusercontent.com/covid-19-Re/dailyRe-Data/master/ZAF-estimates.csv"

mydata3<-read_csv(url(urlfile3))
R_estimates<-mydata3

R_estimates$date<-as.Date(cut(R_estimates$date,
                              breaks = "day",
                              start.on.monday = FALSE))

R_estimates$date2<-as.Date(cut(R_estimates$date,
                              breaks = "week",
                              start.on.monday = FALSE))

R_estimates=subset(subset(subset(R_estimates, region=='ZAF'),data_type=="Deaths"),estimate_type=="Cori_slidingWindow")
R_estimates=subset(R_estimates, date>as.Date("2020-04-01"))
R_estimates=subset(R_estimates, date<as.Date("2021-11-04"))


prop.table(table(data2$days, data2$Nextstrain_variants))

#data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2', '21K (Omicron)',"Delta"))
data2<-read_excel('Updated_SA_all data_15Dec21.xlsx')
data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Delta","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2','21K (Omicron)',"Other Lineages"))

#data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2','21K (Omicron)',"Delta"))
data2$division<-factor(data2$division,levels = c("Mpumalanga","North West","Northern Cape","Free State","Eastern Cape","Limpopo","Western Cape","Gauteng","KwaZulu-Natal"))

data2$days<-as.Date(cut(data2$date,breaks = "day",start.on.monday = FALSE))
data2$date<-as.Date(cut(data2$date,breaks = "week",start.on.monday = FALSE))
data2$date2<-as.Date(cut(data2$date,breaks = "2 week",start.on.monday = FALSE))
data2$date4<-as.Date(cut(data2$date,breaks = "1 month",start.on.monday = FALSE))

#data2<- data2 %>% filter(Nextstrain_variants!="Other lineages")
data2<- data2 %>% filter(division!="South Africa")
data2<- subset(data2, !is.na(division))

P <- prop.table(table(data2$days, data2$Nextstrain_variants), margin=1)

temp<-as.data.frame(P)
names(temp)[1] <- 'days'
#names(temp)[3] <- 'total_daily_7day'
temp <- temp %>% 
  dplyr::mutate(Freq_7day = zoo::rollmean(Freq, k = 10, fill = NA)
  ) %>% 
  dplyr::ungroup()
head(temp)

temp2<-provincial_cases[c("days","total_daily_7day")]
head(temp2)

library(plyr)
temp3<-join(temp, temp2,
            type = "left")

#temp3<-subset(temp3,!is.na(days))
tail(temp3)

temp3$days<-as.Date(cut(as.Date(temp3$days,format='%Y-%m-%d'),
                               breaks = "day",
                               start.on.monday = FALSE))


# Create empty data frame
extra.data <- data.frame()

# Populate the data frame using a for loop
for (i in seq(as.Date("2021/12/07"), by = "day", length.out = 9)) {
  # Get the row data
  days <- as.Date(i)
  total_daily_7day <- subset(temp2,days==as.Date(i))$total_daily_7day
  
  
  # Populate the row
  new.row1 <- data.frame(days = days, Var2 = "21K (Omicron)", Freq=1,Freq_7day = 9.939394e-01,total_daily_7day=total_daily_7day)
  new.row2 <- data.frame(days = days, Var2 = "Delta",  Freq=0,Freq_7day = 3.030303e-03,total_daily_7day=total_daily_7day)
  new.row3 <- data.frame(days = days, Var2 = "20I (Alpha, V1)", Freq=0, Freq_7day = 4.943962e-18,total_daily_7day=total_daily_7day)
  new.row4 <- data.frame(days = days, Var2 = "20H (Beta, V2)",  Freq=0,Freq_7day = 3.030303e-03,total_daily_7day=total_daily_7day)
  new.row4 <- data.frame(days = days, Var2 = "C.1.2",  Freq=0,Freq_7day = 5.984796e-18,total_daily_7day=total_daily_7day)
  
  # Add the row
  extra.data <- rbind(extra.data, new.row1)
  extra.data <- rbind(extra.data, new.row2)
  extra.data <- rbind(extra.data, new.row3)
  extra.data <- rbind(extra.data, new.row4)
  
  
  
}

# Print the data frame
extra.data

temp3 <- rbind(temp3, extra.data)

p_Epi_SA<-ggplot() + 
  theme_minimal()+
  #geom_hline(yintercept=10000, color='grey50', linetype=2) +
  
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  #scale_fill_manual(values=c('indianred3','aquamarine3','grey40','yellow2','dodgerblue3','grey90'), name='Variants', labels=c('Delta (B.1.617.2/AY.x)','Beta (B.1.351)','Alpha (B.1.1.7)','C.1.2','B.1.1.529','Other Lineages'))+
  scale_fill_manual(values=c('mediumseagreen','bisque2','grey40','dodgerblue3','deeppink2','grey90'), name='Variants', labels=c('Delta (B.1.617.2/AY.x)','Beta (B.1.351)','Alpha (B.1.1.7)','C.1.2','Omicron (B.1.1.529)','Other Lineages'))+
  geom_density(data=temp3, aes(x = days, y = total_daily_7day*Freq_7day, fill = Var2),stat="identity",size=0.4, position='stack')+
  ylab('Daily Cases\n(7-day Moving Average)')+
  xlab('Date')+
  #geom_line(data = R_estimates, aes(x = date, y = median_R_mean*10000, color = "Re"), size=0.8) +
  scale_color_manual(values=c('purple4'), name='')+
  #geom_hline(yintercept=600, color='purple4', linetype=2) +
  #geom_ribbon(data = R_estimates,aes(x=date, ymin=median_R_lowHPD*10000, ymax=median_R_highHPD*10000), fill='purple4', alpha=0.2) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases\n(7-day Moving Average)"
    
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./10000, name="Re")
    
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.text= element_text(size=8, family="Helvetica")) +
  ggtitle('Epidemic and Variant Dynamics in South Africa')

p_Epi_SA

data2$date_submitted<-as.Date(data2$date_submitted)

data2$days_submitted<-as.Date(cut(data2$date_submitted,breaks = "day",start.on.monday = FALSE))


data2$submission_lag=data2$days_submitted-data2$days

bots_data<-read_excel('Bots_Omicron_Nov_Dec_15Dec2021.xlsx')

bots_data$days<-as.Date(cut(bots_data$date,breaks = "day",start.on.monday = FALSE))
bots_data$date<-as.Date(cut(bots_data$date,breaks = "week",start.on.monday = FALSE))
bots_data$date2<-as.Date(cut(bots_data$date,breaks = "2 week",start.on.monday = FALSE))
bots_data$date4<-as.Date(cut(bots_data$date,breaks = "1 month",start.on.monday = FALSE))

bots_data$date_submitted<-as.Date(bots_data$date_submitted)

bots_data$days_submitted<-as.Date(cut(bots_data$date_submitted,breaks = "day",start.on.monday = FALSE))
bots_data$submission_lag=bots_data$days_submitted-bots_data$days

data2 <- rbind(data2,bots_data)

omicron_patient_data<-read.table('patient_status_omicrongisaid_hcov-19_2021_12_05_17.tsv',sep='\t', header = TRUE)

p_Detection<-ggplot()+
  theme_minimal()+
  geom_bar(data=subset(subset(data2,Nextstrain_variants=="21K (Omicron)"),!is.na(strain)),mapping=aes(x=days_submitted,fill=division), stat ='bin',color='black', size=0.2)+
                   
  geom_bar_pattern(data=subset(subset(subset(data2,Nextstrain_variants=="21K (Omicron)"),date_submitted==as.Date("2021/11/23")),!is.na(strain)),mapping=aes(x=days_submitted,fill=division,pattern=originating_lab=="LANCET LABORATORY"),show.legend = FALSE, stat ='bin',color='black', size=0.2,
                   pattern_angle = 45, 
                   pattern_density = 0.1, pattern_spacing = 0.025, 
                   pattern_key_scale_factor = 0.8)+
  geom_line(data=provincial_cases, aes(x=days,y=total_daily_7day/80), size=1)+
  scale_pattern_manual(values = c("none", "stripe"),guide = FALSE) +
  
  #scale_size_manual(values=c(0.5,2))+
  scale_fill_manual(values=c('#fff7f3','#fde0dd','white','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#49006a','goldenrod2'), name='')+
  scale_color_manual(values=c('black','white'),label=c('SGTF targetted sequencing','Baseline Surveillance'),name='Sampling Strategy')+
  xlab('Date of GISAID Submission')+
  scale_x_date(date_labels = "%d-%b",date_breaks = "week",limits=as.Date(c("2021/11/01","2021/12/14")))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "No. of Omicron Genomes\nshared per day",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*80, name="New Cases in South Africa\n7-day Moving Average")
    
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="top",legend.text= element_text(size=8, family="Helvetica")) +
  
  ggtitle("Timeline of Omicron Detection")+
  theme(plot.margin=unit(c(0.2,0.2,2.2,0.2),"cm"))

p_Detection

plot_grid(p_Epi_SA,p_Detection,ncol=2)




library(rgeos)
library(raster)
library(maps)
library(ggmap)
library(ggplot2)
library(scales)
library(gridExtra)
library(plyr)
library(cowplot)
library("readxl")



theme_opts<-list(theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_blank(),
                       axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(),
                       axis.ticks = element_blank(),
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       plot.title = element_blank(),
                       legend.title=element_text(size=12, face="bold"),
                       legend.text=element_text(size=10)))




SA_original<-getData("GADM", country="ZAF", level=1)
#plot(SA, lwd=0.1, border="black")

#data2[data2=="North-West"]<-"North West"

##### Omicron genomic prevalence maps

SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(data2,date==as.Date('2021-11-28'))
data4<-subset(data3,Nextstrain_variants=="21K (Omicron)")

df_count <- data3 %>% count("division")
df_count

#change division to NAME_1
names(df_count)[names(df_count) == "division"] <- "NAME_1"
names(df_count)[names(df_count) == "freq"] <- "Sequences"
df_count

df_count1 <- data4 %>% count("division")
df_count1
names(df_count1)[names(df_count1) == "division"] <- "NAME_1"
names(df_count1)[names(df_count1) == "freq"] <- "Delta"
df_count1

df_count<-merge(df_count,df_count1,by=c("NAME_1"))
df_count

df_count$prev=df_count$Delta/df_count$Sequences

df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Omicron5<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=prev), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Eastern Cape'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  geom_polygon(data=subset(SA_df, NAME_1=='Limpopo'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Mpumalanga'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  geom_polygon(data=subset(SA_df, NAME_1=='North West'), aes(long,lat,group=group, fill=0.83), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Gauteng'), aes(long,lat,group=group, fill=0.95), size=0.1, show.legend = FALSE)+
  
  geom_path(data=SA_df, aes(long,lat, group=group), color="white",
            size=0.2) +
  scale_fill_distiller(palette = "PuRd",limits=c(0,1), direction=1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,0.5,1),labels=c(0.0,0.5,1.0),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Omicron5

# Week 4
# NAME_1 Sequences Delta      prev
# 1      Botswana        20    20 1.0000000
# 2       Gauteng        89    82 0.9213483
# 3 KwaZulu-Natal        35    15 0.4285714
# 4    North West         6     5 0.8333333
# 5  Western Cape        29    26 0.8965517

# Week 3
# NAME_1 Sequences Delta      prev
# 1      Botswana         1     1 1.0000000
# 2  Eastern Cape         1     1 1.0000000
# 3       Gauteng       215   205 0.9534884
# 4 KwaZulu-Natal         8     3 0.3750000
# 5       Limpopo         1     1 1.0000000
# 6    Mpumalanga         2     2 1.0000000
# 7  Western Cape        13     5 0.3846154



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(data2,date==as.Date('2021-11-21'))
data4<-subset(data3,Nextstrain_variants=="21K (Omicron)")

df_count <- data3 %>% count("division")
df_count

#change division to NAME_1
names(df_count)[names(df_count) == "division"] <- "NAME_1"
names(df_count)[names(df_count) == "freq"] <- "Sequences"
df_count

df_count1 <- data4 %>% count("division")
df_count1
names(df_count1)[names(df_count1) == "division"] <- "NAME_1"
names(df_count1)[names(df_count1) == "freq"] <- "Delta"
df_count1

df_count<-merge(df_count,df_count1,by=c("NAME_1"))
df_count

df_count$prev=df_count$Delta/df_count$Sequences

df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Omicron4<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=prev), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Eastern Cape'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Limpopo'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  #geom_polygon(data=subset(SA_df, NAME_1=='Mpumalanga'), aes(long,lat,group=group, fill=1), size=0.1, show.legend = FALSE)+
  
    geom_path(data=SA_df, aes(long,lat, group=group), color="white",
          size=0.2) +
  scale_fill_distiller(palette = "PuRd",limits=c(0,1), direction=1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,0.5,1),labels=c(0.0,0.5,1.0),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Omicron4
# Week 4
# NAME_1 Sequences Delta      prev
# 1      Botswana        20    20 1.0000000
# 2       Gauteng        89    82 0.9213483
# 3 KwaZulu-Natal        35    15 0.4285714
# 4    North West         6     5 0.8333333
# 5  Western Cape        29    26 0.8965517

# Week 3
# NAME_1 Sequences Delta      prev
# 1      Botswana         1     1 1.0000000
# 2  Eastern Cape         1     1 1.0000000
# 3       Gauteng       215   205 0.9534884
# 4 KwaZulu-Natal         8     3 0.3750000
# 5       Limpopo         1     1 1.0000000
# 6    Mpumalanga         2     2 1.0000000
# 7  Western Cape        13     5 0.3846154



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(data2,date==as.Date('2021-11-14'))
data4<-subset(data3,Nextstrain_variants=="21K (Omicron)")

df_count <- data3 %>% count("division")
df_count

#change division to NAME_1
names(df_count)[names(df_count) == "division"] <- "NAME_1"
names(df_count)[names(df_count) == "freq"] <- "Sequences"
df_count

df_count1 <- data4 %>% count("division")
df_count1
names(df_count1)[names(df_count1) == "division"] <- "NAME_1"
names(df_count1)[names(df_count1) == "freq"] <- "Delta"
df_count1

df_count<-merge(df_count,df_count1,by=c("NAME_1"))
df_count

df_count$prev=df_count$Delta/df_count$Sequences

df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Omicron3<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=prev), size=0.1, show.legend = FALSE)+
  geom_path(data=SA_df, aes(long,lat, group=group), color="white",
            size=0.2) +
  scale_fill_distiller(palette = "PuRd",limits=c(0,1), direction=1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,0.5,1),labels=c(0.0,0.5,1.0),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Omicron3
# Week 3
# NAME_1 Sequences Delta      prev
# 1      Botswana         1     1 1.0000000
# 2  Eastern Cape         1     1 1.0000000
# 3       Gauteng       215   205 0.9534884
# 4 KwaZulu-Natal         8     3 0.3750000
# 5       Limpopo         1     1 1.0000000
# 6    Mpumalanga         2     2 1.0000000
# 7  Western Cape        13     5 0.3846154


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(data2,date==as.Date('2021-11-07'))
data4<-subset(data3,Nextstrain_variants=="21K (Omicron)")

df_count <- data3 %>% count("division")
df_count

#change division to NAME_1
names(df_count)[names(df_count) == "division"] <- "NAME_1"
names(df_count)[names(df_count) == "freq"] <- "Sequences"
df_count

df_count1 <- data4 %>% count("division")
df_count1
names(df_count1)[names(df_count1) == "division"] <- "NAME_1"
names(df_count1)[names(df_count1) == "freq"] <- "Delta"
df_count1

df_count<-merge(df_count,df_count1,by=c("NAME_1"))
df_count

df_count$prev=df_count$Delta/df_count$Sequences

df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Omicron2<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=prev), size=0.1, show.legend = FALSE)+
  geom_path(data=SA_df, aes(long,lat, group=group), color="white",
            size=0.2) +
  scale_fill_distiller(palette = "PuRd",limits=c(0,1), direction=1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,0.5,1),labels=c(0.0,0.5,1.0),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Omicron2

SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(data2,date==as.Date('2021-10-31'))
data4<-subset(data3,Nextstrain_variants=="21K (Omicron)")

df_count <- data3 %>% count("division")
df_count

#change division to NAME_1
names(df_count)[names(df_count) == "division"] <- "NAME_1"
names(df_count)[names(df_count) == "freq"] <- "Sequences"
df_count

df_count1 <- data4 %>% count("division")
df_count1
names(df_count1)[names(df_count1) == "division"] <- "NAME_1"
names(df_count1)[names(df_count1) == "freq"] <- "Delta"
df_count1

df_count<-merge(df_count,df_count1,by=c("NAME_1"))
df_count

df_count$prev=df_count$Delta/df_count$Sequences

df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Omicron1<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=prev), size=0.1, show.legend = FALSE)+
  geom_path(data=SA_df, aes(long,lat, group=group), color="white",
            size=0.2) +
  scale_fill_distiller(palette = "PuRd",limits=c(0,1), direction=1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,0.5,1),labels=c(0.0,0.5,1.0),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Omicron1
  

plot_grid(p_Omicron1,p_Omicron2,p_Omicron3,p_Omicron4,p_Omicron5,ncol=5)

#### Case maps

pop_data<-read_excel('Provinces popn estimates.xlsx')

provincial_cases_daily<-subset(provincial_cases,
  select=c('GP_daily','KZN_daily','WC_daily','EC_daily','LP_daily','MP_daily','NC_daily','NW_daily','FS_daily','date'))

names(provincial_cases_daily)[1] <- 'GT'
names(provincial_cases_daily)[2] <- 'KZN'
names(provincial_cases_daily)[3] <- 'WC'
names(provincial_cases_daily)[4] <- 'EC'
names(provincial_cases_daily)[5] <- 'LIM'
names(provincial_cases_daily)[6] <- 'MP'
names(provincial_cases_daily)[7] <- 'NC'
names(provincial_cases_daily)[8] <- 'NW'
names(provincial_cases_daily)[9] <- 'FS'


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(provincial_cases_daily,date==as.Date('2021-11-21'))

data3<-data3[sapply(data3, is.numeric)]  
df_count<-data.frame(sapply(data3, mean, na.rm = T)) # Returns a vector
df_count

df_count <- cbind(CC_1 = rownames(df_count), df_count)
rownames(df_count) <- 1:nrow(df_count)

colnames(df_count)<-c('CC_1','mean_daily')


df_count<-join(df_count,pop_data, by="CC_1")
df_count

df_count$mean_daily_per_100k<-(df_count$mean_daily/df_count$Population_2021)*100000
df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="CC_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Cases4<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=mean_daily_per_100k), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_viridis_c(option = 'magma', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,45),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Cases4



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(provincial_cases_daily,date==as.Date('2021-11-14'))

data3<-data3[sapply(data3, is.numeric)]  
df_count<-data.frame(sapply(data3, mean, na.rm = T)) # Returns a vector
df_count

df_count <- cbind(CC_1 = rownames(df_count), df_count)
rownames(df_count) <- 1:nrow(df_count)

colnames(df_count)<-c('CC_1','mean_daily')


df_count<-join(df_count,pop_data, by="CC_1")
df_count

df_count$mean_daily_per_100k<-(df_count$mean_daily/df_count$Population_2021)*100000
df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="CC_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Cases3<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=mean_daily_per_100k), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_viridis_c(option = 'magma', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,45),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Cases3



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(provincial_cases_daily,date==as.Date('2021-11-07'))

data3<-data3[sapply(data3, is.numeric)]  
df_count<-data.frame(sapply(data3, mean, na.rm = T)) # Returns a vector
df_count

df_count <- cbind(CC_1 = rownames(df_count), df_count)
rownames(df_count) <- 1:nrow(df_count)

colnames(df_count)<-c('CC_1','mean_daily')


df_count<-join(df_count,pop_data, by="CC_1")
df_count

df_count$mean_daily_per_100k<-(df_count$mean_daily/df_count$Population_2021)*100000
df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="CC_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Cases2<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=mean_daily_per_100k), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_viridis_c(option = 'magma', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,45),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Cases2


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(provincial_cases_daily,date==as.Date('2021-10-31'))

data3<-data3[sapply(data3, is.numeric)]  
df_count<-data.frame(sapply(data3, mean, na.rm = T)) # Returns a vector
df_count

df_count <- cbind(CC_1 = rownames(df_count), df_count)
rownames(df_count) <- 1:nrow(df_count)

colnames(df_count)<-c('CC_1','mean_daily')


df_count<-join(df_count,pop_data, by="CC_1")
df_count

df_count$mean_daily_per_100k<-(df_count$mean_daily/df_count$Population_2021)*100000
df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="CC_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Cases1<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=mean_daily_per_100k), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_viridis_c(option = 'magma', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,45),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Cases1


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(provincial_cases_daily,date==as.Date('2021-11-28'))

data3<-data3[sapply(data3, is.numeric)]  
df_count<-data.frame(sapply(data3, mean, na.rm = T)) # Returns a vector
df_count

df_count <- cbind(CC_1 = rownames(df_count), df_count)
rownames(df_count) <- 1:nrow(df_count)

colnames(df_count)<-c('CC_1','mean_daily')


df_count<-join(df_count,pop_data, by="CC_1")
df_count

df_count$mean_daily_per_100k<-(df_count$mean_daily/df_count$Population_2021)*100000
df_count

SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="CC_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_Cases5<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=mean_daily_per_100k), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_viridis_c(option = 'magma', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,45),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_Cases5




plot_grid(p_Cases1,p_Cases2,p_Cases3,p_Cases4,p_Cases5,ncol=5)



#### Positivity maps

positivity_data<-read_excel('TestingPositivity.xlsx')
positivity_data$date<-as.Date(cut(positivity_data$date,breaks = "week",start.on.monday = FALSE))



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-28'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_positivity5<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=positivity), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'Spectral', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,35),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_positivity5



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-21'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_positivity4<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=positivity), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'Spectral', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,35),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_positivity4



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-14'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_positivity3<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=positivity), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'Spectral', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,35),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_positivity3


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-07'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_positivity2<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=positivity), size=0.1, show.legend = TRUE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'Spectral', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,35),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_positivity2


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-10-31'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_positivity1<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=positivity), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'Spectral', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,15,30),labels=c(0,20,40),limits=c(0,35),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_positivity1

plot_grid(p_positivity1,p_positivity2,p_positivity3,p_positivity4,p_positivity5,ncol=5)





#### SGTF maps

positivity_data<-read_excel('TestingPositivity.xlsx')
positivity_data$date<-as.Date(cut(positivity_data$date,breaks = "week",start.on.monday = FALSE))



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-28'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_SGTF5<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=SGTF), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'PiYG', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,50,100),labels=c(0,50,100),limits=c(0,100),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_SGTF5



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-21'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_SGTF4<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=SGTF), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'PiYG', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,50,100),labels=c(0,50,100),limits=c(0,100),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_SGTF4


SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-14'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_SGTF3<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=SGTF), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'PiYG', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,50,100),labels=c(0,50,100),limits=c(0,100),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_SGTF3




SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-11-07'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_SGTF2<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=SGTF), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'PiYG', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,50,100),labels=c(0,50,100),limits=c(0,100),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_SGTF2



SA<-SA_original

#data3<-subset(data2,year==2020)
#data3<-subset(data3,month=='October')
data3<-subset(positivity_data,date==as.Date('2021-10-31'))

#data3<-data3[sapply(data3, is.numeric)]  
df_count<-data3
df_count


colnames(df_count)<-c('NAME_1','date','positivity','SGTF')


SA_df<-fortify(SA)


#join with KwaZulu_Natal@data
SA@data$id <- rownames(SA@data)
SA@data<-join(SA@data, df_count, by="NAME_1")
SA_df<-join(SA_df,SA@data, by="id")


#plot (fill=n)
p_SGTF1<-ggplot() + 
  geom_polygon(data=SA_df, aes(long,lat,group=group, fill=SGTF), size=0.1, show.legend = FALSE)+
  #geom_path(data=SA_df, aes(long,lat, group=group), color="#ffffcc",
  #        size=0.2) +
  scale_fill_distiller(palette = 'PiYG', direction=-1,
                       guide = guide_colourbar(frame.colour = "black", 
                                               ticks.colour = "black", # you can also remove the ticks with NA
                                               barwidth=0.8, barheight=6),
                       breaks=c(0,50,100),labels=c(0,50,100),limits=c(0,100),
                       na.value = "grey90")+
  theme(aspect.ratio=1)+theme_opts+
  labs(fill='')
p_SGTF1

plot_grid(p_SGTF1,p_SGTF2,p_SGTF3,p_SGTF4,p_SGTF5,ncol=5)






library (readr)

#urlfile="https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv"

#mydata<-read_csv(url(urlfile))
bots_cases<-read_excel('case_dates_covidzone.xlsx')


bots_cases$days<-as.Date(cut(as.Date(bots_cases$date,format='%d-%m-%Y'),
                                   breaks = "day",
                                   start.on.monday = FALSE))

bots_cases$date<-as.Date(cut(as.Date(bots_cases$date,format='%d-%m-%Y'),
                                   breaks = "week",
                                   start.on.monday = FALSE))

bots_daily_cases<-bots_cases %>% count("days")

rollspan <- 7 # span of rolling average, in days.

bots_daily_cases <- bots_daily_cases %>% 
  dplyr::mutate(new_cases_7day = zoo::rollmean(freq, k = 7, fill = NA)
                
  ) %>% 
  dplyr::ungroup()




#data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2', '21K (Omicron)',"Delta"))
data2_bots<-read_excel('Botswana_GISAID_14Dec.xlsx')
data2_bots$Nextstrain_variants<-factor(data2_bots$Nextstrain_variants,levels = c("Delta","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2','21K (Omicron)',"Other Lineages"))

#data2$Nextstrain_variants<-factor(data2$Nextstrain_variants,levels = c("Other Lineages","20H (Beta, V2)","20I (Alpha, V1)",'C.1.2','21K (Omicron)',"Delta"))
#data2$division<-factor(data2$division,levels = c("Mpumalanga","North West","Northern Cape","Free State","Eastern Cape","Limpopo","Western Cape","Gauteng","KwaZulu-Natal"))

data2_bots$days<-as.Date(cut(data2_bots$date,breaks = "day",start.on.monday = FALSE))
data2_bots$date<-as.Date(cut(data2_bots$date,breaks = "week",start.on.monday = FALSE))
data2_bots$date2<-as.Date(cut(data2_bots$date,breaks = "2 week",start.on.monday = FALSE))
data2_bots$date4<-as.Date(cut(data2_bots$date,breaks = "1 month",start.on.monday = FALSE))

#data2<- data2 %>% filter(Nextstrain_variants!="Other lineages")
#data2_bots<- data2_bots %>% filter(division!="South Africa")
data2_bots<- subset(data2_bots, !is.na(division))

P <- prop.table(table(data2_bots$days, data2_bots$Nextstrain_variants), margin=1)

temp<-as.data.frame(P)
names(temp)[1] <- 'days'
#names(temp)[3] <- 'total_daily_7day'
temp <- temp %>% 
  dplyr::mutate(Freq_7day = zoo::rollmean(Freq, k = 10, fill = NA)
  ) %>% 
  dplyr::ungroup()
head(temp)

temp2<-bots_daily_cases[c("days","new_cases_7day")]
head(temp2)

library(plyr)
temp3<-join(temp, temp2,
            type = "left")

#temp3<-subset(temp3,!is.na(days))
tail(temp3)

temp3$days<-as.Date(cut(as.Date(temp3$days,format='%Y-%m-%d'),
                        breaks = "day",
                        start.on.monday = FALSE))


# Create empty data frame
extra.data <- data.frame()

# Populate the data frame using a for loop
for (i in seq(as.Date("2021/12/06"), by = "day", length.out = 7)) {
  # Get the row data
  days <- as.Date(i)
  new_cases_7day <- subset(temp2,days==as.Date(i))$new_cases_7day
  
  
  # Populate the row
  new.row1 <- data.frame(days = days, Var2 = "21K (Omicron)", Freq=1,Freq_7day = 1,new_cases_7day=new_cases_7day)
  new.row2 <- data.frame(days = days, Var2 = "Delta",  Freq=0,Freq_7day = 5.551115e-18,new_cases_7day=new_cases_7day)
  #new.row3 <- data.frame(days = days, Var2 = "20I (Alpha, V1)", Freq=0, Freq_7day = 4.336809e-18,total_daily_7day=total_daily_7day)
  new.row4 <- data.frame(days = days, Var2 = "20H (Beta, V2)",  Freq=0,Freq_7day = 5.551115e-18,new_cases_7day=new_cases_7day)
  new.row4 <- data.frame(days = days, Var2 = "C.1.2",  Freq=0,Freq_7day = 5.551115e-18,new_cases_7day=new_cases_7day)
  
  # Add the row
  extra.data <- rbind(extra.data, new.row1)
  extra.data <- rbind(extra.data, new.row2)
  extra.data <- rbind(extra.data, new.row3)
  extra.data <- rbind(extra.data, new.row4)
  
  
  
}

# Print the data frame
extra.data

temp3 <- rbind(temp3, extra.data)

p_Epi_Bots<-ggplot() + 
  theme_minimal()+
  #geom_hline(yintercept=10000, color='grey50', linetype=2) +
  
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  #scale_fill_manual(values=c('indianred3','aquamarine3','grey40','yellow2','dodgerblue3','grey90'), name='Variants', labels=c('Delta (B.1.617.2/AY.x)','Beta (B.1.351)','Alpha (B.1.1.7)','C.1.2','B.1.1.529','Other Lineages'))+
  scale_fill_manual(values=c('mediumseagreen','bisque2','grey40','dodgerblue3','deeppink2','grey90'), name='Variants', labels=c('Delta (B.1.617.2/AY.x)','Beta (B.1.351)','Alpha (B.1.1.7)','C.1.2','Omicron (B.1.1.529)','Other Lineages'))+
  geom_density(data=temp3, aes(x = days, y = new_cases_7day*Freq_7day, fill = Var2),stat="identity",size=0.4, position='stack')+
  ylab('Daily Cases\n(7-day Moving Average)')+
  xlab('Date')+
  #geom_line(data = R_estimates, aes(x = date, y = median_R_mean*1000, color = "Re"), size=0.8) +
  scale_color_manual(values=c('purple4'), name='')+
  #geom_hline(yintercept=600, color='purple4', linetype=2) +
  #geom_ribbon(data = R_estimates,aes(x=date, ymin=median_R_lowHPD*1000, ymax=median_R_highHPD*1000), fill='purple4', alpha=0.2) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Daily Cases\n(7-day Moving Average)"
    
    # Add a second axis and specify its features
    #sec.axis = sec_axis(~./1000, name="Re")
    
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.text= element_text(size=8, family="Helvetica")) +
  ggtitle('Epidemic and Variant Dynamics in Botswana')

p_Epi_Bots



Bots_positivity<-read_excel('Botswana_test_positivity.xlsx')

Bots_positivity$Date<-as.Date(cut(as.Date(Bots_positivity$Date,format='%Y-%m-%d'),
                        breaks = "days",
                        start.on.monday = FALSE))


p_Tests<-ggplot(Bots_positivity,aes(x=Date, y=PercentPositive))+
  theme_minimal()+
  geom_line(aes(x=Date, y=Total_tested/100, color='Tests'), size=1)+
  
  geom_line(aes(color='Positivity'), size=1)+
  scale_color_manual(values=c('darkorange2','grey'), name='')+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 weeks", limits=as.Date(c("2021/10/01","2021/12/13")))+
  ylab('Test Positivity Rate (%)')+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Test Positivity Rate (%)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*100, name="Total Daily Tests Performed")
    
  )+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom",legend.text= element_text(size=8, family="Helvetica")) +
  ggtitle('Testing Trends in Botswana')

p_Tests

plot_grid(p_Epi_Bots,p_Tests,labels = c('A', 'B'))



