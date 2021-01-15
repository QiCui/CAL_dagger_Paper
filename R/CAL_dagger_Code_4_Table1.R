
###############################################################
##### Code for CAL_dagger and decomposition
#####
##### Table 1
#####
##### period e0 and e_dagger, CAL and CAL_dagger 2013
#####
##### If you have any questions, please send your questions to qicui1128@gmail.com
###############################################################

####
#### Clear plot window and environment
####
dev.off()
rm(list = ls())

####
#### load data
####
load("Data1.RData")
load("Data2.RData")
px_all <- rbind(px_all_1, px_all_2)
###############################################
################## Initial setting
###############################################

#### 1989 to 2016
year = 2016

for_Table1<-merge(subset(e0_dagger_period_all, e0_dagger_period_all$Period==year),
                  subset(cal_dagger_all, cal_dagger_all$Interested_year==year),
                  by="Country", all = T)


female_section <- data.frame(Population=for_Table1$Country,
                             e0p=for_Table1$e0_f,
                             edagger_p= for_Table1$e_dagger_f,
                             Gap_e_d_p=as.numeric(for_Table1$e_dagger_f) - as.numeric(for_Table1$e_dagger_f[1]),
                             CAL = for_Table1$cal_f,
                             CAL_dagger= for_Table1$cal_f_dagger,
                             Gap_CAL_dagger = as.numeric(for_Table1$cal_f_dagger) - as.numeric(for_Table1$cal_f_dagger[1]),
                             Ratio_cal_dagger_edagger = as.numeric(for_Table1$cal_f_dagger)/as.numeric(for_Table1$e_dagger_f))


male_section <- data.frame(Population=for_Table1$Country,
                           e0p=for_Table1$e0_m,
                           edagger_p= for_Table1$e_dagger_m,
                           Gap_e_d_p=as.numeric(for_Table1$e_dagger_m) - as.numeric(for_Table1$e_dagger_m[1]),
                           CAL = for_Table1$cal_m,
                           CAL_dagger= for_Table1$cal_m_dagger,
                           Gap_CAL_dagger = as.numeric(for_Table1$cal_m_dagger) - as.numeric(for_Table1$cal_m_dagger[1]),
                           Ratio_cal_dagger_edagger = as.numeric(for_Table1$cal_m_dagger)/as.numeric(for_Table1$e_dagger_m))


female_section
male_section


