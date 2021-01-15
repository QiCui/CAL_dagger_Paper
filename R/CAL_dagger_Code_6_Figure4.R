

###############################################################
##### Code for CAL_dagger and decomposition
#####
##### Figure 4
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
#### load data and functions
####
load("Data1.RData")
load("Data2.RData")
px_all <- rbind(px_all_1, px_all_2)

source("R/CAL_dagger_contour_plot_functions.R")

###############################################
################## Initial setting
###############################################

## Interested year
## Starts from 1989 to 2016
Int_Year <- 2016
### Interested sex
## Female or Male
Sex = "Male"

## Choose the reference population
Pop2 <- "Average"

###############################################
################## SWE vs. Average
###############################################

## re-format the sex variable
if (Sex=="Female") {
  Int_sex = c(1,2,4)
} else {
  Int_sex = c(1,2,5)
}

## DNK(Denmark), Fin(Finland), FRATNP(France), ITA(Italy), NLD(Netherlands)
## NOR(Norway), SWE(Sweden), CHE(Switzerland), GBR_SCO(Scotland), GBRTENW(England & Wales)
## Average
Pop1 <- "SWE"

cal_px_1 <- subset(px_all, px_all$Country == Pop1 & px_all$Int_year ==Int_Year)[, Int_sex]
cal_px_2 <- subset(px_all, px_all$Country == Pop2 & px_all$Int_year ==Int_Year)[, Int_sex]

length_age <- 110
Year_cohort<-unique(cal_px_1$Cohort)

i_year<-1
age_contribution_all_cohort<-cum_matrix<-c()
repeat{

  px_1<-subset(cal_px_1,cal_px_1$Cohort==Year_cohort[i_year])[,3]
  px_2<-subset(cal_px_2,cal_px_2$Cohort==Year_cohort[i_year])[,3]

  log_p<-log(px_1/px_2)
  mid_lx<-((c(1,cumprod(px_1))[length(px_1)])+(c(1,cumprod(px_2))[length(px_2)]))*0.5

  cohort_contribution<- (-log_p*mid_lx*(1+log(mid_lx)))
  cohort_contribution[is.na(cohort_contribution)|cohort_contribution>999|cohort_contribution<(-999)]=0

  cum_matrix_pre<-cumsum(cohort_contribution)
  cum_matrix<-cbind(cum_matrix, rev(c(cum_matrix_pre,rep(0,110-length(cohort_contribution)))))

  cohort_contribution<-c(cohort_contribution, rep(0,(110-length(cohort_contribution))))
  cohort_contribution<-rev(cohort_contribution)

  age_contribution_all_cohort<-cbind(age_contribution_all_cohort, cohort_contribution)

  i_year=i_year+1
  if (i_year==length(Year_cohort)+1) break
}

## lexis format
i_r=1
Lexis_age_con<-Lexis_cum_con<-c()
repeat{
  Lexis_age_con_pre<-c(rep(0, 110-i_r),age_contribution_all_cohort[i_r,])[1:110]
  Lexis_cum_con_pre<-c(rep(0, 110-i_r),cum_matrix[i_r,])[1:110]
  Lexis_age_con<-rbind(Lexis_age_con,Lexis_age_con_pre)
  Lexis_cum_con<-rbind(Lexis_cum_con,Lexis_cum_con_pre)
  i_r=i_r+1
  if (i_r==111) break
}

## filled_contour format
j_r=1
FCF_age_con<-FCF_cum_con<-c()
repeat{
  FCF_age_con_pre<-Lexis_age_con[j_r,]
  FCF_cum_con_pre<-Lexis_cum_con[j_r,]
  FCF_age_con<-cbind(FCF_age_con_pre,FCF_age_con)
  FCF_cum_con<-cbind(FCF_cum_con_pre,FCF_cum_con)
  j_r=j_r+1
  if (j_r==111) break
}
FCF_age_con_SWE.m = FCF_age_con

###############################################
################## ITA vs. Average
###############################################

Pop3 <- "ITA"

cal_px_1 <- subset(px_all, px_all$Country == Pop3 & px_all$Int_year ==Int_Year)[, Int_sex]
cal_px_2 <- subset(px_all, px_all$Country == Pop2 & px_all$Int_year ==Int_Year)[, Int_sex]

length_age <- 110
Year_cohort<-unique(cal_px_1$Cohort)

i_year<-1
age_contribution_all_cohort<-cum_matrix<-c()
repeat{

  px_1<-subset(cal_px_1,cal_px_1$Cohort==Year_cohort[i_year])[,3]
  px_2<-subset(cal_px_2,cal_px_2$Cohort==Year_cohort[i_year])[,3]

  log_p<-log(px_1/px_2)
  mid_lx<-((c(1,cumprod(px_1))[length(px_1)])+(c(1,cumprod(px_2))[length(px_2)]))*0.5

  cohort_contribution<- (-log_p*mid_lx*(1+log(mid_lx)))
  cohort_contribution[is.na(cohort_contribution)|cohort_contribution>999|cohort_contribution<(-999)]=0

  cum_matrix_pre<-cumsum(cohort_contribution)
  cum_matrix<-cbind(cum_matrix, rev(c(cum_matrix_pre,rep(0,110-length(cohort_contribution)))))

  cohort_contribution<-c(cohort_contribution, rep(0,(110-length(cohort_contribution))))
  cohort_contribution<-rev(cohort_contribution)

  age_contribution_all_cohort<-cbind(age_contribution_all_cohort, cohort_contribution)

  i_year=i_year+1
  if (i_year==length(Year_cohort)+1) break
}


## lexis format
i_r=1
Lexis_age_con<-Lexis_cum_con<-c()
repeat{
  Lexis_age_con_pre<-c(rep(0, 110-i_r),age_contribution_all_cohort[i_r,])[1:110]
  Lexis_cum_con_pre<-c(rep(0, 110-i_r),cum_matrix[i_r,])[1:110]
  Lexis_age_con<-rbind(Lexis_age_con,Lexis_age_con_pre)
  Lexis_cum_con<-rbind(Lexis_cum_con,Lexis_cum_con_pre)
  i_r=i_r+1
  if (i_r==111) break
}

## filled_contour format
j_r=1
FCF_age_con<-FCF_cum_con<-c()
repeat{
  FCF_age_con_pre<-Lexis_age_con[j_r,]
  FCF_cum_con_pre<-Lexis_cum_con[j_r,]
  FCF_age_con<-cbind(FCF_age_con_pre,FCF_age_con)
  FCF_cum_con<-cbind(FCF_cum_con_pre,FCF_cum_con)
  j_r=j_r+1
  if (j_r==111) break
}
FCF_age_con_ITA.m = FCF_age_con

###############################################
################## PLOT
###############################################
options(scipen=18)
Year<-seq((Int_Year-110), (Int_Year)-1,1)
Age<-seq(0, 109)
###### setting colors
WildColors<-rev(c("#67001f","#b2182b","#d6604d","#f4a582","#f8c9b4","#fde2d2","#fde9dd","#fef4ee", ## from dark red to light red
                  "white","white",
                  "#f1f7fa","#e3eff6","#d3e7f1","#bddceb","#92c5de","#4393c3","#2166ac","#053061"))## from light blue to dark blue

levels_age<-c(-0.050,-0.02,-0.0100,-0.0050,-0.0010,-0.0005,-0.0001,-0.00005,-0.00001,
          0,
          0.00001,0.00005,0.0001,0.0005,0.0010,0.0050,0.0100,0.02,0.05)/10

levels_age_c<-c(format(round(levels_age[1:floor(19/2)], digits = 6), scientific=F),
           "0",
           format(round(levels_age[(floor(19/2)+2):length(levels_age)], digits = 6), scientific=F))

###### Contour plot
plot.new()

## Pop1
par(xpd=NA,cex = 1)
text(x =-0.05 ,y = 0.5,"Age",srt = 90,cex = 1.3)
text(x = 0.45,y = -0.1,"Year",cex = 1.3)

par(new = "TRUE",plt = c(0.1,0.42,0.17,0.9),las = 1,cex.axis = 1.3)
filled.contour3(Year,Age,FCF_age_con_SWE.m,levels=levels_age, col=WildColors,
                plot.axes={
                  axis(1,at = seq(Int_Year-110, Int_Year,10), las=1)
                  axis(2,at = seq(0, 100,10), las=1)},
                key.axes=customAxis(),
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
MakeLetter(paste(Pop1,"-", Sex, sep = ""), cex=1.3)
text(Int_Year-110+15, 98, paste("Gap = ", round(sum(FCF_age_con_SWE.m), digits = 2), sep = ""),cex=1.2)

## Pop3
par(new = "TRUE",plt =c(0.5,0.82,0.17,0.9),las = 1,cex.axis = 1.3)
filled.contour3(Year,Age,FCF_age_con_ITA.m,levels=levels_age, col=WildColors,
                plot.axes={
                  axis(1,at = seq(Int_Year-110, Int_Year,10), las=1)
                  axis(2,at = seq(0, 100,10), las=1)},
                key.axes=customAxis(),
                cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
MakeLetter(paste(Pop3,"-", Sex, sep = ""), cex=1.3)
text(Int_Year-110+15, 98, paste("Gap = ", round(sum(FCF_age_con_ITA.m), digits = 2), sep = ""),cex=1.2)


#Add a legend:
par(new = "TRUE",plt = c(0.85,0.88,0.17,0.9),las = 1,cex = 1.3)
filled.legend(Year,Age,FCF_age_con_SWE.m,levels=levels_age, col=WildColors,
              key.axes=customAxis(),
              font=2,las=2, cex.axis=1.3)













