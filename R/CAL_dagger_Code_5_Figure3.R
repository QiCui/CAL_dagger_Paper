

###############################################################
##### Code for CAL_dagger and decomposition
#####
##### Figure 3
#####
##### Cumulative age-cohort contribution
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

## Interested year
## Starts from 1989 to 2016
Int_Year <- 2016

###############################################
################## Female: SWE vs. Average
###############################################

## DNK(Denmark), Fin(Finland), FRATNP(France), ITA(Italy), NLD(Netherlands)
## NOR(Norway), SWE(Sweden), CHE(Switzerland), GBR_SCO(Scotland), GBRTENW(England & Wales)
## Average
Pop1 <- "SWE"
Pop2 <- "Average"

### Interested sex
## Female or Male
Sex = "Female"

## re-format the sex variable
if (Sex=="Female") {
  Int_sex = c(1,2,4)
} else {
  Int_sex = c(1,2,5)
}

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

##  Age SUM
Cohort_effect_SWE.f<-colSums(age_contribution_all_cohort)

###############################################
################## Male: SWE vs. Average
###############################################

### Interested sex
## Female or Male
Sex = "Male"

## re-format the sex variable
if (Sex=="Female") {
  Int_sex = c(1,2,4)
} else {
  Int_sex = c(1,2,5)
}

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

##  Age SUM
Cohort_effect_SWE.m<-colSums(age_contribution_all_cohort)


###############################################
################## Female: ITA vs. Average
###############################################
Pop1 <- "ITA"
Pop2 <- "Average"

### Interested sex
## Female or Male
Sex = "Female"

## re-format the sex variable
if (Sex=="Female") {
  Int_sex = c(1,2,4)
} else {
  Int_sex = c(1,2,5)
}

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

##  Age SUM
Cohort_effect_ITA.f<-colSums(age_contribution_all_cohort)

###############################################
################## Male: ITA vs. Average
###############################################

### Interested sex
## Female or Male
Sex = "Male"

## re-format the sex variable
if (Sex=="Female") {
  Int_sex = c(1,2,4)
} else {
  Int_sex = c(1,2,5)
}

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

##  Age SUM
Cohort_effect_ITA.m<-colSums(age_contribution_all_cohort)


###############################################
################## PLOT
###############################################

par(mfrow=c(2,1))

#SWE
par(mar = c(3, 5, 1, 2))
plot(Cohort_effect_SWE.f, xaxt = 'n',yaxt = 'n', col="white",ylim = c(-0.05,0.05), ylab = "", xlab = "",frame = FALSE)

axis(side=2, at=seq(-0.05,0.05,0.01),las = 2, cex.axis=0.8)
axis(side=1, at=c(110,90,80,70,60,50,40,30,20,10,0),las=1, labels=F)
mtext("Contribution", side = 2, line = 4,cex=1)
grid(col = "gray65")
abline(h=0, col="black", lwd=1, lty=1)

lines(seq(0,109,1),Cohort_effect_SWE.f, col="#846DBA", lwd=3, lty=1)
lines(seq(0,109,1),Cohort_effect_SWE.m, col="#68A35E", lwd=4, lty=3)

legend ("topleft", legend=c("Female","Male"), col=c("#846DBA","#68A35E"), lwd=4,bty="n", cex=1,lty=c(1,3))
text(90, 0.04, "Sweden",cex=1.5)

#ITA
par(mar = c(5, 5, 0, 2))
plot(Cohort_effect_ITA.f, xaxt = 'n',yaxt = 'n', col="white",ylim = c(-0.05,0.05), ylab = "", xlab = "",frame = FALSE)

axis(side=2, at=seq(-0.05,0.05,0.01),las = 2,cex=0.8, cex.axis=0.8)
axis(side=1, at=seq(0,110,10), las=1, cex.axis=0.8,
     labels = c(paste("110\n(",Int_Year-110,")", sep = ""), paste("100\n(",Int_Year-100,")", sep = ""),
                paste("90\n(",Int_Year-90,")", sep = ""), paste("80\n(",Int_Year-80,")", sep = ""),
                paste("70\n(",Int_Year-70,")", sep = ""), paste("60\n(",Int_Year-60,")", sep = ""),
                paste("50\n(",Int_Year-50,")", sep = ""), paste("40\n(",Int_Year-40,")", sep = ""),
                paste("30\n(",Int_Year-30,")", sep = ""), paste("20\n(",Int_Year-20,")", sep = ""),
                paste("10\n(",Int_Year-10,")", sep = ""), paste("0\n(",Int_Year-0,")", sep = "")))
mtext("Contribution", side = 2, line = 4,cex=1)
mtext(paste("Age reached in ", Int_Year, " (Birth Cohort)", sep=""), side = 1, line = 3,cex=0.8)

grid(col = "gray65")
abline(h=0, col="black", lwd=1, lty=1)

lines(seq(0,109,1),Cohort_effect_ITA.f, col="#6a51a3", lwd=3, lty=1)
lines(seq(0,109,1),Cohort_effect_ITA.m, col="#68A35E", lwd=4, lty=3)

text(90, 0.04, "Italy",cex=1.5)
















