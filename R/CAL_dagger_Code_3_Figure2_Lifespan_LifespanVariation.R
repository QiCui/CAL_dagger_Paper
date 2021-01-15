


###############################################################
##### Code for CAL_dagger and decomposition
#####
##### Figure 2
#####
##### period and cohort e0 and e-dagger, CAL and CAL-dagger
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
HMD_Names<-c("DNK", "FIN", "FRATNP","ITA", "NLD",
             "NOR", "SWE","CHE", "GBRTENW", "GBR_SCO")

cex.lab_xy<-1.2
cex_axis_xy<-1.2
cex_text<-1.4
transp_alpha <- 0.3

#### for colour and transparency
col_trans_f <- rgb(0.42,0.32,0.64,transp_alpha+0.2)
col_plot_f<-c(rep(rgb(0.85, 0.73, 0.92,transp_alpha), 10))

col_trans_m <- rgb(0.19,0.64,0.33,transp_alpha+0.2)
col_plot_m<-c(rep(rgb(0.76, 0.9, 0.6,transp_alpha), 10))

#### location of the plot
par(mfrow=c(3,2))
par(mar = c(3.5, 5.5, 2, 0))

#########################
##### Period - female
#########################
plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n',cex.lab=1.5,
     las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 , main="Female")
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)),las=1, cex.axis = cex_axis_xy)
mtext(side=1, expression("e"[0][',p']), line=3, cex = 1,las=1)
mtext(side=2, expression("e"['p']^"\u2020"), line=2.5, cex = 1,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(e0_dagger_period_all, e0_dagger_period_all$Country==HMD_Names[c_HMD])
  points(summary_plot$e0_f, summary_plot$e_dagger_f, col=col_plot_f[c_HMD], ylim = c(7,14),xlim = c(70,90), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

legend(x=68,y=35, c("Selected populations", "Average"), col =c(rgb(0.85, 0.73, 0.92), rgb(0.42,0.32,0.64)), pch=16, bty = "n", cex=1.5)

#### add average
average_sub_period <- subset(e0_dagger_period_all, e0_dagger_period_all$Country=="Average")
points(average_sub_period$e0_f, average_sub_period$e_dagger_f,col=c(col_trans_f), ylim = c(7,14),xlim = c(70,90), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
e0_dagger_period_all_without_ave<-subset(e0_dagger_period_all, e0_dagger_period_all$Country!="Average")
mod1 = lm(as.numeric(as.character(e0_dagger_period_all_without_ave$e_dagger_f)) ~ as.numeric(as.character(e0_dagger_period_all_without_ave$e0_f)) + as.numeric(as.character(e0_dagger_period_all_without_ave$Period)))
slope_ed <- round(mod1$coefficients[2],2)
r_sq_ed <- round(summary(mod1)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.44", sep="")), cex=cex_text, col=rgb(0.42,0.32,0.64))
text(x=30, y=7, labels = expression(paste(R^2,"=0.98")),  cex=cex_text, col=rgb(0.42,0.32,0.64))


#########################
##### Period - male
#########################

par(mar = c(3.5, 2, 2, 2))
plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n',cex.lab=1.5,
     las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 , main="Male" )
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA,las=1, cex.axis = 1)
mtext(side=1, expression("e"[0][',p']), line=3, cex = 1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(e0_dagger_period_all, e0_dagger_period_all$Country==HMD_Names[c_HMD])
  points(summary_plot$e0_m, summary_plot$e_dagger_m, col=col_plot_m[c_HMD], ylim = c(7,14),xlim = c(70,90), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}
legend(x=68,y=35, c("Selected populations", "Average"), col =c(rgb(0.76, 0.9, 0.6), rgb(0.19,0.64,0.33) ), pch=16, bty = "n", cex=1.5)

#### add average
points(average_sub_period$e0_m, average_sub_period$e_dagger_m,col=c(col_trans_m), ylim = c(7,14),xlim = c(70,90), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
mod2 = lm(as.numeric(as.character(e0_dagger_period_all_without_ave$e_dagger_m)) ~ as.numeric(as.character(e0_dagger_period_all_without_ave$e0_m)) + as.numeric(as.character(e0_dagger_period_all_without_ave$Period)))
slope_ed <- round(mod2$coefficients[2],2)
r_sq_ed <- round(summary(mod2)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.30")),  cex=cex_text, col=rgb(0.19,0.64,0.33))
text(x=30, y=7, labels = expression(R^2==0.96),  cex=cex_text, col=rgb(0.19,0.64,0.33))


#########################
##### Cohort - female
#########################
par(mar = c(4.5, 5.5, 1, 0))

plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n',cex.lab=1.5, las=1, xlab = "", ylab ="" ,frame = FALSE,xaxt="n",yaxt="n",cex=0.8)
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)),las=1, cex.axis = cex_axis_xy)
mtext(side=1, expression("e"[0][',c']), line=3, cex = 1)
mtext(side=2, expression("e"['c']^"\u2020"), line=2.5, cex = 1,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country==HMD_Names[c_HMD])
  points(summary_plot$e0_f,summary_plot$e_dagger_f, col=col_plot_f[c_HMD], las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
average_sub_cohort <- subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country=="Average")
points(average_sub_cohort$e0_f, average_sub_cohort$e_dagger_f,col=c(col_trans_f), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
e0_dagger_cohort_all_without_ave<-subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country!="Average")
mod3 = lm(as.numeric(as.character(e0_dagger_cohort_all_without_ave$e_dagger_f)) ~ as.numeric(as.character(e0_dagger_cohort_all_without_ave$e0_f)) + as.numeric(as.character(e0_dagger_cohort_all_without_ave$Cohort)))
slope_ed <- round(mod3$coefficients[2],2)
r_sq_ed <- round(summary(mod3)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.40")),  cex=cex_text, col=rgb(0.42,0.32,0.64))
text(x=30, y=7, labels = expression(R^2==0.93),  cex=cex_text, col=rgb(0.42,0.32,0.64))

#########################
##### Cohort - male
#########################
par(mar = c(4.5, 2, 1, 2))

plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n',cex.lab=1.5, las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 )
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA,las=1, cex.axis = cex_axis_xy)
mtext(side=1, expression("e"[0][',c']), line=3, cex = 1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country==HMD_Names[c_HMD])
  points(summary_plot$e0_m,summary_plot$e_dagger_m, col=col_plot_m[c_HMD], las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
points(average_sub_cohort$e0_m, average_sub_cohort$e_dagger_m,col=c(col_trans_m), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
mod4 = lm(as.numeric(as.character(e0_dagger_cohort_all_without_ave$e_dagger_m)) ~ as.numeric(as.character(e0_dagger_cohort_all_without_ave$e0_m)) + as.numeric(as.character(e0_dagger_cohort_all_without_ave$Cohort)))
slope_ed <- round(mod4$coefficients[2],2)
r_sq_ed <- round(summary(mod4)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.26")),  cex=cex_text, col=rgb(0.19,0.64,0.33))
text(x=30, y=7, labels = expression(R^2==0.82),  cex=cex_text, col=rgb(0.19,0.64,0.33))



#########################
##### CAL - female
#########################
par(mar = c(5.5, 5.5, 1, 0))

plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n', las=1, xlab = "", ylab ="" ,frame = FALSE,xaxt="n",yaxt="n",cex=0.8)
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)), las=1, cex.axis = cex_axis_xy)
mtext(side=1, expression("CAL"), line=3, cex = cex.lab_xy-0.2)
mtext(side=2, expression("CAL"^"\u2020"), line=2.5, cex = cex.lab_xy-0.2,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(cal_dagger_all, cal_dagger_all$Country==HMD_Names[c_HMD])
  points(summary_plot$cal_f,summary_plot$cal_f_dagger, col=col_plot_f[c_HMD], las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
average_sub_cal <- subset(cal_dagger_all, cal_dagger_all$Country=="Average")
points(average_sub_cal$cal_f, average_sub_cal$cal_f_dagger,col=c(col_trans_f), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
CAL_dagger_cohort_all_without_ave<-subset(cal_dagger_all, cal_dagger_all$Country!="Average")
mod5 = lm(as.numeric(as.character(CAL_dagger_cohort_all_without_ave$cal_f_dagger)) ~ as.numeric(as.character(CAL_dagger_cohort_all_without_ave$cal_f)) + as.numeric(as.character(CAL_dagger_cohort_all_without_ave$Interested_year)))
slope_ed <- round(mod5$coefficients[2],2)
r_sq_ed <- round(summary(mod5)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.37")),  cex=cex_text, col=rgb(0.42,0.32,0.64))
text(x=30, y=7, labels = expression(R^2==0.87),  cex=cex_text, col=rgb(0.42,0.32,0.64))

#########################
##### CAL - male
#########################

par(mar = c(5.5, 2, 1, 2))

plot(NULL, ylim = c(5,32.5),xlim = c(20,90),yaxt='n',xaxt='n', las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 )
axis(1, at = seq(20, 90,5), labels = c(seq(20, 90,5)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA, las=1, cex.axis = cex_axis_xy)
mtext(side=1, expression("CAL"), line=3, cex = cex.lab_xy-0.2)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot <- subset(cal_dagger_all, cal_dagger_all$Country==HMD_Names[c_HMD])
  points(summary_plot$cal_m,summary_plot$cal_m_dagger, col=col_plot_m[c_HMD], las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
average_sub_cal <- subset(cal_dagger_all, cal_dagger_all$Country=="Average")
points(average_sub_cal$cal_m, average_sub_cal$cal_m_dagger,col=c(col_trans_m), las=1, cex = 1, lwd=3, pch=16)

#### calculate slope and r-square
mod6= lm(as.numeric(as.character(CAL_dagger_cohort_all_without_ave$cal_m_dagger)) ~ as.numeric(as.character(CAL_dagger_cohort_all_without_ave$cal_m)) + as.numeric(as.character(CAL_dagger_cohort_all_without_ave$Interested_year)))
slope_ed <- round(mod6$coefficients[2],2)
r_sq_ed <- round(summary(mod6)$r.squared,2)
slope_ed
r_sq_ed
text(x=30, y=12, labels = expression(paste(alpha,"=-0.28")),  cex=cex_text, col=rgb(0.42,0.32,0.64))
text(x=30, y=7, labels = expression(R^2==0.77),  cex=cex_text, col=rgb(0.42,0.32,0.64))

















