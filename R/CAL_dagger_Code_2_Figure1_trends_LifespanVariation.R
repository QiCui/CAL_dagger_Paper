


###############################################################
##### Code for CAL_dagger and decomposition
#####
##### Figure 1
#####
##### trends of period and cohort e-daggers, and CAL-dagger
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

col_plot_m<-c(rep(col_plot_m, 10))
col_plot_f<-c(rep(col_plot_f, 10))

#### location of the plot
par(mfrow=c(3,2))
par(mar = c(3.5, 5.5, 2, 0))
#########################
##### Period - female
#########################

plot(NULL, ylim = c(5,32.5),xlim = c(1880,2020),yaxt='n',xaxt='n',cex.lab=1.5, las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 , main="Female")
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)),las=1, cex.axis = cex_axis_xy)
mtext(side=1, " ", line=3, cex = 1,las=1)
mtext(side=2, expression("e"['p']^"\u2020"), line=2.5, cex = 1,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(e0_dagger_period_all, e0_dagger_period_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Period, summary_plot$e_dagger_f, col=col_plot_f[c_HMD], xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
average_e0_dagger_p <- subset(e0_dagger_period_all, e0_dagger_period_all$Country=="Average")
points(average_e0_dagger_p$Period,average_e0_dagger_p$e_dagger_f, col=c(col_trans_f), las=1, cex = 1, lwd=3, pch=16)

legend("topright", c("Selected populations", "Average"), col =c(rgb(0.85, 0.73, 0.92), rgb(0.42,0.32,0.64)),  pch=16, bty = "n", cex=1.5)


#########################
##### Period - male
#########################

par(mar = c(3.5, 2, 2, 2))
plot(NULL, ylim = c(5,32.5),xlim = c(1880,2020),yaxt='n',xaxt='n',cex.lab=1.5, las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 , main="Male")
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA,las=1, cex.axis = cex_axis_xy)
mtext(side=1, " ", line=3, cex = 1,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(e0_dagger_period_all, e0_dagger_period_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Period, summary_plot$e_dagger_m, col=col_plot_m[c_HMD], ylim = c(7,14),xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
points(average_e0_dagger_p$Period,average_e0_dagger_p$e_dagger_m, col=c(col_trans_m), las=1, cex = 1, lwd=3, pch=16)

legend("topright", c("Selected populations", "Average"), col =c(rgb(0.76, 0.9, 0.6), rgb(0.19,0.64,0.33) ), pch=16, bty = "n", cex=1.5)


#########################
##### Cohort - female
#########################
par(mar = c(4.5, 5.5, 1, 0))

plot(NULL, ylim = c(5,32.5),xlim =c(1880,2020),yaxt='n',xaxt='n',cex.lab=1.5, las=1, xlab = "", ylab ="",frame = FALSE,xaxt="n",yaxt="n",cex=0.8 )
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)),las=1, cex.axis = cex_axis_xy)
mtext(side=1, " ", line=3, cex = 1,las=1)
mtext(side=2, expression("e"['c']^"\u2020"), line=2.5, cex = 1,las=1)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Cohort, summary_plot$e_dagger_f, col=col_plot_f[c_HMD], xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

#### add average
average_e0_dagger_c <- subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country=="Average")
points(average_e0_dagger_c$Cohort, average_e0_dagger_c$e_dagger_f, col=c(col_trans_f), las=1, cex = 1, lwd=3, pch=16)

#########################
##### Cohort - male
#########################
par(mar = c(4.5, 2, 1, 2))

plot(NULL, ylim = c(5,32.5),xlim = c(1880,2015), xaxt='n', yaxt='n', cex.axis=cex_axis_xy, cex.lab=cex.lab_xy, frame = FALSE,cex=0.8, las=1, xlab = "",ylab = "")
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA,las=1, cex.axis = cex_axis_xy)
mtext(side=1, "", line=2.5, cex = cex.lab_xy-0.3)
axis(1, at = seq(1880, 2015,10), las=1,labels = c(seq(1880, 2015,10)),cex.axis=cex_axis_xy)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(e0_dagger_cohort_all, e0_dagger_cohort_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Cohort, summary_plot$e_dagger_m, col=col_plot_m[c_HMD], xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}
points(average_e0_dagger_c$Cohort, average_e0_dagger_c$e_dagger_m, col=c(col_trans_m), las=1, cex = 1, lwd=3, pch=16)


#########################
##### CAL - female
#########################
par(mar = c(5.5, 5.5, 1, 0))

plot(NULL, ylim = c(5,32.5),xlim = c(1880,2015), xaxt='n', yaxt='n', cex.axis=cex_axis_xy, cex.lab=cex.lab_xy, frame = FALSE,cex=0.8, las=1, xlab = "",ylab = "")
mtext(side=2, expression("CAL"^"\u2020"), line=2.5, cex = cex.lab_xy-0.2, las=1)
mtext(side=1, "Year", line=2.5, cex = cex.lab_xy-0.3)
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = c(seq(5, 35,5)),las=1, cex.axis = cex_axis_xy)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(cal_dagger_all, cal_dagger_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Interested_year, summary_plot$cal_f_dagger, col=col_plot_f[c_HMD], xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}
average_CAL_dagger_c <- subset(cal_dagger_all, cal_dagger_all$Country=="Average")
points(average_CAL_dagger_c$Interested_year, average_CAL_dagger_c$cal_f_dagger, col=c(col_trans_f), las=1, cex = 1, lwd=3, pch=16)

#########################
##### CAL - male
#########################
par(mar = c(5.5, 2, 1, 2))
plot(NULL, ylim = c(5,32.5),xlim = c(1880,2015), xaxt='n', yaxt='n', cex.axis=cex_axis_xy, cex.lab=cex.lab_xy, frame = FALSE,cex=0.8, las=1, xlab = "",ylab = "")

mtext(side=2, "", line=2.5, cex = cex.lab_xy-0.2)
mtext(side=1, "Year", line=2.5, cex = cex.lab_xy-0.3)
axis(1, at = seq(1880, 2020,10), labels = c(seq(1880, 2020,10)), cex.axis = cex_axis_xy)
axis(2, at = seq(5, 35,5), labels = NA,las=1, cex.axis = cex_axis_xy)
grid(col = "gray65")

c_HMD=1
repeat{
  summary_plot<-subset(cal_dagger_all, cal_dagger_all$Country==HMD_Names[c_HMD])
  points(summary_plot$Interested_year, summary_plot$cal_m_dagger, col=col_plot_m[c_HMD], xlim = c(1880,2020), las=1, cex = 1, lwd=3, pch=16)
  c_HMD=c_HMD+1
  if (c_HMD==length(HMD_Names)+1) break
}

points(average_CAL_dagger_c$Interested_year, average_CAL_dagger_c$cal_m_dagger, col=c(col_trans_m), las=1, cex = 1, lwd=3, pch=16)



