

###############################################################
##### Code for CAL_dagger and decomposition
##### To explore, please see the shinyapps
##### https://caldagger.shinyapps.io/CALdagger/?_ga=2.182331879.118419113.1608632229-217553970.1608632229
#####
##### NOTE
##### Since this code has to use online database - HMD, it might take more than 2 hours to finish the data preparation!
##### To save your time, you can directly use RData, by using load()
#####
##### If you have any questions, please send your questions to qicui1128@gmail.com
###############################################################

####
#### Clear plot window and environment
####
dev.off()
rm(list = ls())

########################
######  Library and Source
########################
# install.packages("HMDHFDplus")
library(HMDHFDplus)
HMD_account_name <- "XXXXXXXXXXXX"
HMD_account_pin  <- "XXXXXXXXXXXX"

source("R/CAL_dagger_life_table_functions.R")

########################
###### Interested population
###### Interested truncation year
###### Interested sex
########################

### Interested country
## HMD countries can cover 110 years
## DNK(Denmark), FIN(Finland), FRATNP(France), ITA(Italy), NLD(Netherlands)
## NOR(Norway), SWE(Sweden), CHE(Switzerland), GBR_SCO(Scotland), GBRTENW(England & Wales)

### Interested year
##  Starts from 1989 to 2016
Year_seqence <- seq(1989,2016,1)


############################################
######  Preparing DATA for CAL & CAL_dagger
############################################

HMD_Names<-c("DNK", "FIN", "FRATNP", "ITA", "NLD",
             "NOR", "SWE", "CHE", "GBR_SCO", "GBRTENW")

i_ayear <- 1

px_all <- lx_all <- c()
repeat{
  Int_Year <- Year_seqence[i_ayear]
  T_Year_series = Int_Year + 1
  c_HMD<-1
  px_tri_summary <- lx_tri_summary <- c()

  repeat{

    ################################################################
    ### Calculating 1qx based on original HMD data
    ################################################################

    Input_Population <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="Population", username=HMD_account_name, password=HMD_account_pin, fixup=F)
    Input_DX_lexis <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="Deaths_lexis", username=HMD_account_name, password=HMD_account_pin, fixup=F)

    # re-format the age. 0 ~ 110+ to 0~110
    Input_DX_lexis$Age<-as.numeric(as.character(Input_DX_lexis$Age))
    Input_DX_lexis$Age[is.na(Input_DX_lexis$Age)]=110
    Input_Population$Age<-as.numeric(as.character(Input_Population$Age))
    Input_Population$Age[is.na(Input_Population$Age)]=110

    Input_Population$Year <- as.numeric(as.character(substr(Input_Population$Year, 1, 4)))
    ry <- as.numeric(which(table(Input_Population$Year)>111))+Input_Population$Year[1]-1

    if (length(ry) !=0) {
      ry_i<-1
      repeat{
        A<-as.numeric(which(grepl(subset(Input_Population,
                                         Input_Population$Year==ry[ry_i])[1,1],
                                  Input_Population$Year))[1:1])                        # first
        B<-A+111                                                                       # second

        Pop_rep <- (Input_Population[A:(A+110),]+Input_Population[B:(B+110),])*0.5
        Input_Population <- rbind(Input_Population[0:(A-1),],
                                  Pop_rep,
                                  Input_Population[(B+111):length(Input_Population[,1]),])

        ry_i=ry_i+1
        if (ry_i> length(ry)) break
      }
    }

    ################################################################
    ### Add the cohort column for period data
    ################################################################

    Population_s<-c()
    cohort_year_start <- seq(T_Year_series[1]-110, T_Year_series[length(T_Year_series)],1)
    ii_year<-1

    repeat{
      Population_s_pre <- subset(Input_Population,
                                 Input_Population$Year == cohort_year_start[ii_year]+1)

      Cohort_hypo <- seq(cohort_year_start[ii_year], cohort_year_start[ii_year]-110,-1)
      Population_s_pre <- cbind(Cohort_hypo, Population_s_pre)
      Population_s <- rbind(Population_s, Population_s_pre)

      ii_year=ii_year+1
      if (ii_year==length(cohort_year_start)+1) break
    }


    ################################################################
    ### get the triangle mortality
    ################################################################

    i_year <- 1
    repeat {

      Survival_prob_Summary <- lx_summary <- c()
      target_age <- 110 # we use the mortality rates from birth to 109.99999, because in HMD 110 means "110+"
      Start_year <- T_Year_series[i_year]
      Start_cohort <- Start_year-target_age

      repeat{

        DX_lexis_c <- subset(Input_DX_lexis, Input_DX_lexis$Cohort == Start_cohort &
                               Input_DX_lexis$Age <= target_age &
                               Input_DX_lexis$Year<= Start_year)
        Population_c <- subset(Population_s, Population_s$Cohort_hypo == Start_cohort &
                                 Population_s$Age <= target_age &
                                 Population_s$Year <= Start_year)

        denominator_p <-  Population_c[, c(4,5)] + DX_lexis_c[seq(1,length(DX_lexis_c[,1])-1,2),c(4,5)]
        numerator_p <- Population_c[, c(4,5)] - DX_lexis_c[seq(2,length(DX_lexis_c[,1]),2),c(4,5)]
        Survival_prob_c <- data.frame(Country = rep(HMD_Names[c_HMD], target_age),
                                      Cohort=rep(Population_c$Cohort_hypo[1], target_age),
                                      Age=seq(0, target_age-1,1),
                                      Female=(numerator_p/denominator_p)$Female,
                                      Male=(numerator_p/denominator_p)$Male)

        lx_summary_f_s <- c(1,cumprod(Survival_prob_c$Female)[1:(length(Survival_prob_c$Female)-1)])
        lx_summary_f <- lx_summary_f_s[target_age-length(which(is.na(lx_summary_f_s) | lx_summary_f_s==0))] # avoid the NA and zero value
        lx_summary_m_s <- c(1,cumprod(Survival_prob_c$Male)[1:(length(Survival_prob_c$Male)-1)])
        lx_summary_m <- lx_summary_m_s[target_age-length(which(is.na(lx_summary_m_s) | lx_summary_m_s==0))] # avoid the NA and zero value

        lx_summary_pre <- data.frame(Country = HMD_Names[c_HMD],
                                     Cohort=Population_c$Cohort_hypo[1],
                                     Age=target_age-1,
                                     Female=lx_summary_f,
                                     Male=lx_summary_m)

        Survival_prob_Summary <- rbind(Survival_prob_Summary, Survival_prob_c)
        lx_summary <- rbind(lx_summary,lx_summary_pre)

        Start_cohort=Start_cohort+1
        target_age=target_age-1
        if (Start_cohort>=Start_year) break
      }

      lx_summary$Age <- rev(lx_summary$Age)

      i_year=i_year+1
      if (i_year==length(T_Year_series)+1) break
    }

    px_tri_summary <- rbind(px_tri_summary, Survival_prob_Summary)
    lx_tri_summary <- rbind(lx_tri_summary, lx_summary)

    c_HMD=c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }

  px_tri_summary$Int_year <- rep(Int_Year, nrow(px_tri_summary))
  lx_tri_summary$Int_year <- rep(Int_Year, nrow(lx_tri_summary))

  ################################################################
  ### get the Average
  ### Method: average px and then lx can be calculated
  ################################################################

  ### px
  px_f_average_pre <- matrix(px_tri_summary$Female, ncol = length(HMD_Names))
  px_m_average_pre <- matrix(px_tri_summary$Male, ncol = length(HMD_Names))

  px_f_average <-rowSums(px_f_average_pre, na.rm = T)/rowSums(!is.na(px_f_average_pre))
  px_m_average <-rowSums(px_m_average_pre, na.rm = T)/rowSums(!is.na(px_m_average_pre))

  px_average <- data.frame(Country = rep("Average", length(px_f_average)),
                           Cohort = subset(px_tri_summary, px_tri_summary$Country==HMD_Names[1])$Cohort,
                           Age = subset(px_tri_summary, px_tri_summary$Country==HMD_Names[1])$Age,
                           Female = px_f_average,
                           Male = px_m_average,
                           Int_year = subset(px_tri_summary, px_tri_summary$Country==HMD_Names[1])$Int_year)

  px_tri_summary <- rbind(px_tri_summary, px_average)

  ### lx
  i_birth_cohort<-1
  lx_m_summary<-lx_f_summary<-c()
  target_age<-110
  birth_co_series <- unique(px_tri_summary$Cohort)

  repeat{
    Average_px_single <- subset(px_tri_summary, px_tri_summary$Country == "Average")
    Average_px_single_subset <- subset(Average_px_single, Average_px_single$Cohort==birth_co_series[i_birth_cohort])

    lx_f_summary_s <- c(1,cumprod(Average_px_single_subset$Female)[1:(length(Average_px_single_subset$Female)-1)])
    lx_f_summary_ss <- lx_f_summary_s[target_age-length(which(is.na(lx_f_summary_s) | lx_f_summary_s==0))] # avoid the NA and zero value
    lx_m_summary_s <- c(1,cumprod(Average_px_single_subset$Male)[1:(length(Average_px_single_subset$Male)-1)])
    lx_m_summary_ss <- lx_m_summary_s[target_age-length(which(is.na(lx_m_summary_s) | lx_m_summary_s==0))] # avoid the NA and zero value

    lx_m_summary <- rbind(lx_m_summary, lx_m_summary_ss)
    lx_f_summary <- rbind(lx_f_summary, lx_f_summary_ss)

    i_birth_cohort=i_birth_cohort+1
    target_age=target_age-1
    if (i_birth_cohort==length(birth_co_series)+1) break
  }

  lx_average <- data.frame(Country = rep("Average", nrow(lx_m_summary)),
                           Cohort = subset(lx_tri_summary, lx_tri_summary$Country==HMD_Names[1])$Cohort,
                           Age = subset(lx_tri_summary, lx_tri_summary$Country==HMD_Names[1])$Age,
                           Female = lx_f_summary,
                           Male = lx_m_summary,
                           Int_year = subset(lx_tri_summary, lx_tri_summary$Country==HMD_Names[1])$Int_year)
  lx_tri_summary <- rbind(lx_tri_summary, lx_average)

  ## Total summary
  px_all <- rbind(px_all, px_tri_summary)
  lx_all <- rbind(lx_all, lx_tri_summary)

  i_ayear=i_ayear + 1
  if (i_ayear==length(Year_seqence)+1) break
}



############################################
######  Preparing DATA for Cohort
############################################
Cohort_range <- seq(min(lx_all$Cohort), min(lx_all$Cohort)+ Year_seqence[length(Year_seqence)] - Year_seqence[1])
e0_dagger_cohort_all<-c()
i_cr <- 1
repeat{
  c_HMD = 1
  e0_dagger_cohort <- c()
  repeat {
    Mx_cohort <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="cMx_1x1", username=HMD_account_name, password=HMD_account_pin, fixup=F)
    Cohort_mx_sub <- subset(Mx_cohort, Mx_cohort$Year== Cohort_range[i_cr])

    LT_c_f_sum_pre<-LT_reconstruction_female(mx=as.numeric(as.character(Cohort_mx_sub$Female)))
    LT_c_m_sum_pre<-LT_reconstruction_male(mx=as.numeric(as.character(Cohort_mx_sub$Male)))
    e0_f_sum_cohort<-LT_c_f_sum_pre$ex[1] ; e0_m_sum_cohort<-LT_c_m_sum_pre$ex[1]
    LT_c_f_sum_pre$lx[LT_c_f_sum_pre$lx==0]=1 ; LT_c_m_sum_pre$lx[LT_c_m_sum_pre$lx==0]=1
    e_f_dagger_cohort_l<-sum((LT_c_f_sum_pre$lx)*(-log(LT_c_f_sum_pre$lx)))
    e_m_dagger_cohort_l<-sum((LT_c_m_sum_pre$lx)*(-log(LT_c_m_sum_pre$lx)))

    e0_dagger_cohort <- rbind(e0_dagger_cohort, c(HMD_Names[c_HMD], Cohort_range[i_cr],
                                                  e0_f_sum_cohort, e0_m_sum_cohort, e_f_dagger_cohort_l, e_m_dagger_cohort_l))
    c_HMD=c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }
  e0_dagger_cohort_all <- rbind(e0_dagger_cohort_all, e0_dagger_cohort)
  i_cr=i_cr+1
  if(i_cr==length(Cohort_range)+1) break
}
e0_dagger_cohort_all[,2:6] <- num_matrix_function(e0_dagger_cohort_all[,2:6])
e0_dagger_cohort_all <- as.data.frame(e0_dagger_cohort_all)
colnames(e0_dagger_cohort_all) <- c("Country", "Cohort", "e0_f", "e0_m", "e_dagger_f", "e_dagger_m")

### Calculating Average
i_cr=1
average_e0_dagger_c<-c()
repeat {
  c_HMD=1
  average_m_mx_matrix_cohort<-average_f_mx_matrix_cohort<-c()
  repeat{
    Mx_cohort <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="cMx_1x1", username=HMD_account_name, password=HMD_account_pin, fixup=F)
    Cohort_mx_sub <- subset(Mx_cohort, Mx_cohort$Year== Cohort_range[i_cr])

    Cohort_mx_f_sub<-as.numeric(as.character(Cohort_mx_sub[,3]))
    Cohort_mx_m_sub<-as.numeric(as.character(Cohort_mx_sub[,4]))

    average_m_mx_matrix_cohort<-cbind(average_m_mx_matrix_cohort, Cohort_mx_m_sub)
    average_f_mx_matrix_cohort<-cbind(average_f_mx_matrix_cohort, Cohort_mx_f_sub)

    c_HMD=c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }
  average_f_e0_dagger_c_pre<-rowSums(average_f_mx_matrix_cohort, na.rm = T)/rowSums(!is.na(average_f_mx_matrix_cohort))
  average_m_e0_dagger_c_pre<-rowSums(average_m_mx_matrix_cohort, na.rm = T)/rowSums(!is.na(average_m_mx_matrix_cohort))

  e0_f<-LT_reconstruction_female(average_f_e0_dagger_c_pre)$ex[1]
  lx_f<-LT_reconstruction_female(average_f_e0_dagger_c_pre)$lx
  lx_f[lx_f<=0 | is.na(lx_f)]=1 ## for 0, negative, or NA value
  e0_dagger_f<- -sum(lx_f*log(lx_f))

  e0_m<-LT_reconstruction_male(average_m_e0_dagger_c_pre)$ex[1]
  lx_m<-LT_reconstruction_male(average_m_e0_dagger_c_pre)$lx
  lx_m[lx_m<=0 | is.na(lx_m)]=1 ## for 0, negative, or NA value
  e0_dagger_m<- -sum(lx_m*log(lx_m))

  average_e0_dagger_c_pre<-cbind(Cohort_range[i_cr], e0_f, e0_dagger_f, e0_m, e0_dagger_m )
  average_e0_dagger_c<-rbind(average_e0_dagger_c, average_e0_dagger_c_pre)

  i_cr=i_cr+1
  if(i_cr==length(Cohort_range)+1) break
}

average_e0_dagger_c <- data.frame(Country = rep("Average", nrow(average_e0_dagger_c)),
                                  Cohort = average_e0_dagger_c[,1],
                                  e0_f = average_e0_dagger_c[,2],
                                  e0_m = average_e0_dagger_c[,4],
                                  e_dagger_f = average_e0_dagger_c[,3],
                                  e_dagger_m = average_e0_dagger_c[,5])

e0_dagger_cohort_all<-rbind(e0_dagger_cohort_all, average_e0_dagger_c)

############################################
######  Preparing DATA for Period
############################################
Period_range<-seq(min(lx_all$Cohort), max(lx_all$Cohort))
i_pr=1
e0_dagger_period_all<-c()

repeat{
  c_HMD = 1
  e0_dagger_period <- c()
  repeat {
    Mx_period <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="Mx_1x1", username=HMD_account_name, password=HMD_account_pin, fixup=F)
    Period_mx_sub <- subset(Mx_period, Mx_period$Year== Period_range[i_pr])

    LT_p_f_sum_pre<-LT_reconstruction_female(mx=as.numeric(as.character(Period_mx_sub$Female)))
    LT_p_m_sum_pre<-LT_reconstruction_male(mx=as.numeric(as.character(Period_mx_sub$Male)))
    e0_f_sum_period<-LT_p_f_sum_pre$ex[1] ; e0_m_sum_period<-LT_p_m_sum_pre$ex[1]
    LT_p_f_sum_pre$lx[LT_p_f_sum_pre$lx==0]=1 ; LT_p_m_sum_pre$lx[LT_p_m_sum_pre$lx==0]=1
    e_f_dagger_period_l<-sum((LT_p_f_sum_pre$lx)*(-log(LT_p_f_sum_pre$lx)))
    e_m_dagger_period_l<-sum((LT_p_m_sum_pre$lx)*(-log(LT_p_m_sum_pre$lx)))

    e0_dagger_period <- rbind(e0_dagger_period, c(HMD_Names[c_HMD], Period_range[i_pr],
                                                  e0_f_sum_period, e0_m_sum_period, e_f_dagger_period_l, e_m_dagger_period_l))
    c_HMD=c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }
  e0_dagger_period_all <- rbind(e0_dagger_period_all, e0_dagger_period)
  i_pr=i_pr+1
  if(i_pr==length(Period_range)+1) break
}

e0_dagger_period_all[,2:6] <- num_matrix_function(e0_dagger_period_all[,2:6])
e0_dagger_period_all <- as.data.frame(e0_dagger_period_all)
colnames(e0_dagger_period_all) <- c("Country", "Period", "e0_f", "e0_m", "e_dagger_f", "e_dagger_m")

### Calculating Average
i_pr=1
average_e0_dagger_p<-c()
repeat {
  c_HMD=1
  average_m_mx_matrix_period<-average_f_mx_matrix_period<-c()
  repeat{
    Mx_period <- readHMDweb(CNTRY=HMD_Names[c_HMD],item="Mx_1x1", username=HMD_account_name, password=HMD_account_pin, fixup=F)
    Cohort_mx_sub <- subset(Mx_period, Mx_period$Year== Period_range[i_pr])

    Cohort_mx_f_sub<-as.numeric(as.character(Cohort_mx_sub[,3]))
    Cohort_mx_m_sub<-as.numeric(as.character(Cohort_mx_sub[,4]))

    average_m_mx_matrix_period<-cbind(average_m_mx_matrix_period, Cohort_mx_m_sub)
    average_f_mx_matrix_period<-cbind(average_f_mx_matrix_period, Cohort_mx_f_sub)

    c_HMD=c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }
  average_f_e0_dagger_p_pre<-rowSums(average_f_mx_matrix_period, na.rm = T)/rowSums(!is.na(average_f_mx_matrix_period))
  average_m_e0_dagger_p_pre<-rowSums(average_m_mx_matrix_period, na.rm = T)/rowSums(!is.na(average_m_mx_matrix_period))

  e0_f<-LT_reconstruction_female(average_f_e0_dagger_p_pre)$ex[1]
  lx_f<-LT_reconstruction_female(average_f_e0_dagger_p_pre)$lx
  lx_f[lx_f<=0 | is.na(lx_f)]=1 ## for 0, negative, or NA value
  e0_dagger_f<- -sum(lx_f*log(lx_f))

  e0_m<-LT_reconstruction_male(average_m_e0_dagger_p_pre)$ex[1]
  lx_m<-LT_reconstruction_male(average_m_e0_dagger_p_pre)$lx
  lx_m[lx_m<=0 | is.na(lx_m)]=1 ## for 0, negative, or NA value
  e0_dagger_m<- -sum(lx_m*log(lx_m))

  average_e0_dagger_p_pre<-cbind(Period_range[i_pr], e0_f, e0_dagger_f, e0_m, e0_dagger_m )
  average_e0_dagger_p<-rbind(average_e0_dagger_p, average_e0_dagger_p_pre)

  i_pr=i_pr+1
  if(i_pr==length(Period_range)+1) break
}

average_e0_dagger_p <- data.frame(Country = rep("Average", nrow(average_e0_dagger_p)),
                                  Period = average_e0_dagger_p[,1],
                                  e0_f = average_e0_dagger_p[,2],
                                  e0_m = average_e0_dagger_p[,4],
                                  e_dagger_f = average_e0_dagger_p[,3],
                                  e_dagger_m = average_e0_dagger_p[,5])

e0_dagger_period_all<-rbind(e0_dagger_period_all, average_e0_dagger_p)


############################################
######  Preparing DATA for CAL and CAL dagger
############################################
i_cal=1
cal_dagger_all<-c()
HMD_Names <- unique(lx_all$Country)

Year_seqence


repeat {
  summary_cal_dagger<-c()
  c_HMD=1
  repeat{
    lx_all_sub <- subset(lx_all, lx_all$Country==HMD_Names[c_HMD] & lx_all$Int_year == Year_seqence[i_cal])
    lx_all_sub$Female[lx_all_sub$Female<=0]=0 ; lx_all_sub$Male[lx_all_sub$Male<=0]=0
    cal_f<-sum(lx_all_sub$Female) ; cal_m<-sum(lx_all_sub$Male)
    lx_all_sub$Female[lx_all_sub$Female==0]=1 ; lx_all_sub$Male[lx_all_sub$Male==0]=1 # then log of x equals to 0
    cal_f_dagger<-sum(lx_all_sub$Female*(-log(lx_all_sub$Female)))  ;  cal_m_dagger<-sum(lx_all_sub$Male*(-log(lx_all_sub$Male)))

    summary_cal_dagger_pre<-cbind(HMD_Names[c_HMD], Year_seqence[i_cal], cal_f,cal_m,cal_f_dagger,cal_m_dagger)
    summary_cal_dagger<-rbind(summary_cal_dagger,summary_cal_dagger_pre)

    c_HMD = c_HMD+1
    if (c_HMD==length(HMD_Names)+1) break
  }
  cal_dagger_all<-rbind(cal_dagger_all, summary_cal_dagger)

  i_cal=i_cal+1
  if (i_cal==length(Year_seqence)+1) break
}
cal_dagger_all[,2:6] <- num_matrix_function(cal_dagger_all[,2:6])

cal_dagger_all<-as.data.frame(cal_dagger_all)
colnames(cal_dagger_all)[1:2] <- c("Country", "Interested_year")

############################################
######  SAVE DATA
############################################

## since px_all is too big and cannot be uploaded to github
## I separate it into two

px_all_1 <- px_all[1:(nrow(px_all)/2),]
px_all_2 <- px_all[((nrow(px_all)/2)+1):nrow(px_all) ,]

save(px_all_1, lx_all,        ## for CAL_dagger decomposition
     e0_dagger_cohort_all,    ## for cohort
     e0_dagger_period_all,    ## For Period
     cal_dagger_all,          ## For CAL
     file = "Data1.RData")

save(px_all_2, file = "Data2.RData") ## for CAL_dagger decomposition







