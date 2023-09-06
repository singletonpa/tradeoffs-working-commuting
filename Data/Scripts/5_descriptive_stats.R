########################################
# Project:  Understanding tradeoffs between working and commuting
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Niranjan Poudel (niranjan111@hotmail.com)
# File:     5_descriptive_stats.R
# Date:     2023 Summer
# About:    Calculates descriptive statistics from wide and long datasets
########################################

# Major edits
# 2023-08-11 PS created from NP script "Code.R"

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library(tidyverse)
# library(fastDummies)

########################################
# Load data

# Load processed data
dw <- readRDS(file.path("Data", "4_Final", "DataWide.rds"))
dl <- readRDS(file.path("Data", "4_Final", "DataLong.rds"))

# Backup
xdw <- dw
xdl <- dl
# dw <- xdw
# dl <- xdl

########################################
# Select columns to describe

# Inspect
summary(dl)
summary(dw[1:which(names(dw)=="IDEALTT_15_UP")])

# Choices from experiment
cols0 <- c("value")

# Attributes of alternatives
dw$TCPDN0 <- ifelse(dw$TCPD == 0, NA, dw$TCPD)
dw$WTPDHR <- dw$WTPD / 60
dw$wagerate <- dw$INPD / dw$WTPDHR
cols1 <- c("TTPD", "TCPD", "TCPDN0", "WTPDHR", "INPD", "wagerate")

# Explanatory variables
cols2 <- c("AGE1", "AGE2_18_29", "AGE2_30_44", "AGE2_45_59", "AGE2_60_UP", 
           "GEND_Male", "GEND_Female", "RACE_White", "RACE_Other", 
           "EDUC_L_HS", "EDUC_Bach", "EDUC_Mast", 
           "HHADULTS", "HHKIDS" , "HHWORKERS", 
           "HHINC_000_050", "HHINC_050_100", "HHINC_100__UP", 
           "PLACETYPE_Urban", "PLACETYPE_Sub_Urban", 
           "PLACETYPE_Suburban", "PLACETYPE_Rural_all", 
           "WDAYS", "CDAYS", "COMMUTE_YesCurrently", "COMMUTE_NoUsedTo", 
           "MODE_AutoD", "MODE_AutoP", "MODE_Transit", 
           "MODE_Walk", "MODE_Bike", 
           "PARKCOST_Yes", "PARKCOST_No", "TOLLCOST_Yes", "TOLLCOST_No", 
           "TIMEDEP_Morning", "TIMEDEP_AfterEveNight", 
           "FLEXWORK_Yes", "FLEXWORK_No", "FLEXINC_Yes", "FLEXINC_No", 
           "PRIMWORK_Yes", "PRIMWORK_No", 
           "IMP_TT", "IMP_TC", "IMP_WT", "IMP_IN", 
           "SAT_WORK", "SAT_COMM", "ENJOYWC_Work", "ENJOYWC_Comm", 
           "IDEALTT_00___", "IDEALTT_01_05", "IDEALTT_06_15", "IDEALTT_15_UP")

########################################
# Calculate descriptive statistics

# Correlations among explanatory variables
mycor <- cor(dw[,cols2], use="pairwise.complete.obs", method="pearson")
mycor

# Frequency and proportions of choices from experiment
df0 <- data.frame(Var=c("C", "A", "B"), 
                  Freq=as.vector(table(dl[, cols0])), 
                  Prop=as.vector(prop.table(table(dl[, cols0]))))
df0

# Descriptive statistics for attributes of alternatives
# initialize
df1 <- data.frame(Var=cols1)
df1[, c("Mean", "SD", "Min", "PCT5", "PCT50", "PCT95", "Max")] <- NA
# calculate
for (i in 1:nrow(df1)) {
  df1$Mean[i] <- mean(dw[, df1$Var[i]], na.rm=T)
  df1$SD[i] <- sd(dw[, df1$Var[i]], na.rm=T)
  df1$Min[i] <- min(dw[, df1$Var[i]], na.rm=T)
  df1$PCT5[i] <- quantile(dw[, df1$Var[i]], 0.05, na.rm=T)
  df1$PCT50[i] <- quantile(dw[, df1$Var[i]], 0.50, na.rm=T)
  df1$PCT95[i] <- quantile(dw[, df1$Var[i]], 0.95, na.rm=T)
  df1$Max[i] <- max(dw[, df1$Var[i]], na.rm=T)
}; rm(i)
# inspect
df1

# Descriptive statistics for explanatory variables
# initialize
df2 <- data.frame(Var=cols2)
df2[,c("Freq", "Perc", "Mean", "SD")] <- NA

# calculate
for (i in 1:nrow(df2)) {
  df2$Freq[i] <- sum(dw[, df2$Var[i]] == 1, na.rm=T)
  df2$Perc[i] <- df2$Freq[i] / sum(!is.na(dw[, df2$Var[i]]))
  df2$Mean[i] <- mean(dw[, df2$Var[i]], na.rm=T)
  df2$SD[i] <- sd(dw[, df2$Var[i]], na.rm=T)
  if (df2$Perc[i] == df2$Mean[i]) {
    df2$Mean[i] <- NA
    df2$SD[i] <- NA
  } else {
    df2$Freq[i] <- NA
    df2$Perc[i] <- NA
  }
}; rm(i)
# inspect
df2

########################################
# Save descriptive statistics

# Save
write.csv(mycor, file.path("Data", "Descriptives", "corr_vars.csv"), 
          row.names=T)
write.csv(df0, file.path("Data", "Descriptives", "desc_alts.csv"), 
          row.names=F)
write.csv(df1, file.path("Data", "Descriptives", "desc_atts.csv"), 
          row.names=F)
write.csv(df2, file.path("Data", "Descriptives", "desc_vars.csv"), 
          row.names=F)

########################################
# Clean up

# Remove
rm(dw, dl, xdw, xdl)
rm(cols0, cols1, cols2)
rm(mycor, df0, df1, df2)
gc()

########################################
# END
########################################