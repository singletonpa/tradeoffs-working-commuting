########################################
# Project:  Understanding tradeoffs between working and commuting
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Niranjan Poudel (niranjan111@hotmail.com)
# File:     4_process_final_data.R
# Date:     2023 Summer
# About:    Formats and prepares final wide and long datasets
########################################

# Major edits
# 2023-08-08 PS created from NP script "Final correct data.R"
# 2023-08-11 PS updated: read from new dataset, process data, create wide/long

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library(tidyverse)
library(fastDummies)

########################################
# Load data

# Load processed data
Data <- readRDS(file.path("Data", "3_Processed", "Data.rds"))
DataCols <- readRDS(file.path("Data", "3_Processed", "DataCols.rds"))
flags <- readRDS(file.path("Data", "3_Processed", "flags.rds"))

# Backup
xData <- Data
# Data <- xData

########################################
# Apply exclusion criteria

# Note: see "3_remove_responses.R" script for details

# All responses
nrow(Data) # 2332 responses to start

# Exclusion criteria, set 1: Completed survey
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE1 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE2 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE3 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE4 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE5 == F],]
nrow(Data) # 809 responses remain

# Exclusion criteria, set 2: Reasonable responses
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE6 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE7 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE8 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE9 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE10 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE11 == F],]
Data <- Data[Data$ResponseId %in% flags$ResponseId[flags$EXCLUDE12 == F],]
nrow(Data) # 709 responses remain

########################################
# Create wide dataset

# Initialize wide dataset
# identifiers
dw <- data.frame(ID=0L, ResponseId=Data$ResponseId, DurationSec=Data$ExpTime)
dw$ID <- 1L : nrow(dw)

# Add personal characteristics
dw <- cbind(dw, Data[, c("AGE1", "AGE2", "GEND", "RACE", "EDUC")])
levels(dw$AGE2) <- c("18_29", "30_44", "45_59", "60_UP", "60_UP")
# dw$AGE2 <- relevel(dw$AGE2, ref="30_44")
dw$GEND <- droplevels(dw$GEND) # only Male and Female
dw$RACE <- ifelse(is.na(dw$RACE), NA, 
           ifelse(dw$RACE == "White", "White", "Other"))
dw$RACE <- factor(dw$RACE, levels=c("White", "Other"))
levels(dw$EDUC) <- c("L_HS", "L_HS", "Bach", "Mast", NA)
# dw$EDUC <- relevel(dw$EDUC, ref="Bach")
dw <- dummy_cols(dw, c("AGE2", "GEND", "RACE", "EDUC"),
                 ignore_na=T, remove_selected_columns=T)

# Add household characteristics
dw <- cbind(dw, Data[,c("HHINC", "HHADULTS", "HHKIDS", "HHWORKERS")])
levels(dw$HHINC) <- c("000_050", "000_050", "050_100", "100__UP", "100__UP", NA)
# dw$HHINC <- relevel(dw$HHINC, ref="050_100")
dw$HHADULTS <- pmax(0L, dw$HHADULTS - 1L) # number of other adults
dw$HHWORKERS <- pmax(0L, dw$HHWORKERS - 1L) # number of other workers
dw$HHWORKERS[dw$HHWORKERS > 10] <- NA # erroneous responses
dw <- dummy_cols(dw, c("HHINC"), ignore_na=T, remove_selected_columns=T)

# Add location characteristics
dw <- cbind(dw, Data[, c("PLACE_LIVE", "PLACE_WORK")])
table(dw$PLACE_LIVE, dw$PLACE_WORK)
table(is.na(dw$PLACE_LIVE), is.na(dw$PLACE_WORK))
dw$PLACETYPE <- ifelse(is.na(dw$PLACE_LIVE) | is.na(dw$PLACE_WORK), NA, 
                ifelse(dw$PLACE_LIVE=="Rural" | dw$PLACE_WORK=="Rural", 
                       "Rural_all", 
                ifelse(dw$PLACE_LIVE=="Suburban" & dw$PLACE_WORK=="Suburban", 
                       "Suburban", 
                ifelse(dw$PLACE_LIVE=="Suburban" & dw$PLACE_WORK=="Urban", 
                       "Sub_Urban", 
                ifelse(dw$PLACE_LIVE=="Urban" & dw$PLACE_WORK=="Suburban", 
                       "Sub_Urban", 
                ifelse(dw$PLACE_LIVE=="Urban" & dw$PLACE_WORK=="Urban", 
                       "Urban", NA))))))
table(dw$PLACE_LIVE, dw$PLACE_WORK, dw$PLACETYPE)
dw$PLACETYPE <- factor(dw$PLACETYPE, 
                       levels=c("Urban", "Sub_Urban", "Suburban", "Rural_all"))
dw$PLACE_LIVE <- NULL
dw$PLACE_WORK <- NULL
dw <- dummy_cols(dw, c("PLACETYPE"), ignore_na=T, remove_selected_columns=T)

# Add travel and work characteristics
dw <- cbind(dw, Data[,c("COMMUTE", "WDAYS", "CDAYS", "MODE", 
                        "PARKCOST", "TOLLCOST",
                        "TIMEDEP", "FLEXWORK", "FLEXINC", "PRIMWORK")])
dw$COMMUTE <- droplevels(dw$COMMUTE)
dw$MODE <- droplevels(dw$MODE)
# dw$PARKCOST <- relevel(dw$PARKCOST, ref="No")
# dw$TOLLCOST <- relevel(dw$TOLLCOST, ref="No")
dw$TIMEDEP <- format(dw$TIMEDEP, format="%H", tz="America/Denver") # to hours
dw$TIMEDEP <- ifelse(is.na(dw$TIMEDEP), NA, 
              ifelse(dw$TIMEDEP %in% c("05", "06", "07", 
                                       "08", "09", "10", "11"), 
                     "Morning", "AfterEveNight"))
dw$TIMEDEP <- factor(dw$TIMEDEP, levels=c("Morning", "AfterEveNight"))
dw <- dummy_cols(dw, c("COMMUTE", "MODE", "PARKCOST", "TOLLCOST", 
                       "TIMEDEP", "FLEXWORK", "FLEXINC", "PRIMWORK"), 
                 ignore_na=T, remove_selected_columns=T)

# Add subjective responses
dw <- cbind(dw, Data[, c("IMP_TT", "IMP_TC", "IMP_WT", "IMP_IN", 
                        "SAT_WORK", "SAT_COMM", "ENJOYWC", "IDEALTT")])
dw$IMP_TT <- as.integer(dw$IMP_TT)-3L
dw$IMP_TC <- as.integer(dw$IMP_TC)-3L
dw$IMP_WT <- as.integer(dw$IMP_WT)-3L
dw$IMP_IN <- as.integer(dw$IMP_IN)-3L
dw$SAT_WORK <- as.integer(dw$SAT_WORK)-3L
dw$SAT_COMM <- as.integer(dw$SAT_COMM)-3L
levels(dw$ENJOYWC) <- c("Work", "Comm")
levels(dw$IDEALTT) <- c("00___", "01_05", "06_15", "15_UP")
# dw$IDEALTT <- relevel(dw$IDEALTT, ref="6 to 15 minutes")
dw <- dummy_cols(dw, c("ENJOYWC", "IDEALTT"), 
                 ignore_na=T, remove_selected_columns=T)

# Add choices from experiment
tq <- c()
for (i in 1 : 5) {
  for (j in 1 : 10) {
    tq <- c(tq, paste0("M", i, "Q", j))
  }; rm(j)
}; rm(i)
dw <- cbind(dw, Data[, tq])

# Add travel and work attributes
# self-reported
dw <- cbind(dw, Data[, c("TTPD", "TCPD", "WTPD", "INPD")])
# experimental 
dw <- cbind(dw, Data[, which(names(Data) == "MD12TT1") 
                      :which(names(Data)=="MD345WM5")])

# Inspect
str(dw, list.len=ncol(dw))
summary(dw[1 : which(names(dw) == "IDEALTT_15_UP")])

########################################
# Create long dataset

# Initialize long dataset
dl <- pivot_longer(dw, M1Q1 : M5Q10)

# Add choice column
dl$choice <- ifelse(is.na(dl$value), NA, 
             ifelse(dl$value=="C", 1L, 
             ifelse(dl$value=="A", 2L, 
             ifelse(dl$value=="B", 3L, NA))))

# Add attributes: Current alternative
dl %>%
  mutate(TT_C = TTPD) %>%
  mutate(TC_C = TCPD) %>%
  mutate(WT_C = WTPD) %>%
  mutate(IN_C = INPD) -> dl

# Add attributes: Alternative A
# travel time
dl %>%
  mutate(TT_A = ifelse(name %in% c("M1Q4", "M1Q8", "M2Q3", "M2Q5", "M2Q10"), 
                       MD12TT1, 
                ifelse(name %in% c("M1Q3", "M1Q6", "M2Q2", "M2Q6"), 
                       MD12TT2, 
                ifelse(name %in% c("M1Q9", "M1Q10","M2Q8"), 
                       MD12TT3, 
                ifelse(name %in% c("M1Q2", "M1Q5", "M1Q7", "M2Q7", "M2Q9"), 
                       MD12TT4, 
                ifelse(name %in% c("M1Q1", "M2Q1", "M2Q4"), 
                       MD12TT5, 
                ifelse(name %in% c("M3Q3", "M3Q7", "M4Q1", "M4Q4", "M5Q3", 
                                   "M5Q7"), 
                       MD345TT1, 
                ifelse(name %in% c("M3Q6", "M3Q9", "M4Q2", "M5Q6", "M5Q9"), 
                       MD345TT2, 
                ifelse(name %in% c("M3Q2", "M3Q10","M4Q5", "M4Q9", "M5Q2", 
                                   "M5Q10"), 
                       MD345TT3, 
                ifelse(name %in% c("M3Q1", "M3Q4", "M4Q6", "M4Q7", "M4Q10",
                                   "M5Q1", "M5Q4"), 
                       MD345TT4, 
                ifelse(name %in% c("M3Q5", "M3Q8", "M4Q3", "M4Q8", "M5Q5", 
                                   "M5Q8"), 
                       MD345TT5, 
                       0))))))))))) -> dl
# travel cost
dl %>%
  mutate(TC_A = ifelse(name %in% c("M1Q2", "M1Q7", "M1Q10","M5Q4"), 
                       M15TC1, 
                ifelse(name %in% c("M1Q3", "M5Q7", "M5Q8"), 
                       M15TC2, 
                ifelse(name %in% c("M1Q6", "M1Q8", "M5Q2", "M5Q3"), 
                       M15TC3, 
                ifelse(name %in% c("M1Q1", "M1Q4", "M5Q5", "M5Q10"),
                       M15TC4, 
                ifelse(name %in% c("M1Q5", "M1Q9", "M5Q1", "M5Q6", "M5Q9"), 
                       M15TC5, 
                ifelse(name %in% c("M3Q4"), 
                       M3TC1, 
                ifelse(name %in% c("M3Q7", "M3Q8"), 
                       M3TC2, 
                ifelse(name %in% c("M3Q2", "M3Q3"), 
                       M3TC3, 
                ifelse(name %in% c("M3Q5", "M3Q10"),
                       M3TC4, 
                ifelse(name %in% c("M3Q1", "M3Q6", "M3Q9"), 
                       M3TC5, 
                       0))))))))))) -> dl
# work time
dl %>%
  mutate(WT_A = ifelse(name %in% c("M1Q3", "M1Q8", "M2Q3", "M2Q7", "M2Q8"), 
                       MD12WT1, 
                ifelse(name %in% c("M1Q5", "M1Q9", "M2Q5", "M2Q9"), 
                       MD12WT2, 
                ifelse(name %in% c("M1Q6", "M1Q7", "M2Q1", "M2Q4"), 
                       MD12WT3, 
                ifelse(name %in% c("M1Q4", "M2Q6", "M2Q10"), 
                       MD12WT4, 
                ifelse(name %in% c("M1Q1", "M1Q2", "M1Q10","M2Q2"), 
                       MD12WT5, 
                ifelse(name %in% c("M3Q10","M4Q9", "M5Q10"), 
                       MD345WT1, 
                ifelse(name %in% c("M3Q2", "M3Q3", "M3Q4", "M4Q5", "M5Q2", 
                                   "M5Q3", "M5Q4"), 
                       MD345WT2, 
                ifelse(name %in% c("M3Q8", "M3Q9", "M4Q4", "M5Q8", "M5Q9"), 
                       MD345WT3, 
                ifelse(name %in% c("M3Q5", "M3Q6", "M4Q2", "M4Q6", "M4Q7", 
                                   "M5Q5", "M5Q6"), 
                       MD345WT4, 
                ifelse(name %in% c("M3Q1", "M3Q7", "M4Q1", "M4Q3", "M4Q8", 
                                   "M4Q10","M5Q1", "M5Q7"), 
                       MD345WT5, 
                       0))))))))))) -> dl

# income
dl %>%
  mutate(IN_A = ifelse(name %in% c("M1Q3", "M1Q6", "M2Q5", "M2Q8"), 
                       MD12INC1, 
                ifelse(name %in% c("M1Q4", "M1Q8", "M2Q3", "M2Q7", "M2Q10"), 
                       MD12INC2, 
                ifelse(name %in% c("M1Q5", "M2Q2", "M2Q6"), 
                       MD12INC3, 
                ifelse(name %in% c("M1Q1", "M1Q9", "M2Q1", "M2Q4"), 
                       MD12INC4, 
                ifelse(name %in% c("M1Q2", "M1Q7", "M1Q10","M2Q9"), 
                       MD12INC5, 
                ifelse(name %in% c("M3Q2", "M3Q3", "M4Q4", "M4Q9", "M5Q2", 
                                   "M5Q3"), 
                       MD345INC1, 
                ifelse(name %in% c("M3Q4", "M3Q7", "M4Q5", "M5Q4", "M5Q7"), 
                       MD345INC2, 
                ifelse(name %in% c("M3Q10","M4Q1", "M4Q2", "M5Q10"), 
                       MD345INC3, 
                ifelse(name %in% c("M3Q5", "M3Q6", "M3Q9", "M4Q3", "M4Q6", 
                                   "M4Q8", "M5Q5", "M5Q6", "M5Q9"), 
                       MD345INC4, 
                ifelse(name %in% c("M3Q1", "M3Q8", "M4Q7", "M4Q10","M5Q1", 
                                   "M5Q8"), 
                       MD345INC5, 
                       0))))))))))) -> dl

# Add attributes: Alternative B
# travel time
dl %>%
  mutate(TT_B = ifelse(name %in% c("M1Q5", "M1Q7", "M2Q7", "M2Q9"), 
                       MD12TT1, 
                ifelse(name %in% c("M1Q3", "M1Q9", "M2Q2", "M2Q5"), 
                       MD12TT2, 
                ifelse(name %in% c("M1Q1", "M2Q1", "M2Q10"), 
                       MD12TT3, 
                ifelse(name %in% c("M1Q2", "M1Q4", "M1Q10","M2Q3", "M2Q6"), 
                       MD12TT4, 
                ifelse(name %in% c("M1Q6", "M1Q8", "M2Q4", "M2Q8"), 
                       MD12TT5, 
                ifelse(name %in% c("M3Q4", "M3Q5", "M4Q6", "M5Q4", "M5Q5"), 
                       MD345TT1, 
                ifelse(name %in% c("M3Q8", "M3Q10","M4Q3", "M4Q5", "M4Q8", 
                                   "M5Q8", "M5Q10"), 
                       MD345TT2, 
                ifelse(name %in% c("M3Q7", "M3Q9", "M4Q4", "M5Q7", "M5Q9"), 
                       MD345TT3, 
                ifelse(name %in% c("M3Q1", "M3Q6", "M4Q1", "M4Q7", "M5Q1", 
                                   "M5Q6"), 
                       MD345TT4, 
                ifelse(name %in% c("M3Q2", "M3Q3", "M4Q2", "M4Q9", "M4Q10",
                                   "M5Q2", "M5Q3"), 
                       MD345TT5, 
                       0))))))))))) -> dl
# travel cost
dl %>%
  mutate(TC_B = ifelse(name %in% c("M1Q5", "M1Q9", "M5Q6", "M5Q9", "M5Q10"), 
                       M15TC1, 
                ifelse(name %in% c("M1Q4", "M1Q8", "M5Q7"), 
                       M15TC2, 
                ifelse(name %in% c("M1Q1", "M5Q4", "M5Q5"), 
                       M15TC3, 
                ifelse(name %in% c("M1Q2", "M1Q3", "M1Q6", "M5Q1", "M5Q2"), 
                       M15TC4, 
                ifelse(name %in% c("M1Q7", "M1Q10","M5Q3", "M5Q8"), 
                       M15TC5, 
                ifelse(name %in% c("M3Q6", "M3Q9", "M3Q10"), 
                       M3TC1, 
                ifelse(name %in% c("M3Q7"), 
                       M3TC2, 
                ifelse(name %in% c("M3Q4", "M3Q5"), 
                       M3TC3, 
                ifelse(name %in% c("M3Q1", "M3Q2"), 
                       M3TC4, 
                ifelse(name %in% c("M3Q3", "M3Q8"), 
                       M3TC5, 
                       0))))))))))) -> dl
# work time
dl %>%
  mutate(WT_B = ifelse(name %in% c("M1Q1", "M1Q2", "M1Q4", "M2Q2", "M2Q10"), 
                       MD12WT1, 
                ifelse(name %in% c("M1Q10","M2Q4"), 
                       MD12WT2, 
                ifelse(name %in% c("M1Q5", "M1Q8", "M2Q6", "M2Q7"), 
                       MD12WT3, 
                ifelse(name %in% c("M1Q7", "M1Q9", "M2Q8", "M2Q9"), 
                       MD12WT4, 
                ifelse(name %in% c("M1Q3", "M1Q6", "M2Q1", "M2Q3", "M2Q5"), 
                       MD12WT5, 
                ifelse(name %in% c("M3Q1", "M3Q7", "M4Q4", "M4Q6", "M4Q7", 
                                   "M5Q1", "M5Q7"), 
                       MD345WT1, 
                ifelse(name %in% c("M3Q4", "M3Q10","M4Q1", "M4Q3", "M5Q4", 
                                   "M5Q10"), 
                       MD345WT2, 
                ifelse(name %in% c("M3Q5", "M4Q8", "M4Q9", "M5Q5"), 
                       MD345WT3, 
                ifelse(name %in% c("M3Q3", "M3Q6", "M3Q8", "M4Q5", "M4Q10",
                                   "M5Q3", "M5Q6", "M5Q8"), 
                       MD345WT4, 
                ifelse(name %in% c("M3Q2", "M3Q9", "M4Q2", "M5Q2", "M5Q9"), 
                       MD345WT5, 
                       0))))))))))) -> dl

# income
dl %>%
  mutate(IN_B = ifelse(name %in% c("M1Q1", "M2Q7"), 
                       MD12INC1, 
                ifelse(name %in% c("M1Q4", "M1Q5", "M2Q2", "M2Q10"), 
                       MD12INC2, 
                ifelse(name %in% c("M1Q3", "M1Q9", "M2Q4", "M2Q5"), 
                       MD12INC3, 
                ifelse(name %in% c("M1Q2", "M1Q6", "M1Q8", "M2Q6", "M2Q8"), 
                       MD12INC4, 
                ifelse(name %in% c("M1Q7", "M1Q10","M2Q1", "M2Q3", "M2Q9"), 
                       MD12INC5, 
                ifelse(name %in% c("M3Q4", "M3Q5", "M4Q6", "M4Q8", "M5Q4", 
                                   "M5Q5"), 
                       MD345INC1, 
                ifelse(name %in% c("M3Q7", "M3Q10","M4Q3", "M4Q4", "M5Q7", 
                                   "M5Q10"), 
                       MD345INC2, 
                ifelse(name %in% c("M3Q1", "M4Q1", "M4Q5", "M5Q1"), 
                       MD345INC3, 
                ifelse(name %in% c("M3Q6", "M3Q9", "M4Q2", "M4Q7", "M4Q9", 
                                   "M5Q6", "M5Q9"), 
                       MD345INC4, 
                ifelse(name %in% c("M3Q2", "M3Q3", "M3Q8", "M4Q10","M5Q2", 
                                   "M5Q3", "M5Q8"), 
                       MD345INC5, 
                       0))))))))))) -> dl

# Remove rows with no choices
dl %>% drop_na("value") -> dl

# Remove unnecessary columns
dl <- dl[, -c(which(names(dl) == "MD12TT1") : which(names(dl) == "MD345WM5"))]

# Inspect
str(dl, list.len=ncol(dl))
summary(dl)

########################################
# Final data processing

# For modeling, not using the following explanatory variables: 
# - b/c correlation is too high (> 0.60)
#   c("AGE1", "HHWORKERS")
# - b/c not asked for all people
#   c("FLEXINC_Yes", "FLEXINC_No")
# - b/c subjective question is distracting from study purpose
#   c("IMP_TT", "IMP_TC", "IMP_WT", "IMP_IN", 
#     "IDEALTT_00___", "IDEALTT_01_05", "IDEALTT_06_15", "IDEALTT_15_UP")

# Remove rows for missing on explanatory variables
# select columns
fac <- c("AGE2_18_29", "GEND_Female", "RACE_Other", "EDUC_L_HS", 
         "HHADULTS", "HHKIDS", "HHINC_000_050", "PLACETYPE_Sub_Urban", 
         "WDAYS", "CDAYS", "COMMUTE_NoUsedTo", "MODE_AutoP", 
         "PARKCOST_Yes", "TOLLCOST_Yes", "TIMEDEP_AfterEveNight", 
         "FLEXWORK_No", "PRIMWORK_No", "SAT_WORK", "SAT_COMM")

# check for missingness
mis <- dw$ResponseId[rowSums(is.na(dw[, fac]))>0] # 34 removed

# remove responses with missing
dw <- dw[!(dw$ResponseId %in% mis), ]
dl <- dl[!(dl$ResponseId %in% mis),]

# remove
rm(fac, mis)

# Inspect
nrow(dw) # 675 responses remain

########################################
# Save data

# Save
saveRDS(dw, file.path("Data", "4_Final", "DataWide.rds"))
write.csv(dw, file.path("Data", "4_Final", "DataWide.csv"), row.names=F)
saveRDS(dl, file.path("Data", "4_Final", "DataLong.rds"))
write.csv(dl, file.path("Data", "4_Final", "DataLong.csv"), row.names=F)

########################################
# Clean up

# Remove
rm(Data, DataCols, xData)
rm(tq, flags, dw, dl)
gc()

########################################
# END
########################################