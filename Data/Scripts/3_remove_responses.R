########################################
# Project:  Nirajan Poudel MS Thesis
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     2_remove_responses.R
# Date:     2023 Summer
# About:    Checks survey data and removes potentially problematic responses
########################################

# Major edits
# 2023-08-08 PS created from NP script "Get_removed ID.R"
# 2023-08-10 PS updated: new removal criteria, new flags

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library()

########################################
# Load data

# Load processed data
Data <- readRDS(file.path("Data", "3_Processed", "Data.rds"))
DataCols <- readRDS(file.path("Data", "3_Processed", "DataCols.rds"))

# Backup
xData <- Data
# Data <- xData

# Initialize final dataset
temp <- Data

# Initialize exclusion and flagging dataframe
flags <- data.frame(ResponseId=Data$ResponseId)

# Identify choice columns
tq <- c()

for (i in 1 : 5) {
  for (j in 1 : 10) {
    tq <- c(tq, paste0("M", i, "Q", j))
  }; rm(j)
}; rm(i)

########################################
# Exclusion criteria, set 1: Completed survey

# 1. Did not finish survey
# Progress / Finished
table(Data$Progress); table(is.na(Data$Progress))
table(Data$Finished); table(is.na(Data$Progress))

# - exclude if < 100 / FALSE
table(Data$Progress < 100, Data$Finished == F)
flags$EXCLUDE1 <- (Data$Finished == F)
summary(flags$EXCLUDE1) # TRUE = 537, FALSE = 1795

# 2. Not acquired through main channel
# Status / DistributionChannel
table(Data$Status); table(is.na(Data$Status))
table(Data$DistributionChannel); table(is.na(Data$DistributionChannel))

# - exclude if {Spam, Survey Preview} / preview
table(Data$Status %in% c("Spam", "Survey Preview"), 
      Data$DistributionChannel == "preview")

flags$EXCLUDE2 <- (Data$Status %in% c("Spam", "Survey Preview"))
summary(flags$EXCLUDE2) # TRUE = 25, FALSE = 2307

# 3. Did not pass inclusion criteria
# ACCEPT
table(Data$ACCEPT); table(is.na(Data$ACCEPT))

# - exclude if Decline
# AGE1
table(Data$AGE1); table(is.na(Data$AGE1))

# - exclude if <18
# USRESID
table(Data$USRESID); table(is.na(Data$USRESID))

# - exclude if Outside of United States of America.
# COMMUTE
table(Data$COMMUTE); table(is.na(Data$COMMUTE))

# - exclude if No
table(Data$ACCEPT == "Decline")
table(Data$AGE1 < 18)
table(Data$USRESID == "Outside of United States of America.")
table(Data$COMMUTE == "No")
flags$EXCLUDE3 <- ((!is.na(Data$ACCEPT) & Data$ACCEPT == "Decline") | 
                   (!is.na(Data$AGE1) & Data$AGE1 < 18) | 
                   (!is.na(Data$USRESID) & 
                      Data$USRESID == "Outside of United States of America.") | 
                   (!is.na(Data$COMMUTE) & Data$COMMUTE == "No"))
summary(flags$EXCLUDE3) # TRUE = 109, FALSE = 2223

# 4. Qualtrics flags
# term
table(Data$term); table(is.na(Data$term))

# - exclude if failed inclusion questions, or if speeder
# - okay to keep "OQ" = over-quota, because the quota 
#   was reset each survey round due to poor-quality responses
table(Data$term %in% c("Q1.2_Consent","Q1.4_Age", 
                       "Q1.5_NotUS", "Q1.6_NotCommuting", "Speeder"))

flags$EXCLUDE4 <- (!is.na(Data$term) & 
                  Data$term %in% c("Q1.2_Consent",
                                   "Q1.4_Age", 
                                   "Q1.5_NotUS",
                                   "Q1.6_NotCommuting",
                                   "Speeder"))

summary(flags$EXCLUDE4) # TRUE = 161, FALSE = 2171

# 5. Did not complete any choice experiments
table(rowSums(!is.na(Data[, tq])))

# - exclude if answered 0 choice situation questions
table(rowSums(!is.na(Data[, tq])) == 0)
flags$EXCLUDE5 <- (rowSums(!is.na(Data[, tq])) == 0)
summary(flags$EXCLUDE5) # TRUE = 1441, FALSE = 891

# Apply exclusion criteria
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE1 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE2 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE3 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE4 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE5 == F], ]
# 809 responses remain

########################################
# Exclusion criteria, set 2: Reasonable responses

# 6. Work time (per day) is too short or too long
# WTPD
summary(Data$WTPD / 60)
table(round(Data$WTPD / 60, 2))
quantile(Data$WTPD / 60, seq(0, 1, 0.05), na.rm=T)
head(sort(Data$WTPD / 60, decreasing=F), 25)
head(sort(Data$WTPD / 60, decreasing=T), 25)

# - exclude if < 1 hr or > 16 hr
table(Data$WTPD/60 < 1 | Data$WTPD/60 > 16)
flags$EXCLUDE6 <- (!is.na(Data$WTPD) & (Data$WTPD/60 < 1 | Data$WTPD/60 > 16))
summary(flags$EXCLUDE6) # TRUE = 21, FALSE = 2311

# 7. Income (per day) is too low or too high
# INPD
summary(Data$INPD)
table(Data$INPD)
quantile(Data$INPD, seq(0, 1, 0.05), na.rm=T)
head(sort(Data$INPD, decreasing=F), 25)
head(sort(Data$INPD, decreasing=T), 25)
 
# - exclude if < $5 or > $10000
table(Data$INPD < 5 | Data$INPD > 10000)
flags$EXCLUDE7 <- (!is.na(Data$INPD) & (Data$INPD < 5 | Data$INPD > 10000))
summary(flags$EXCLUDE7) # TRUE = 14, FALSE = 2318

# 8. Travel time (per day) is too short or too long
#TTPD
summary(Data$TTPD)
table(Data$TTPD)
quantile(Data$TTPD, seq(0, 1, 0.05), na.rm=T)
head(sort(Data$TTPD, decreasing=F), 25)
head(sort(Data$TTPD, decreasing=T), 25)

# - exclude if < 10 min or > 240 min
table(Data$TTPD < 10 | Data$TTPD > 240)
flags$EXCLUDE8 <- (!is.na(Data$TTPD) & (Data$TTPD < 10 | Data$TTPD > 240))
summary(flags$EXCLUDE8) # TRUE = 37, FALSE = 2318

# 9. Travel cost (per day) is too high
#TCPD
summary(Data$TCPD)
table(Data$TCPD)
quantile(Data$TCPD, seq(0, 1, 0.05), na.rm=T)
head(sort(Data$TCPD, decreasing=T), 25)

# - exclude if > $200
table(Data$TCPD > 200)
flags$EXCLUDE9 <- (!is.na(Data$TCPD) & (Data$TCPD > 200))
summary(flags$EXCLUDE9) # TRUE = 19, FALSE = 2313

# 10. Completed only 1-9 choice experiments
table(rowSums(!is.na(Data[, tq])))

# - exclude if answered 0 choice situation questions
table(rowSums(!is.na(Data[, tq])) %in% 1 : 9)
flags$EXCLUDE10 <- (rowSums(!is.na(Data[, tq])) %in% 1 : 9)
summary(flags$EXCLUDE10) # TRUE = 57, FALSE = 2275

# 11. Straight-lined choices: all A or all B
b <- as.matrix(Data[, tq])

# convert choices to numbers
b[b == "A"] <- 1
b[b == "B"] <- 10
b[b == "C"] <- 100
b <- as.data.frame(apply(b, 2, FUN=as.numeric))
table(rowSums(b, na.rm=T))

# - exclude if answered all A (10) or all B (100)
table(rowSums(b, na.rm=T) %in% c(10, 100))
flags$EXCLUDE11 <- (rowSums(b, na.rm=T) %in% c(10, 100))
summary(flags$EXCLUDE11) # TRUE = 8, FALSE = 2324

# Apply exclusion criteria
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE6 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE7 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE8 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE9 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE10 == F], ]
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE11 == F], ]
# 733 responses remain

########################################
# Flagging criteria: Reasonable responses

# 1. Timing
# A. Short or long duration to complete survey
# ExpTime / Q_TotalDuration
summary(temp$ExpTime / 60); summary(temp$Q_TotalDuration/60)
quantile(temp$ExpTime / 60, seq(0, 1, 0.05))
head(sort(temp$ExpTime / 60, decreasing=F), 25)
head(sort(temp$ExpTime / 60, decreasing=T), 25)

# - flag if < 5 min or > 60 min
table(temp$ExpTime / 60 < 5 | temp$ExpTime / 60 > 60) # TRUE = 11, FALSE = 722
table(Data$ExpTime /60 < 5 | Data$ExpTime / 60 > 60)
flags$FLAG1A <- (Data$ExpTime / 60 < 5 | Data$ExpTime / 60 > 60)
summary(flags$FLAG1A) # TRUE = 1248, FALSE = 1084

# B. Short or long duration to complete experiment questions
# Last Click
c <- Data[, names(Data)[grepl(".*Last Click", names(Data))]]
c <- c[, 2 : 51]
summary(rowSums(c, na.rm=T) / 60)
d <- temp[, names(temp)[grepl(".*Last Click", names(temp))]]
d <- d[, 2 : 51]
summary(rowSums(d, na.rm=T) / 60)
quantile(rowSums(d, na.rm=T) / 60, seq(0, 1, 0.05))
head(sort(rowSums(d, na.rm=T) / 60, decreasing=F), 25)
head(sort(rowSums(d, na.rm=T) / 60, decreasing=T), 25)

# - flag if < 45 sec or > 20 min
table(rowSums(d, na.rm=T) < 45 | rowSums(d, na.rm=T) / 60 > 20) # TRUE = 9, FALSE = 724
table(rowSums(c, na.rm=T) < 45 | rowSums(c, na.rm=T) / 60 > 20)
flags$FLAG1B <- (rowSums(c, na.rm=T) < 45 | rowSums(c, na.rm=T) / 60 > 20)
summary(flags$FLAG1B) # TRUE = 1476, FALSE = 856
rm(c, d)

# combined flag
flags$FLAG1 <- (flags$FLAG1A | flags$FLAG1B)
summary(flags$FLAG1) # TRUE = 1524, FALSE = 808
summary(flags$FLAG1[flags$ResponseId %in%
                      temp$ResponseId]) # TRUE = 19, FALSE = 714

# 2. Almost straight-line responses on experiment
table(rowSums(b, na.rm=T))

# - flag if 19, 91, 109, 190 (9 of 10 either A or B)
#   or if 1000 (straight-line Current)
table(rowSums(b, na.rm=T) %in% c(19, 91, 109, 190, 1000))
flags$FLAG2 <- (rowSums(b, na.rm=T) %in% c(19, 91, 109, 190, 1000))
summary(flags$FLAG2) # TRUE = 49, FALSE = 2283
summary(flags$FLAG2[flags$ResponseId %in% 
                      temp$ResponseId]) # TRUE = 44, FALSE = 689

# 3. Less-realistic values for attributes
# A. Work time
table(temp$WTPD / 60)
# - flag if < 2 or > 12
table(temp$WTPD / 60 < 2 | temp$WTPD / 60 > 12) # TRUE = 22, FALSE = 711
table(Data$WTPD / 60 < 2 | Data$WTPD / 60 > 12)
flags$FLAG3A <- (!is.na(Data$WTPD) & 
                   (Data$WTPD / 60 < 2 | Data$WTPD / 60 > 12))

summary(flags$FLAG3A) # TRUE = 53, FALSE = 2279

# B. Income
table(temp$INPD)

# - flag if < $15 or > $4000
table(temp$INPD < 15 | temp$INPD > 4000) # TRUE = 14, FALSE = 719
table(Data$INPD < 15 | Data$INPD > 4000)
flags$FLAG3B <- (!is.na(Data$INPD) & (Data$INPD < 15 | Data$INPD > 4000))
summary(flags$FLAG3B) # TRUE = 39, FALSE = 2293

# C. Travel time
table(temp$TTPD)

# - no flags
flags$FLAG3C <- F

# D. Travel cost
table(temp$TCPD)

# - flag if > $100
table(temp$TCPD > 100) # TRUE = 3, FALSE = 730
table(Data$TCPD > 100)
flags$FLAG3D <- (!is.na(Data$TCPD) & (Data$TCPD > 100))
summary(flags$FLAG3D) # TRUE = 26, FALSE = 2306

# E. More daily travel cost than daily income
summary(temp$TCPD); summary(temp$INPD)

# - flag if TCPD > INPD
table(temp$TCPD > temp$INPD) # TRUE = 2, FALSE = 731
table(Data$TCPD > Data$INPD)
flags$FLAG3E <- (!is.na(Data$TCPD) & 
                   !is.na(Data$INPD) &
                   (Data$TCPD > Data$INPD))
summary(flags$FLAG3E) # TRUE = 29, FALSE = 2303

# F. Implied wage rate is very high or very low
summary(temp$WTPD / 60); summary(temp$INPD)
summary(temp$INPD / (temp$WTPD / 60))
quantile(temp$INPD / (temp$WTPD / 60), seq(0, 1, 0.05))
head(sort(temp$INPD / (temp$WTPD / 60), decreasing=F), 25)
head(sort(temp$INPD / (temp$WTPD / 60), decreasing=T), 25)

# - flag if < $4/hr or > $500/hr
table((temp$INPD / (temp$WTPD / 60) < 4) | 
      (temp$INPD / (temp$WTPD / 60) > 500)) # TRUE = 21, FALSE = 712

table((Data$INPD / (Data$WTPD / 60) < 4) | (Data$INPD / (Data$WTPD/60) > 500))
flags$FLAG3F <- (!is.na(Data$INPD) & !is.na(Data$WTPD) & 
                 ((Data$INPD / (Data$WTPD / 60) < 4) |
                    (Data$INPD / (Data$WTPD / 60) > 500)))

summary(flags$FLAG3F) # TRUE = 63, FALSE = 2269

# combined flag
flags$FLAG3 <- (flags$FLAG3A |
                  flags$FLAG3B |
                  flags$FLAG3C |
                  flags$FLAG3D |
                  flags$FLAG3E |
                  flags$FLAG3F)

summary(flags$FLAG3) # TRUE = 117, FALSE = 2215
summary(flags$FLAG3[flags$ResponseId %in%
                      temp$ResponseId]) # TRUE = 44, FALSE = 689

# 4. Inconsistencies among other variables
# A. More weekly commute days than weekly work days
# CDAYS, WDAYS
table(temp$CDAYS); table(temp$WDAYS)
table(temp$CDAYS, temp$WDAYS)

# - flag if CDAYS > WDAYS
table(temp$CDAYS > temp$WDAYS) # TRUE = 0, FALSE = 733
table(Data$CDAYS > Data$WDAYS)
flags$FLAG4A <- (!is.na(Data$CDAYS) &
                   !is.na(Data$WDAYS) &
                   (Data$CDAYS > Data$WDAYS))

summary(flags$FLAG4A) # TRUE = 1, FALSE = 2331

# B. Age reported at start differs from age bin selected at end
# AGE1, AGE2
table(Data$AGE1); table(Data$AGE2)
Data$AGECAT <- cut(Data$AGE1, breaks=c(18, 30, 45, 60, 80, 100), right=F)
levels(Data$AGECAT) <- levels(Data$AGE2)
table(Data$AGE1, Data$AGECAT)
table(Data$AGECAT, Data$AGE2)

# - flag if AGECAT not equal to AGE2
table(Data$AGECAT != Data$AGE2)
flags$FLAG4B <- (!is.na(Data$AGECAT) &
                   !is.na(Data$AGE2) &
                   (Data$AGECAT != Data$AGE2))

summary(flags$FLAG4B) # TRUE = 28, FALSE = 2304
summary(flags$FLAG4B[flags$ResponseId %in% 
                       temp$ResponseId]) # TRUE = 19, FALSE = 714
Data$AGECAT <- NULL

# C. More household workers than household adults
# HHADULTS, HHWORKERS
table(temp$HHADULTS); table(temp$HHWORKERS)
table(temp$HHADULTS, temp$HHWORKERS)

# - flag if HHADULT = 0 or > 10
#   and if  HHWORKERS = 0 or > 10
#   and if  HHWORKERS > HHADULT
table((temp$HHADULTS==0 | temp$HHADULTS > 10) | 
      (temp$HHWORKERS==0 | temp$HHWORKERS > 10) | 
      (temp$HHWORKERS > temp$HHADULTS)) # TRUE = 29, FALSE = 689

table((Data$HHADULTS==0 | Data$HHADULTS > 10) | 
      (Data$HHWORKERS==0 | Data$HHWORKERS > 10) | 
      (Data$HHWORKERS > Data$HHADULTS))

flags$FLAG4C <- (!is.na(Data$HHADULTS) & !is.na(Data$HHWORKERS) & 
                 ((Data$HHADULTS==0 | Data$HHADULTS > 10) | 
                  (Data$HHWORKERS==0 | Data$HHWORKERS > 10) | 
                  (Data$HHWORKERS > Data$HHADULTS)))

summary(flags$FLAG4C) # TRUE = 38, FALSE = 2294
summary(flags$FLAG4C[flags$ResponseId %in%
                       temp$ResponseId]) # TRUE = 29, FALSE = 704

# D. More personal income than household income
# INPD, HHINC
Data$PERINC <- Data$INPD * Data$WDAYS * 52 # calculate yearly income from daily
summary(Data$PERINC); summary(Data$HHINC)
Data$PERINCCAT <- cut(Data$PERINC,
                      breaks=c(0, 15000, 50000,
                               100000, 150000,
                               1000000000), right=F)

levels(Data$PERINCCAT) <- levels(Data$HHINC)[1 : 5]
table(round(Data$PERINC, -3), Data$PERINCCAT)
table(Data$PERINCCAT, Data$HHINC)

# - flag if PERINCCAT > HHINC
table(as.integer(Data$PERINCCAT) > as.integer(Data$HHINC))
flags$FLAG4D <- (!is.na(Data$PERINCCAT) & !is.na(Data$HHINC) & 
                 (as.integer(Data$PERINCCAT) > as.integer(Data$HHINC)))

summary(flags$FLAG4D) # TRUE = 91, FALSE = 2241
summary(flags$FLAG4D[flags$ResponseId %in% 
                       temp$ResponseId]) # TRUE = 65, FALSE = 668

Data$PERINC <- NULL
Data$PERINCCAT <- NULL

# combined flag
flags$FLAG4 <- (flags$FLAG4A | flags$FLAG4B | flags$FLAG4C | flags$FLAG4D)
summary(flags$FLAG4) # TRUE = 148, FALSE = 2184
summary(flags$FLAG4[flags$ResponseId %in%
                      temp$ResponseId]) # TRUE = 107, FALSE = 626

# Combined flag
flags$FLAGS <- rowSums(flags[, c("FLAG1", "FLAG2", "FLAG3", "FLAG4")])
table(flags$FLAGS) # 0 = 592, 1 = 1658, 2 = 66, 3 = 16
table(flags$FLAGS[flags$ResponseId %in%
                    temp$ResponseId]) # 0 = 550, 1 = 159, 2 = 17, 3 = 7

# - exclude if FLAGS >= 2
table(flags$FLAGS >= 2)
flags$EXCLUDE12 <- (flags$FLAGS >= 2)
summary(flags$EXCLUDE12) # TRUE = 82, FALSE = 2250
summary(flags$EXCLUDE12[flags$ResponseId %in%
                          temp$ResponseId]) # TRUE = 24, FALSE = 709

# Apply exclusion criteria
temp <- temp[temp$ResponseId %in% flags$ResponseId[flags$EXCLUDE12 == F], ]
# 709 responses remain

########################################
# Save data

# Save
saveRDS(flags, file.path("Data", "3_Processed", "flags.rds"))
write.csv(flags, file.path("Data", "3_Processed", "flags.csv"), row.names=F)

########################################
# Clean up

# Remove
rm(Data, DataCols, xData)
rm(b, tq, flags, temp)
gc()

########################################
# END
########################################