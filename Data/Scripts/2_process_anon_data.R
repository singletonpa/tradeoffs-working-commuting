########################################
# Project:  Understanding tradeoffs between working and commuting
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Niranjan Poudel (niranjan111@hotmail.com)
# File:     2_process_anon_data.R
# Date:     2023 Summer
# About:    Processes anonymous data
########################################

# Major edits
# 2023-08-08 PS created from NP script "Final correct data.R"
# 2023-08-10 PS updated: read from new dataset, process columns types/names

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
# library()

########################################
# Load data

# Load anonymous data
Data <- readRDS(file.path("Data", "2_Anonymous", "Data.rds"))
DataCols <- readRDS(file.path("Data", "2_Anonymous", "DataCols.rds"))

# Backup
xData <- Data
# Data <- xData

########################################
# Process column types

# Select columns by type
c_logical <- c("Finished")
c_integer <- c("Progress", "Duration (in seconds)",
               "Q1.4", "Q5.1_1", "Q5.1_2", "Q5.2_1", "Q5.3_1",
               "Q5.5_1", "Q5.6_1", "Q15.4", "Q15.5", "Q15.6", 
               grep("First Click", names(Data), value=T), 
               "Q_TotalDuration", "gc", "Commute days", 
               "Work in hours", "Work Minutes", 
               "DWT", "Work days", "TTPD")
c_numeric <- c("LocationLatitude", "LocationLongitude",
               "Q5.4_1_TEXT", "Q5.4_2_TEXT", "Q5.4_3_TEXT",
               "Q6.4_1_TEXT", "Q6.4_2_TEXT", "Q6.4_3_TEXT","Q6.5_1",
               "Q6.5_2", "Q6.5_3", "Q6.7_1_TEXT", "Q6.7_2_TEXT",
               "Q6.7_3_TEXT", "Q6.9_1_TEXT", "Q6.9_2_TEXT","Q6.9_3_TEXT",
               "Q6.11_1_TEXT", "Q6.11_2_TEXT", "Q6.11_3_TEXT", 
               grep("First Click", names(Data), value=T), 
               grep("Last Click", names(Data), value=T), 
               grep("Page Submit", names(Data), value=T), 
               "Income PD", "Gallons consumed", 
               "Calculated fuel cost", "Fuel cost from provided", 
               "Parking cost", "Toll cost", "Fare cost", 
               "TCPD", "PTTC1", "PTTC2", "PWTC1", "PWTC2",
               "PINCC1", "PINCC2", "PTCC1", "PTCC2", "CINCC1", "CINCC2", 
               grep("MD12", names(Data), value=T), 
               grep("MD345", names(Data), value=T), 
               grep("M15TC", names(Data), value=T), 
               grep("M3TC", names(Data), value=T))
c_datetime <- c("StartDate", "EndDate", "RecordedDate")
temp <- c()
for (i in c(9 : 13)) {
  for (j in seq(2, 20, 2)) {
    temp <- c(temp, paste0("Q", i, ".", j))
  }; rm(j)
}; rm(i)
c_factor <- c("Q1.2", "Q1.5", "Q1.6", "Q2.1", "Q2.2", "Q5.4",
              "Q6.2", "Q6.3", "Q6.4", "Q6.6", "Q6.7", "Q6.8", 
              "Q6.9", "Q6.10", "Q14.1_1", "Q14.1_2", "Q14.1_3",
              "Q14.1_4", "Q14.4", "Q14.5", "Q14.6", "Q14.7",
              "Q14.8", "Q14.9", "Q15.2", "Q15.7", "Q15.8",
              "Q15.9", "Q15.10", "Mode", temp)
c_check_text <- c("Q2.2_3_TEXT", "Q6.2_6_TEXT", "Q14.3_1", "Q15.3", "Q16.2")
# all other columns are characters, no change

# Convert logical columns
for (i in c_logical) {
  Data[,i] <- as.logical(Data[ i], tz="America/Denver")
}; rm(i)

# Convert integer columns
for (i in c_integer) {
  Data[,i] <- as.integer(Data[,i], tz="America/Denver")
}; rm(i)

# Convert numeric columns
for (i in c_numeric) {
  Data[,i] <- as.numeric(Data[,i], tz="America/Denver")
}; rm(i)

# Convert datetime columns
for (i in c_datetime) {
  Data[,i] <- as.POSIXct(Data[,i], tz="America/Denver")
}; rm(i)

# Convert factor columns
# inspect
for (i in c_factor) {
  print(i)
  print(table(Data[,i]))
}; rm(i)
# format factors
Data[,"Q1.2"] <- factor(Data[,"Q1.2"], levels=c("Accept","Decline"))
Data[,"Q1.5"] <- factor(
  Data[, "Q1.5"], 
  levels=c(
    "United States of America.",
    "Outside of United States of America."
  )
)
Data[,"Q1.6"] <- factor(
  Data[,"Q1.6"],
  levels = c(
    "Yes, I am currently commuting to work on regular basis.",
    "No, but I used to commute to work on regular basis before the pandemic.",
    "No"), labels=c("YesCurrently", "NoUsedTo", "No"
  )
)
Data[,"Q2.1"] <- factor(
  Data[,"Q2.1"], 
  levels=c(
    "Below $15,000", 
    "$15,000 to $49,999", 
    "$50,000 to $99,999", 
    "$100,000 to $150,000", 
    "Above $150,000",
    "Prefer not to answer"
  )
)
Data[,"Q2.2"] <- factor(
  Data[,"Q2.2"], 
  levels=c(
    "Male", "Female", 
    "Prefer to self-describe:",
    "Prefer not to answer"
  )
)
Data[,"Q5.4"] <- factor(
  Data[,"Q5.4"], 
  levels=paste0(
    "Income in dollars per ",
    c("day", "week", "month")
  )
)
Data[,"Q6.2"] <- factor(
  Data[,"Q6.2"], 
  levels=c(
    paste("Automobile, as a driver (you own a vehicle",
          "rental car or car share vehicle)"),
    paste("Automobile, as a passenger (private vehicle, taxi,",
          "or ride-share like Uber or Lyft)"),
    "Public transit (bus, rail, train, shuttle, etc.)", 
    "Walk (or skateboard, wheelchair, etc.)", 
    "Bike (your own bike, bike share, or e-scooter share)", 
    "Other (please describe)"
  ), 
  labels=c("AutoD", "AutoP", "Transit", "Walk", "Bike", "Other")
)
Data[,"Q6.3"] <- factor(
  Data[,"Q6.3"],
  levels=c("Yes", "No")
)
Data[,"Mode"] <- factor(
  Data[,"Mode"], 
  levels=c(
    paste("Automobile, as a driver (you own a vehicle",
          "rental car or car share vehicle)"),
    paste("Automobile, as a passenger (private vehicle, taxi,",
          "or ride-share like Uber or Lyft)"),
    "Public transit (bus, rail, train, shuttle, etc.)", 
    "Walk (or skateboard, wheelchair, etc.)", 
    "Bike (your own bike, bike share, or e-scooter share)", 
    "Other (please describe)"), 
  labels=c("AutoD", "AutoP", "Transit", "Walk", "Bike", "Other"
  )
)
Data[,"Q6.4"] <- factor(
  Data[,"Q6.4"], 
  levels=c(
    paste0(
      "Cost in dollars per ",
      c("day", "week", "month")), 
    "I do not know my exact fuel cost. (We will help you calculate it.)"), 
  labels=c(
    paste0("Cost in dollars per ", c("day", "week", "month")), 
    "I do not know my exact fuel cost."
  )
)
Data[,"Q6.6"] <- factor(Data[,"Q6.6"], levels=c("Yes", "No"))
Data[,"Q6.7"] <- factor(
  Data[, "Q6.7"], 
  levels=paste0("Cost in dollars per ", c("day", "week", "month"))
)
Data[,"Q6.8"] <- factor(Data[,"Q6.8"], levels=c("Yes", "No"))

Data[,"Q6.9"] <- factor(
  Data[,"Q6.9"],
  levels=paste0("Cost in dollars per ", c("day", "week", "month"))
)
Data[,"Q6.10"] <- factor(Data[,"Q6.10"], levels=c("Yes", "No"))
for (i in paste0("Q14.1_", c(1:4))) {
  Data[,i] <- factor(
    Data[,i], 
    levels=c("Not important", "Slightly important", 
             "Moderately important", "Important", "Very important"))
}; rm(i)
for (i in c("Q14.4", "Q14.5")) {
  Data[,i] <- factor(
    Data[,i], 
    levels=c("Extremely dissatisfied", "Somewhat dissatisfied", 
            "Neither satisfied nor dissatisfied", 
            "Somewhat satisfied", "Extremely satisfied"))
}; rm(i)
Data[,"Q14.6"] <- factor(Data[,"Q14.6"], levels=c("Yes", "No"))
Data[,"Q14.7"] <- factor(Data[,"Q14.7"], levels=c("Yes", "No"))
Data[,"Q14.8"] <- factor(Data[,"Q14.8"], 
                         levels=c("Working", "Commuting to work"))
Data[,"Q14.9"] <- factor(
  Data[,"Q14.9"], 
  levels=c("Zero minutes", "1 to 5 minutes", 
           "6 to 15 minutes", "More than 15 minutes"))
Data[,"Q15.2"] <- factor(
  Data[,"Q15.2"], 
  levels=c("18 to 29 years", "30 to 44 years", 
           "45 to 59 years", "60 to 79 years", "80 years and over")
)
Data[,"Q15.7"] <- factor(Data[,"Q15.7"], levels=c("Yes", "No"))
for (i in c("Q15.8", "Q15.9")) {
  Data[,i] <- factor(Data[,i], levels=c("Urban", "Suburban", "Rural"))
}; rm(i)
Data[,"Q15.10"] <- factor(
  Data[,"Q15.10"], 
  levels=c(
    "Less than a high school diploma", 
    "High school diploma or equivalent (e.g. GED)", 
    "Bachelor's or associate degree", 
    paste("Master's degree, doctorate degree,",
          "or professional degree beyond bachelor's degree"),
    "Prefer not to answer"
  ), labels=c("LessHS", "HS", "Bach", "Mast", "Prefer not to answer")
)
for (i in temp) {
  Data[,i] <- factor(
    Data[,i], 
    levels=c("Current", "Alternative A", "Alternative B"),
    labels=c("C", "A", "B"))
}; rm(i)
# inspect
for (i in c_factor) {
  print(i)
  print(summary(Data[, i]))
}; rm(i)

# Check text columns
for (i in c_check_text) {
  print(i)
  print(table(Data[, i]))
}; rm(i)

# Remove
rm(c_logical, c_integer, c_numeric, c_datetime, c_factor, c_check_text)
rm(temp)

# Inspect
str(Data, list.len=ncol(Data))

########################################
# Edit column names

# Old column names
c_old <- c("Duration (in seconds)", "Q1.2", "Q1.4", "Q1.5",
           "Q1.6", "Q2.1", "Q2.2", "Q2.2_3_TEXT", "Q5.1_1",
           "Q5.1_2", "Q5.2_1", "Q5.3_1", "Q5.4", "Q5.4_1_TEXT",
           "Q5.4_2_TEXT", "Q5.4_3_TEXT", "Q5.5_1", "Q5.6_1",
           "Q6.2", "Q6.2_6_TEXT", "Q6.3", "Q6.4", "Q6.4_1_TEXT",
           "Q6.4_2_TEXT", "Q6.4_3_TEXT", "Q6.5_1", "Q6.5_2", 
           "Q6.5_3", "Q6.6", "Q6.7", "Q6.7_1_TEXT", "Q6.7_2_TEXT",
           "Q6.7_3_TEXT", "Q6.8", "Q6.9", "Q6.9_1_TEXT", "Q6.9_2_TEXT",
           "Q6.9_3_TEXT", "Q6.10", "Q6.11", "Q6.11_1_TEXT", "Q6.11_2_TEXT",
           "Q6.11_3_TEXT", "Q9.2", "Q9.4", "Q9.6", "Q9.8", "Q9.10",
           "Q9.12", "Q9.14", "Q9.16", "Q9.18", "Q9.20", "Q10.2",
           "Q10.4", "Q10.6", "Q10.8", "Q10.10", "Q10.12", "Q10.14",
           "Q10.16", "Q10.18", "Q10.20", "Q11.2", "Q11.4", "Q11.6",
           "Q11.8", "Q11.10", "Q11.12", "Q11.14", "Q11.16", "Q11.18", 
           "Q11.20", "Q12.2", "Q12.4", "Q12.6", "Q12.8", "Q12.10",
           "Q12.12", "Q12.14", "Q12.16", "Q12.18", "Q12.20", "Q13.2",
           "Q13.4", "Q13.6", "Q13.8", "Q13.10", "Q13.12", "Q13.14",
           "Q13.16", "Q13.18", "Q13.20", "Q14.1_1", "Q14.1_2", 
           "Q14.1_3", "Q14.1_4", "Q14.3_1", "Q14.4", "Q14.5", 
           "Q14.6", "Q14.7", "Q14.8", "Q14.9", "Q15.2", "Q15.3", 
           "Q15.3_7_TEXT", "Q15.4", "Q15.5", "Q15.6", "Q15.7",
           "Q15.8", "Q15.9", "Q15.10", "Q16.2", "Work in hours",
           "Work Minutes", "Work days", "Commute days", "Mode",
           "Gallons consumed", "Calculated fuel cost",
           "Fuel cost from provided", "Parking cost", "Toll cost",
           "Fare cost", "DWT", "Income PD", "TTPD", "TCPD")

# New column names
c_new <- c("ExpTime", "ACCEPT", "AGE1", "USRESID", "COMMUTE",
           "HHINC", "GEND", "GEND_TEXT", "WHRS", "WMINS",
           "WDAYS", "CDAYS", "PERINC_TYPE", "PERINC_DAY",
           "PERINC_WEEK", "PERINC_MONTH", "TT1", "TT2", "MODE",
           "MODE_TEXT", "FUELCOST", "COST_FUEL_TYPE", "COST_FUEL_DAY",
           "COST_FUEL_WEEK", "COST_FUEL_MONTH", "COST_FUEL_GAS",
           "COST_FUEL_DIST", "COST_FUEL_MPG", "PARKCOST", "COST_PARK_TYPE",
           "COST_PARK_DAY", "COST_PARK_WEEK", "COST_PARK_MONTH", "FARECOST",
           "COST_FARE_TYPE", "COST_FARE_DAY", "COST_FARE_WEEK",
           "COST_FARE_MONTH", "TOLLCOST", "COST_TOLL_TYPE", "COST_TOLL_DAY",
           "COST_TOLL_WEEK", "COST_TOLL_MONTH", "M1Q1", "M1Q2", "M1Q3",
           "M1Q4", "M1Q5", "M1Q6", "M1Q7", "M1Q8", "M1Q9", "M1Q10",
           "M2Q1", "M2Q2", "M2Q3", "M2Q4", "M2Q5", "M2Q6", "M2Q7",
           "M2Q8", "M2Q9", "M2Q10", "M3Q1", "M3Q2", "M3Q3", "M3Q4",
           "M3Q5", "M3Q6", "M3Q7", "M3Q8", "M3Q9", "M3Q10", "M4Q1",
           "M4Q2", "M4Q3", "M4Q4", "M4Q5", "M4Q6", "M4Q7", "M4Q8",
           "M4Q9", "M4Q10", "M5Q1", "M5Q2", "M5Q3", "M5Q4", "M5Q5",
           "M5Q6", "M5Q7", "M5Q8", "M5Q9", "M5Q10", "IMP_TT",
           "IMP_TC", "IMP_WT", "IMP_IN", "TIMEDEP", "SAT_WORK",
           "SAT_COMM", "FLEXWORK", "FLEXINC", "ENJOYWC", "IDEALTT", 
           "AGE2", "RACE", "RACE_TEXT", "HHADULTS", "HHKIDS", 
           "HHWORKERS", "PRIMWORK", "PLACE_LIVE", "PLACE_WORK",
           "EDUC", "COMMENTS", "E_WHRS", "E_WMINS", "E_WDAYS",
           "E_CDAYS", "E_MODE", "E_GALLONS", "E_COST_FUEL_CALC",
           "E_COST_FUEL_PROV", "E_COST_PARK", "E_COST_TOLL",
           "E_COST_FARE", "WTPD", "INPD", "TTPD", "TCPD")

# Change column names
# names(Data)[which(names(Data) %in% c_old)] <- c_new
# DataCols$Name[which(DataCols$Name %in% c_old)] <- c_new
for (i in 1 : length(c_old)) {
  if (c_old[i] %in% names(Data)) {
    names(Data)[which(names(Data) == c_old[i])] <- c_new[i]
    DataCols$Name[which(DataCols$Name == c_old[i])] <- c_new[i]
  }
}; rm(i)

# Remove
rm(c_old, c_new)

########################################
# Process data

# Fix text data
# GEND_TEXt
table(Data$GEND_TEXT)
# - nothing to fix
# MODE_TEXT
table(Data$MODE_TEXT)
# - change to AutoD
tfix <- which(Data$MODE_TEXT %in% c("car", "Company car"))
Data$MODE[tfix] <- "AutoD"
Data$MODE_TEXT[tfix] <- NA
rm(tfix)
# - change to AutoP
tfix <- which(Data$MODE_TEXT %in% 
                c("coworker", "Mom drives me", "Rode with coworkers"))
Data$MODE[tfix] <- "AutoP"
Data$MODE_TEXT[tfix] <- NA
rm(tfix)
# RACE_TEXT
table(Data$RACE_TEXT)
# - nothing to fix
# TIMEDEP
table(Data$TIMEDEP)
# - change to be time on 2020-01-01
Data$TIMEDEP <- as.POSIXct(
  ifelse(is.na(Data$TIMEDEP), NA, paste0("2020-01-01 ", Data$TIMEDEP, ":00")),
  tz="America/Denver")

# Fix clearly incorrect responses
# inspect
table(Data$AGE1)
table(Data$COMMUTE)
table(Data$HHINC)
table(Data$GEND)
table(Data$WHRS)
table(Data$WMINS)
table(Data$WDAYS)
table(Data$CDAYS)
table(Data$PERINC_TYPE)
table(Data$PERINC_DAY)
table(Data$PERINC_WEEK)
table(Data$PERINC_MONTH)
table(Data$TT1)
table(Data$TT2)
table(Data$MODE)
table(Data$FUELCOST)
table(Data$COST_FUEL_TYPE)
table(Data$COST_FUEL_DAY)
table(Data$COST_FUEL_WEEK)
table(Data$COST_FUEL_DAY)
table(Data$COST_FUEL_MONTH)
table(Data$COST_FUEL_GAS)
table(Data$COST_FUEL_DIST)
table(Data$COST_FUEL_MPG)
table(Data$PARKCOST)
table(Data$COST_PARK_TYPE)
table(Data$COST_PARK_DAY)
table(Data$COST_PARK_WEEK)
table(Data$COST_PARK_MONTH)
table(Data$FARECOST)
table(Data$COST_FARE_TYPE)
table(Data$COST_FARE_DAY)
table(Data$COST_FARE_WEEK)
table(Data$COST_FARE_MONTH)
table(Data$TOLLCOST)
table(Data$COST_TOLL_TYPE)
table(Data$COST_TOLL_DAY)
table(Data$COST_TOLL_WEEK)
table(Data$COST_TOLL_MONTH)
table(Data$IMP_TT)
table(Data$IMP_TC)
table(Data$IMP_WT)
table(Data$IMP_IN)
table(Data$SAT_WORK)
table(Data$SAT_COMM)
table(Data$FLEXWORK)
table(Data$FLEXINC)
table(Data$ENJOYWC)
table(Data$IDEALTT)
table(Data$AGE2)
table(Data$RACE)
table(Data$HHADULTS)
table(Data$HHKIDS)
table(Data$HHWORKERS)
table(Data$PRIMWORK)
table(Data$PLACE_LIVE)
table(Data$PLACE_WORK)
table(Data$EDUC)
# - nothing to change in this script

########################################
# Save data

# Save
saveRDS(Data, file.path("Data", "3_Processed", "Data.rds"))
write.csv(Data, file.path("Data", "3_Processed", "Data.csv"), 
          row.names=F)
saveRDS(DataCols, file.path("Data", "3_Processed", "DataCols.rds"))
write.csv(DataCols, file.path("Data", "3_Processed", "DataCols.csv"), 
          row.names=F)

########################################
# Clean up

# Remove
rm(Data, DataCols, xData)
gc()

########################################
# END
########################################