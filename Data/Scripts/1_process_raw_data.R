########################################
# Project:  Understanding tradeoffs between working and commuting
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
#           Niranjan Poudel (niranjan111@hotmail.com)
# File:     1_process_raw_data.R
# Date:     2023 Summer
# About:    Loads & processes raw survey data from Qualtrics
########################################

# Major edits
# 2023-08-08 PS created from NP script "Get_removed ID.R"
# 2023-08-10 PS updated: read from new file, anonymize

########################################
# Notes

# Open R project first, then open this R script

# Install, load packages
library(readr)

########################################
# Load data

# Five datasets received from Qualtrics
# Data1 <- read_csv(file.path("Data", "1_Raw",
#                             "VTTS+Quote_November+30,+2020_10.04.csv"))
# Data2 <- read_csv(file.path("Data", "1_Raw",
#                             "VTTS Quote_December 7, 2020_07.59.csv"))
# Data3 <- read_csv(file.path("Data", "1_Raw",
#                             "VTTS Quote_December 8, 2020_09.19.csv"))
# Data4 <- read_csv(file.path("Data", "1_Raw", 
#                             "VTTS+Quote_December+15,+2020_09.06.csv"))
# Data5 <- read_csv(file.path("Data", "1_Raw", 
#                             "VTTS+Quote_December+18,+2020_16.20.csv"))
Data <- read_csv(file.path("Data", "1_Raw",
                           "VTTS+Quote_August+9,+2023_08.24.csv"))

########################################
# Combine data

# Column names
# DataCols <- data.frame(Name=names(Data5),  Name1=names(Data5),
#                        Name2=t(Data5[1,]), Name3=t(Data5[2,]))
DataCols <- data.frame(Name=names(Data),  Name1=names(Data), 
                       Name2=t(Data[1,]), Name3=t(Data[2,]))
row.names(DataCols) <- NULL
View(DataCols)

# Remove extra rows
# Data1 <- Data1[-1,]; Data1 <- Data1[-1,]
# Data2 <- Data2[-1,]; Data2 <- Data2[-1,]
# Data3 <- Data3[-1,]; Data3 <- Data3[-1,]
# Data4 <- Data4[-1,]; Data4 <- Data4[-1,]
# Data5 <- Data5[-1,]; Data5 <- Data5[-1,]
Data <- Data[-1,]; Data <- Data[-1,]

# Check names
# names(Data1) == names(Data2)
# names(Data1) == names(Data3)
# names(Data1) == names(Data4)
# names(Data1) == names(Data5)
# names(Data1)[343] <- "opp"

# Combine together
# Data <- Data1
# Data <- rbind(Data, Data2)
# Data <- rbind(Data, Data3)
# Data <- rbind(Data, Data4)
# Data <- rbind(Data, Data5)

# Remove
# rm(Data1, Data2, Data3, Data4, Data5)

# Remove tibble attribute
Data <- as.data.frame(Data)

########################################
# Anonymize data
# Remove potentially identifying columns

Data$IPAddress <- NA
Data$RecipientFirstName <- NA
Data$RecipientLastName <- NA
Data$RecipientEmail <- NA
Data$ExternalReference <- NA
Data$LocationLatitude <- NA
Data$LocationLongitude <- NA
Data$Q16.2 <- NA
Data$opp <- NA
Data$QPMID <- NA
Data$RISN <- NA
Data$rid <- NA

########################################
# Save data

# Save
saveRDS(Data, file.path("Data", "2_Anonymous", "Data.rds"))
write.csv(Data, file.path("Data", "2_Anonymous",
                          "Data.csv"), row.names=F)
saveRDS(DataCols, file.path("Data", "2_Anonymous", "DataCols.rds"))
write.csv(DataCols, file.path("Data", "2_Anonymous",
                              "DataCols.csv"), row.names=F)

########################################
# Clean up

# Remove
rm(Data, DataCols)
gc()

########################################
# END
########################################