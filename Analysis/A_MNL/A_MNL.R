########################################
# Project:  Niranjan Poudel MS Thesis
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     A_MNL.R
# Date:     2023 Summer
# About:    Estimates MNL Model A
########################################

# Major edits
# 2023-08-11 PS created from NP script "Model.R"

########################################
# Notes

# Open this R script directly by double-clicking on it
# don't open R project; working directory must be this folder

# Install, load packages
library(apollo)

########################################
# Definition of core settings

# Initialize code
apollo_initialise()

# Set core controls
apollo_control <- list(
  modelName  = "A",
  modelDescr = "MNL",
  indivID    = "ID",
  nCores     = 7
)

########################################
# Data loading

# Load data
dl <- readRDS(file.path("..", "..", "Data", "4_Final", "DataLong.rds"))

# Rename
database <- dl

########################################
# Parameter definition

# Parameters to be estimated
apollo_beta <- c(
  ascC =  0.5,
  ascA =  0.2,
  ascB =  0, 
  b_TC = -0.05,
  g_TC =  1, 
  g_TT =  0.4,
  g_WT = -0.1, 
  g_IN = -0.1
)

# Fixed parameters
apollo_fixed <- c("ascB", "g_TC")

########################################
# Input validation

# Validating the inputs
apollo_inputs <- apollo_validateInputs()
# - should identify panel nature of the dataset
#   and set panelData=TRUE in apollo_control

########################################
# Likelihood definition

# Define probabilities
apollo_probabilities <- function(apollo_beta, apollo_inputs,
                                 functionality="estimate") {
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities P
  P <- list()
  
  # List of utilities
  V <- list()
  V[["C"]] <- ascC + b_TC * (g_TT * TT_C + 
                               g_TC * TC_C + g_WT * WT_C + g_IN * IN_C)
  V[["A"]] <- ascA + b_TC * (g_TT * TT_A +
                               g_TC * TC_A + 
                               g_WT * WT_A + g_IN * IN_A)
  V[["B"]] <- ascB + b_TC * (g_TT * TT_B + 
                               g_TC * TC_B + g_WT * WT_B + g_IN * IN_B)
  
  # Define settings for MNL model component
  mnl_settings <- list(
    alternatives = c(C = 1, A = 2, B = 3), 
    avail        = list(C = 1, A = 1, B = 1), 
    choiceVar    = choice,
    V            = V
  )
  
  # Compute probabilities using MNL model
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # Take product across observation for same individual
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare outputs of function
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  
  # Return
  return(P)
   
}

########################################
# Model estimation and reporting

# Estimate model
model <- apollo_estimate(
  apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, 
  estimate_settings=list(
    writeIter=F)
)

# Print results
apollo_modelOutput(model, modelOutput_settings=list(printPVal=2)) 

# Save results
apollo_saveOutput(model, saveOutput_settings=list(printPVal=2))

########################################
# Postprocessing of results

# none

########################################
# Clean-up

# Remove
rm(dl, database, model)
rm(apollo_control, apollo_inputs, apollo_beta, apollo_fixed)
rm(apollo_probabilities)
gc()

########################################
# END
########################################