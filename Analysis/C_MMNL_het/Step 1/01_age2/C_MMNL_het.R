########################################
# Project:  Niranjan Poudel MS Thesis
# Authors:  Patrick Singleton (patrick.singleton@usu.edu)
# File:     C_MMNL_het.R
# Date:     2023 Summer
# About:    Estimates MMNL Model C
########################################

# Major edits
# 2023-08-12 PS created from NP script "Model.R"

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
  modelName  = "C",
  modelDescr = "MMNL_het",
  indivID    = "ID",
  mixing     = TRUE,
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
  m_ascC =  0.9, s_ascC =  1.4,
  m_ascA =  0.3, s_ascA =  0.5,
  ascB   =  0, 
  m_b_TC = -1.6, s_b_TC =  1.9,
  g_TC   =  1, 
  m_g_TT = -2.0, s_g_TT =  1.3,
  m_g_WT = -3.4, s_g_WT =  0.7, 
  m_g_IN = -1.5, s_g_IN =  1.8, 
  bt_age_1829 = 0, bw_age_1829 = 0, 
  bt_age_4559 = 0, bw_age_4559 = 0, 
  bt_age_60up = 0, bw_age_60up = 0, 
  # bt_gend_fem = 0, bw_gend_fem = 0, 
  # bt_race_oth = 0, bw_race_oth = 0, 
  # bt_educ_hsl = 0, bw_educ_hsl = 0, 
  # bt_educ_mas = 0, bw_educ_mas = 0, 
  # bt_hh_adult = 0, bw_hh_adult = 0, 
  # bt_hh_child = 0, bw_hh_child = 0, 
  # bt_inc_0050 = 0, bw_inc_0050 = 0, 
  # bt_inc_100p = 0, bw_inc_100p = 0, 
  # bt_pt_subur = 0, bw_pt_subur = 0, 
  # bt_pt_subsu = 0, bw_pt_subsu = 0, 
  # bt_pt_rural = 0, bw_pt_rural = 0, 
  # bt_day_work = 0, bw_day_work = 0, 
  # bt_day_comm = 0, bw_day_comm = 0, 
  # bt_comm_not = 0, bw_comm_not = 0, 
  # bt_cmode_ap = 0, bw_cmode_ap = 0, 
  # bt_cmode_pt = 0, bw_cmode_pt = 0, 
  # bt_cmode_wk = 0, bw_cmode_wk = 0, 
  # bt_cmode_bi = 0, bw_cmode_bi = 0, 
  # bt_parkcost = 0, bw_parkcost = 0, 
  # bt_tollcost = 0, bw_tollcost = 0, 
  # bt_dept_aen = 0, bw_dept_aen = 0, 
  # bt_noflexwh = 0, bw_noflexwh = 0, 
  # bt_nopriinc = 0, bw_nopriinc = 0, 
  # bt_sat_work = 0, bw_sat_work = 0, 
  # bt_sat_comm = 0, bw_sat_comm = 0, 
  s_TTWT =  1.3
)

# Fixed parameters
apollo_fixed <- c("ascB", "g_TC")

# Parameters for generating draws
apollo_draws <- list(
  interDrawsType = "mlhs",
  interNDraws = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_ascC", "draws_ascA", 
                     "draws_b_TC", "draws_g_TT", 
                     "draws_g_WT", "draws_g_IN"),
  intraDrawsType = "mlhs",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

# Create random parameters
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff = list()
  
  randcoeff[["ascC"]] =      m_ascC + s_ascC * draws_ascC 
  randcoeff[["ascA"]] =      m_ascA + s_ascA * draws_ascA
  randcoeff[["b_TC"]] = -exp(m_b_TC + s_b_TC * draws_b_TC)
  randcoeff[["g_TT"]] =  exp(m_g_TT + s_g_TT * draws_g_TT + 
                             bt_age_1829*AGE2_18_29 +
                               bt_age_4559*AGE2_45_59 +
                               bt_age_60up*AGE2_60_UP) 
  randcoeff[["g_WT"]] =  exp(m_g_WT + s_g_WT * draws_g_WT +
                               s_TTWT * draws_g_TT + 
                             bw_age_1829*AGE2_18_29 +
                               bw_age_4559*AGE2_45_59 +
                               bw_age_60up*AGE2_60_UP) 
  randcoeff[["g_IN"]] = -exp(m_g_IN + s_g_IN * draws_g_IN) 
  
  return(randcoeff)
}

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
  V[["C"]] <- ascC + b_TC * (g_TT * TT_C + g_TC * TC_C + 
                               g_WT * WT_C + g_IN * IN_C)
  V[["A"]] <- ascA + b_TC * (g_TT * TT_A + g_TC * TC_A +
                               g_WT * WT_A + g_IN * IN_A)
  V[["B"]] <- ascB + b_TC * (g_TT * TT_B + g_TC * TC_B +
                               g_WT * WT_B + g_IN * IN_B)
  
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
  
  # Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
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
    # hessianRoutine="maxLik", 
    # scaling = c(
    #   m_g_WT = 10,  s_g_WT = 10, 
    #   m_g_IN = 100, s_g_IN = 100), 
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
rm(apollo_randCoeff, apollo_draws)
rm(apollo_probabilities)
gc()

########################################
# END
########################################