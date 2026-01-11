###############################################################################################################
# Script for analysis of the Stop Signal task according to the pre-registration on the Open Science Framework #
# Link:                                                                                                       #
# Authors: Jade Pickering & Marta Majewska, 2020                                                              #
###############################################################################################################

##########
# SET-UP #
##########

# load all packages and install if necessary
requiredPackages = c('broom','janitor', 'gdata', 'cowplot', 'viridis', 'tidyverse')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}

# to get the raincloud plots working again
source("https://raw.githubusercontent.com/RainCloudPlots/RainCloudPlots/master/tutorial_R/R_rainclouds.R")





n_trials = 384
n_blocks = 4
trials_per_block = n_trials/n_blocks
comp_req = 0.8
anticipatory_rt = 150
corr_resp = 1
incorr_resp = 0



