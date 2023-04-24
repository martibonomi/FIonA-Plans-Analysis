# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
#
# Name of the project: Visualization and analysis of "FIonA"'s proton treatment plans
# Aim of the project: to process and visualize proton treatment plans
#     outcome from "FIonA" treatment planning system
# --------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(reconPlots)

readDVHs <- function(dvhs.csv, rename.structures = FALSE, structures.names = NA){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a csv file (FIonA output) with the values of DVHs and the volumes for each structure
  # It returns a list with the DVHs for each structure, the volumes and the names of organs
  # ---------------------------------------------------------------------------------------------
  ## Parameters:
  # dvhs.csv -> .csv fie output from FIonA from DVHs visualization
  # rename.structures [logical] -> TRUE if you want to change the structures names from FIonA,
  #   FALSE if you want to keep FIonA's names
  # structures.names [chr] -> vector with new structures names, only if rename.structures = TRUE
  # --------------------------------------------------------------------------------------------- 
  
  dvhs <- read.csv(dvhs.csv)
  colnames(dvhs)[1] <- "Dose"
  
  # renaming structure names
  if(rename.structures == TRUE){
    for (structure in structures.names) {
      curr.structure.idx <- which(str_detect(colnames(dvhs), regex(structure, ignore_case = TRUE)))
      colnames(dvh)[curr.structure.idx] <- structure
    }
  }
  
  # create dataframe with structures volumes [cc] from last row of plan
  vol.cc <- tail(dvhs, n = 1)
  vol.cc <- vol.cc[2:length(dvhs)]
  
  dvhs <- dvhs[1:nrow(dvhs)-1,]
  
  # convert dose to num (from chr)
  dvh$Dose <- as.numeric(dvh$Dose)
  
  plan <- list(dvhs, vol.cc)
  names(plan) <- c("DVHs", "Volumes [cc]")
  
  return(plan)
  
}
