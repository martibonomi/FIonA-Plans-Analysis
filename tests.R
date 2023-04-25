# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
#
# Name of the project: Visualization and analysis of "FIonA"'s proton treatment plans
# Aim of the project: to process and visualize proton treatment plans
#     outcome from "FIonA" treatment planning system
# --------------------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reconPlots)
library(covr)

# test data
dvhs_grid.csv <- "data/dvhs_grid.csv"
robustness_grid.csv <- "data/robustness_grid.csv"
dvhs_air.csv <- "data/dvhs_air.csv"
robustness_air.csv <- "data/robustness_air.csv"
dvhs_water.csv <- "data/dvhs_water.csv"
robustness_water.csv <- "data/robustness_water.csv"

# structures
structures.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs", "SpinalCord")
keep.structures <- c("PTV", "CTV", "Lungs", "Esophagus")


test_readDVHs <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function gives a list with two objcets: plans' DVHs and volumes
  #
  # GIVEN: a .csv file output from FIonA treatment planning system
  # WHEN: I apply readDVHs function
  # THEN: the function returns a list with two objects: a dataframe with DVHs values (double) 
  #   and a dataframe with structures' volumes
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, structures.names)
  
  # assert that "DVHs" is a dataframe with "double" values
  stopifnot(is.data.frame(plan[["DVHs"]]) == TRUE)
  
  stopifnot(length(plan) == 2)
  
  for(col in 1:length(plan[["DVHs"]])){
    stopifnot(is.double(plan[["DVHs"]][,col]) == TRUE)
  }
  
  # assert that "Volumes [cc]" is a dataframe
  stopifnot(is.data.frame(plan[["Volumes [cc]"]]) == TRUE)
  
}


test_selectDVHsStructures <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the DVHs and volumes of the selected structures
  #
  # GIVEN: a plan (output of readDVHs) and a vector with structures names
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the DVHs and volumes of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, structures.names)
  new.plan <- selectDVHsStructures(plan, keep.structures)
  
  stopifnot(colnames(new.plan[["DVHs"]])[-1] %in% keep.structures)
  stopifnot(colnames(new.plan[["Volumes [cc]"]]) %in% keep.structures)
  
}

