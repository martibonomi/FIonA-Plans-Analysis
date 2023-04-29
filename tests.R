# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
# Rscript : tests definition
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
library(assertthat)

# test data
dvhs_grid.csv <- "test_data/dvhs_grid.csv"
robustness_grid.csv <- "test_data/robustness_grid.csv"
dvhs_air.csv <- "test_data/dvhs_air.csv"
robustness_air.csv <- "test_data/robustness_air.csv"
dvhs_water.csv <- "test_data/dvhs_water.csv"
robustness_water.csv <- "test_data/robustness_water.csv"

# structures
renamed.structures <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs", "SpinalCord")
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
  
  # test with default options
  plan <- readDVHs(dvhs_grid.csv, rename.structures = FALSE)
  assert_that(length(plan) == 2)
  assert_that(length(plan$DVHs) == 7 + 1) # number of structures + dose
  assert_that(length(plan$"Volumes [cc]") == 7)
  
  # test with renamed structures
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)
  assert_that(length(plan) == 2)
  assert_that(length(plan$DVHs) == 7 + 1)
  assert_that(length(plan$"Volumes [cc]") == 7)
  assert_that(all(colnames(plan$DVHs)[-1] == renamed.structures)) # removing Dose column
  
}


test_selectDVHsStructures <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the DVHs and volumes of the selected structures
  #
  # GIVEN: a plan (output of readDVHs) and a vector with structures names
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the DVHs and volumes of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, renamed.structures)
  filtered.plan <- selectDVHsStructures(plan, keep.structures)
  
  assert_that(all(colnames(filtered.plan[["DVHs"]])[-1] %in% keep.structures))
  assert_that(all(colnames(filtered.plan[["Volumes [cc]"]]) %in% keep.structures))
  
}

