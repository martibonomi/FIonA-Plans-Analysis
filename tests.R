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
library(testthat)

# test data
dvhs_grid.csv <- "test_data/dvhs_grid.csv"
robustness_grid.csv <- "test_data/robustness_grid.csv"
dvhs_air.csv <- "test_data/dvhs_air.csv"
robustness_air.csv <- "test_data/robustness_air.csv"
dvhs_water.csv <- "test_data/dvhs_water.csv"
robustness_water.csv <- "test_data/robustness_water.csv"

# structures
renamed.structures <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs", "SpinalCord")
structures.to.keep <- c("PTV", "CTV")


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
  assert_that(length(plan$DVHs) == 7 + 1) # number of structures + dose
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
  
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.plan <- selectDVHsStructures(plan, keep.structures = structures.to.keep)
  
  assert_that(all(colnames(filtered.plan[["DVHs"]])[-1] %in% structures.to.keep))
  assert_that(all(colnames(filtered.plan[["Volumes [cc]"]]) %in% structures.to.keep))
  
}


test_readRobustness <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function correctly re-enumerates the columns for each geometrical
  #   shift of the .csv file output from the robustness calculation 
  #   
  # GIVEN: a .csv file output from FIonA's robustness calculation
  # WHEN: I apply "readRobustness" function
  # THEN: the function correctly re-enumerates the columns for geometrical shifts and renames
  #   structures if rename.structures = TRUE
  # ---------------------------------------------------------------------------------------------
  
  # test with default options
  robustness <- readRobustness(robustness_grid.csv, rename.structures = FALSE)
  
  original.csv <- read.csv(robustness_grid.csv)
  original.colnames <- unique(colnames(original.csv))[-1] # get unmodified structures names
  for (idx in 1:length(original.colnames)) {
    original.colnames[idx] <- str_split_1(original.colnames[idx], pattern = "[.]")[1]
  }
  
  expected.colnames <- paste0(original.colnames, "_", 1:9)
  actual.colnames <- colnames(robustness)[-1]
  
  assert_that(all(expected.colnames %in% actual.colnames))
  
  # test with renamed structures
  robustness <- readRobustness(robustness_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  expected.colnames <- paste0(rep(renamed.structures, each = 9), "_", 1:9)
  actual.colnames <- colnames(robustness)[-1]
  
  assert_that(all(expected.colnames %in% actual.colnames))
  
  assert_that(sum(is.na(robustness)) == 0)
  
}


test_selectRobustnessStructures <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the robustness DVHs of the selected structures
  #
  # GIVEN: a robustness datarame (output of "readRobustness") and a vector with structures names
  #   to keep
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the robustness DVHs of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  robustness <- readRobustness(robustness_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.robustness <- selectRobustnessStructures(robustness, keep.structures = structures.to.keep)
  
  structures.to.keep.curves <- paste0(rep(structures.to.keep, each = 9), "_", 1:9)
  
  assert_that(all(colnames(filtered.robustness)[-1] %in% structures.to.keep.curves))
  
}


test_findRobustnessSpread() <- function(){
  # to be done
}


test_getVd <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V[d%] for the selected 
  #   structure
  #
  # GIVEN: a list of dvhs of the wanted plan (output of "readPlan"), a value "d" for the dose 
  #   the structure name for which you want to compute V[d%]
  # WHEN: I apply "getVd" function
  # THEN: the function returns the correct value of V[d%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs.csv = dvhs_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)

  # calculate V95 for CTV
  structure <- "CTV"
  d = 95
  expected.Vd = 97.238
  
  Vd <- getVd(plan, d, structure) 
  
  # check that V[d%] value is the expected one
  assert_that(Vd == expected.Vd)
  
}


test_getDv <- function() {

  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for D[v%] for the selected 
  #   structure
  #
  # GIVEN: a list of dvhs of the wanted plan (output of "readPlan"), a value "v" for the dose 
  #   the structure name for which you want to compute D[v%]
  # WHEN: I apply "getDv" function
  # THEN: the function returns the correct value of D[v%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs.csv = dvhs_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)

  # calculate V95 for CTV
  structure <- "CTV"
  v = 2.025
  expected.Dv = 105.9
  
  Dv <- getDv(plan, v, structure)
  
  # check that D[v%] value is the expected one within a certain tolerance interval
  tolerance <- 1e-6
  assert_that(abs(Dv - expected.Dv) < tolerance)
  
}

