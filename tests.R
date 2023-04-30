# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
# Rscript : tests definition
#
# Name of the project: Visualization and analysis of "FIonA"'s proton treatment plans
# Aim of the project: to process and visualize proton treatment plans
#     outcome from "FIonA" treatment planning system
# --------------------------------------------------------------------------------------

# uplaod functions to test
source("functions.R")

# test data
dvhs.csv <- "test_data/dvhs.csv"
robustness.csv <- "test_data/robustness.csv"
energies.csv <- "test_data/energies.csv"

# parameters
renamed.structures <- c("CTV", "Esophagus", "Medulla", "SpinalCord", "Heart", "Lungs", "PTV")
structures.to.keep <- c("CTV", "PTV")


test_readDVHs <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function gives a list with two objcets: plans' DVHs and volumes
  #
  # GIVEN: a .csv file output from FIonA treatment planning system
  # WHEN: I apply readDVHs function
  # THEN: the function returns a list with two objects: a dataframe with structures' DVHs 
  #   and a dataframe with structures' volumes
  # ---------------------------------------------------------------------------------------------
  
  # test with default options
  plan <- readDVHs(dvhs.csv, rename.structures = FALSE)
  
  expect_equal(length(plan), 2)
  expect_equal(length(plan$DVHs), 7 + 1) # number of structures + dose
  expect_equal(length(plan$"Volumes [cc]"), 7)

  # test with renamed structures
  plan <- readDVHs(dvhs_grid.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  expect_true(length(plan) == 2)
  expect_true(length(plan$DVHs) == 7 + 1) # number of structures + dose
  expect_true(length(plan$"Volumes [cc]") == 7)
  expect_true(all(colnames(plan$DVHs)[-1] == renamed.structures)) # removing Dose column
  
}


test_selectDVHsStructures <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the DVHs and volumes of the selected structures
  #
  # GIVEN: a plan (output of readDVHs) and a vector with structures names
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the DVHs and volumes of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.plan <- selectDVHsStructures(plan, keep.structures = structures.to.keep)
  
  expect_equal(colnames(filtered.plan[["DVHs"]])[-1], structures.to.keep)
  expect_equal(colnames(filtered.plan[["Volumes [cc]"]]), structures.to.keep)
  
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
  robustness <- readRobustness(robustness.csv, rename.structures = FALSE)
  
  structures.names <- c("DIBH_CTV_bridge", "DIBH_Esophagus", "DIBH_Medulla", "PRV_SpinalCord", "DIBH_Heart", "Lungs", "DIBH_PTV_bridge")
  expected.colnames <- paste0(rep(structures.names, each = 9), "_", 1:9)
  actual.colnames <- colnames(robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  expect_equal(sum(is.na(robustness)), 0)
  
  # test with renamed structures
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  structures.names <- renamed.structures
  expected.colnames <- paste0(rep(structures.names, each = 9), "_", 1:9)
  actual.colnames <- colnames(robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  expect_equal(sum(is.na(robustness)), 0)
  
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
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.robustness <- selectRobustnessStructures(robustness, keep.structures = structures.to.keep)
  
  expected.colnames <- paste0(rep(structures.to.keep, each = 9), "_", 1:9)
  actual.colnames <- colnames(filtered.robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  
}


# test_findRobustnessSpread <- function(){
#   # to be done
# }


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
  
  # calculate V95 for CTV
  structure <- "CTV"
  d = 95
  expected.Vd = 97.238
  
  plan <- readDVHs(dvhs.csv = dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Vd <- getVd(plan, d, structure) 
  
  expect_equal(actual.Vd, expected.Vd)
  
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
  
  # calculate D2 for CTV
  structure <- "CTV"
  v = 2.025
  expected.Dv = 105.9
  
  plan <- readDVHs(dvhs.csv = dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Dv <- getDv(plan, v, structure)
  
  # adding a tolerance as the approxfun in "getDv" might approximate values for fitting the curve
  expect_equal(actual.Dv, expected.Dv, tolerance = 1e-6)
  
}


test_getStructureRobustness <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value of robustness for the worst 
  #   case scenario of the selected structure
  #
  # GIVEN: a datframe of structures' robustness dvhs (output of "readRobustness), a value for
  #   the dose and the name of a structure wìfor which you want to calculate robustness
  # WHEN: I apply "getStructureRobustness" function
  # THEN: the function returns the correct value of V[d%] of robustness dvhs for the worst case
  #   for the worst case scenario of the selected structure
  # ---------------------------------------------------------------------------------------------
  
  # expected value
  structure <- "CTV"
  dose = 95
  expected.rob <- 92.755
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.rob <- getStructureRobustness(robustness, dose, structure)
  
  expect_equal(actual.rob, expected.rob)
  
}


test_getEnergies <- function(){
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a list with the total energies used and the 
  #   energies for each field
  #
  # GIVEN: a .csv file with energy values and spots' weights output from FIonA
  # WHEN: I apply "getEnergies" function
  # THEN: the function returns a list with the total energies used and the energies for each 
  #   field of the plan
  # ---------------------------------------------------------------------------------------------
  
  energies <- getEnergies(energies.csv = energies.csv)
  
  expect_true(typeof(energies) == "list")
  expect_equal(length(energies), 4)
  expect_equal(names(energies), c("Total energies", "F0", "F1", "F2"))
  
}

