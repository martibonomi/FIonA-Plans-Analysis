# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
#
# Rscript : tests definition
# --------------------------------------------------------------------------------------

# uplaod functions to test
source("functions.R")

# test data
dvhs.csv <- "test_data/dvhs.csv"
dvhs_comparison.csv <- "test_data/dvhs_comparison.csv"
robustness.csv <- "test_data/robustness_dvhs.csv"
energies.csv <- "test_data/energies.csv"

# parameters
renamed.structures <- c("Esophagus", "CTV", "Medulla", "Lungs", "Heart", "PTV")
structures.to.keep <- c("CTV", "PTV")


test_that("--readDVHs-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function gives a list with two objects: plans' DVHs and volumes
  #
  # GIVEN: a csv file output from the FIonA treatment planning system
  # WHEN: I apply "readDVHs" function
  # THEN: the function returns a list with two objects: a dataframe with DVHs values (double) 
  #   and a dataframe with structures' volumes
  # ---------------------------------------------------------------------------------------------
  
  # test with default options
  plan <- readDVHs(dvhs.csv, rename.structures = FALSE)
  expect_equal(length(plan), 2)
  expect_true(is.data.frame(plan$DVHs))
  expect_equal(length(plan$DVHs), 6 + 1) # number of structures + dose
  expect_equal(length(plan$"Volumes [cc]"), 6)
  
  # test with renamed structures
  plan <- readDVHs(dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  expect_equal(length(plan), 2)
  expect_true(is.data.frame(plan$DVHs))
  expect_equal(length(plan$DVHs), 6 + 1) # number of structures + dose
  expect_equal(length(plan$"Volumes [cc]"), 6)
  expect_true(all(colnames(plan$DVHs)[-1] %in% renamed.structures)) # [-1] for removing Dose column
  
})


test_that("--selectDVHsStructures-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the DVHs and volumes of the selected structures
  #
  # GIVEN: a plan (output of "readDVHs") and a vector with structures names
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the DVHs and volumes of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.plan <- selectDVHsStructures(plan, keep.structures = structures.to.keep)
  
  expect_equal(colnames(filtered.plan[["DVHs"]])[-1], structures.to.keep)
  expect_equal(colnames(filtered.plan[["Volumes [cc]"]]), structures.to.keep)
  
})


test_that("--plotDVHs-- function works correclty", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with the correct title assigned to the 
  #   plot 
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotDVHs" function with title's options
  # THEN: the function correctly assigns the title chosen for the plot
  # ---------------------------------------------------------------------------------------------
  
  plan <- readDVHs(dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  # test with title = TRUE
  plot <- plotDVHs(plan, plan.name = "test plan", title = TRUE)
  expected.title <- "DVHs for plan test plan"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  plot <- plotDVHs(plan, plan.name = "test plan", title = FALSE)
  expected.title <- NULL
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  plot <- plotDVHs(plan, plan.name = "test plan", title = "test title")
  expected.title <- "test title"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotComparePlansDVHs-- function works correclty", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with the correct title assigned to the 
  #   plot 
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotComparePlansDVHs" function with title's options
  # THEN: the function correctly assigns the title chosen for the plot
  # ---------------------------------------------------------------------------------------------
  
  plan1 <- readDVHs(dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  plan2 <- readDVHs(dvhs_comparison.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  plans <- list("Plan 1" = plan1, "Plan 2" = plan2)
  
  # test with title = TRUE
  plot <- plotComparePlansDVHs(plans, title = TRUE)
  expected.title <- "Plan 1 vs. Plan 2"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  plot <- plotComparePlansDVHs(plans, title = FALSE)
  expected.title <- NULL
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  plot <- plotComparePlansDVHs(plans, title = "test title")
  expected.title <- "test title"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--readRobustness-- function works correclty", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function correctly re-enumerates the columns for each geometrical
  #   shift of the .csv file output from the robustness calculation 
  #   
  # GIVEN: a csv file output from FIonA's robustness calculation
  # WHEN: I apply "readRobustness" function
  # THEN: the function correctly re-enumerates the columns for geometrical shifts and renames
  #   structures if rename.structures = TRUE
  # ---------------------------------------------------------------------------------------------
  
  # test with default options
  robustness <- readRobustness(robustness.csv, rename.structures = FALSE)
  
  structures.names <- c("DIBH_Esophagus", "DIBH_CTV_bridge", "DIBH_Medulla", "Lungs", "DIBH_Heart", "DIBH_PTV_bridge")
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
  
})


test_that("--selectRobustnessStructures-- function works correctly", {
  
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
  
})


test_that("--plotRobustness-- function works correctly", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with the correct title assigned to the 
  #   plot 
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustness" function with title's options
  # THEN: the function correctly assigns the title chosen for the plot
  # ---------------------------------------------------------------------------------------------
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  # test with title = TRUE
  plot <- plotRobustness(robustness, robustness.name = "test rob", title = TRUE)
  expected.title <- "Robustness DVHs for plan test rob"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  plot <- plotRobustness(robustness, robustness.name = "test plan", title = FALSE)
  expected.title <- NULL
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  plot <- plotRobustness(robustness, robustness.name = "test plan", title = "test title")
  expected.title <- "test title"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--findRobustnessSpread-- function works correctly", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a dataframe with three columns for each 
  #   structure: str_nom, str_max and str_min with nominal, maximum and minimum values of 
  #   the 9 robustness curves for each structures
  #
  # GIVEN: a dataframe of robustness DVHs output from "readRobustness"
  # WHEN: I apply "findRobustnessSpread" function
  # THEN: the function returns a dataframe with str_nom, str_max and str_min for each structure
  # ---------------------------------------------------------------------------------------------
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  spread <- findRobustnessSpread(robustness)
  
  expected.colnames <- paste0(rep(renamed.structures, each = 3), "_", c("nom", "max", "min"))
  actual.colnames <- colnames(spread)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  
})


test_that("--plotRobustnessSpread-- function works correctly", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with the correct title assigned to the 
  #   plot 
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustnessSpread" function with title's options
  # THEN: the function correctly assigns the title chosen for the plot
  # ---------------------------------------------------------------------------------------------
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  spread <- findRobustnessSpread(robustness)
  
  # test with title = TRUE
  plot <- plotRobustnessSpread(robustness, robustness.name = "test rob", title = TRUE)
  expected.title <- "Robustness spread for plan test rob"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  plot <- plotRobustnessSpread(robustness, robustness.name = "test plan", title = FALSE)
  expected.title <- NULL
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  plot <- plotRobustnessSpread(robustness, robustness.name = "test plan", title = "test title")
  expected.title <- "test title"
  actual.title <- plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--getVd-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V[d%] for the selected 
  #   structure
  #
  # GIVEN: a plan (output of "readPlan"), a value "d" for the dose and the structure name for 
  #   which you want to compute V[d%]
  # WHEN: I apply "getVd" function
  # THEN: the function returns the correct value of V[d%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  # calculate V95 for CTV
  structure <- "CTV"
  d = 95
  expected.Vd = 98.644
  
  plan <- readDVHs(dvhs.csv = dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Vd <- getVd(plan, d, structure) 
  
  expect_equal(actual.Vd, expected.Vd)
  
})


test_that("--getDv-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for D[v%] for the selected 
  #   structure
  #
  # GIVEN: a plan (output of "readPlan"), a value "v" for the dose and the structure name for 
  #   which you want to compute D[v%]
  # WHEN: I apply "getDv" function
  # THEN: the function returns the correct value of D[v%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  # calculate D2 for CTV
  structure <- "CTV"
  v = 2.201
  expected.Dv = 105.3
  
  plan <- readDVHs(dvhs.csv = dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Dv <- getDv(plan, v, structure)
  
  # adding a tolerance as the approxfun in "getDv" might approximate values for fitting the curve
  expect_equal(actual.Dv, expected.Dv, tolerance = 1e-6)
  
})


test_that("--getStructureRobustness-- function works correcly",{
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value of robustness for the worst 
  #   case scenario of the selected structure
  #
  # GIVEN: a datframe of structures' robustness dvhs (output of "readRobustness), a value for
  #   the dose and the name of a structure for which you want to calculate robustness
  # WHEN: I apply "getStructureRobustness" function
  # THEN: the function returns the correct value of V[d%] of robustness dvhs for the worst case
  #   scenario of the selected structure
  # ---------------------------------------------------------------------------------------------
  
  # expected value
  structure <- "CTV"
  dose = 95
  expected.rob <- 95.859
  
  robustness <- readRobustness(robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.rob <- getStructureRobustness(robustness, dose, structure)
  
  expect_equal(actual.rob, expected.rob)
  
})


test_that("--getEnergies-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a list with the total energies used and the 
  #   energies for each field
  #
  # GIVEN: a csv file with energy values and spots' weights output from FIonA
  # WHEN: I apply "getEnergies" function
  # THEN: the function returns a list with the total energies used and the energies for each 
  #   field of the plan
  # ---------------------------------------------------------------------------------------------
  
  energies <- getEnergies(energies.csv = energies.csv)
  
  expect_true(typeof(energies) == "list")
  expect_equal(length(energies), 4)
  expect_equal(names(energies), c("Total energies", "F0", "F1", "F2"))
  
})
