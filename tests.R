# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
#
# Rscript : tests definition
# --------------------------------------------------------------------------------------

# uplaod functions to test
source("functions.R")


test_that("--readDVHs-- function returns correct output for default settings", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function "readDVHs" with default values returns correct output:
  #   a list with two dataframes, i.e. "DVHs" with plan's DVHs and "Volumes [cc]" with structures' 
  #   volumes
  #
  # GIVEN: a csv file output from the FIonA treatment planning system
  # WHEN: I apply "readDVHs" function with default values (renamed.structures = NA)
  # THEN: the function returns a list with two objects: a dataframe "DVHs" with DVHs values
  #   and a dataframe "Volumes [cc]" with structures' volumes
  # ---------------------------------------------------------------------------------------------
  
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv", renamed.structures = NA)
  
  expect_true(is.list(test_plan))
  expect_equal(length(test_plan), 2)
  expect_equal(names(test_plan), c("DVHs", "Volumes [cc]"))
  
  expect_true(is.data.frame(test_plan$DVHs))
  expect_equal(length(test_plan$DVHs), 6 + 1) # number of structures + dose
  
  expect_true(is.data.frame(test_plan$`Volumes [cc]`))
  expect_equal(length(test_plan$`Volumes [cc]`), 6)
  
})


test_that("--readDVHs-- function correctly renames structures when renamed.structures is given", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function correctly renames structures when renamed.structures is 
  #   given in input
  #
  # GIVEN: a csv file output from the FIonA treatment planning system
  # WHEN: I apply "readDVHs" function giving renamed.structures in input to the function
  # THEN: the function correctly renames the structures of my plan with names stored in 
  #   renamed.structures
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv", renamed.structures = new.names)
  
  expect_equal(colnames(test_plan$DVHs)[-1], new.names)
  expect_equal(colnames(test_plan$`Volumes [cc]`), new.names)
  
})


test_that("--selectDVHsStructures-- function returns the correct output", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the DVHs and volumes of the selected structures
  #
  # GIVEN: a plan (output of "readDVHs") and a vector with structures names
  # WHEN: I apply "selectDVHsStructures" function
  # THEN: the function returns a plan with only the DVHs and volumes of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv", renamed.structures = new.names)
  
  structures.to.keep <- c("CTV", "PTV", "Lungs")
  filtered.test_plan <- selectDVHsStructures(test_plan, keep.structures = structures.to.keep)
  
  expect_equal(colnames(filtered.test_plan[["DVHs"]])[-1], structures.to.keep)
  expect_equal(colnames(filtered.test_plan[["Volumes [cc]"]]), structures.to.keep)
  
})


test_that("--plotDVHs-- function assigns the correct title when title = TRUE", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "DVHs for plan test plan" 
  #   when title = TRUE for "test plan" plan
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotDVHs" function with title = TRUE
  # THEN: the function correctly assigns the expected title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plot <- plotDVHs(plan = test_plan, plan.name = "test plan", title = TRUE)
  
  expected.title <- "DVHs for plan test plan"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotDVHs-- function assigns the correct title when title = FALSE", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with no title when title = FALSE for 
  #   "test plan" plan
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotDVHs" function with title = FALSE
  # THEN: the function returns a plot with no title
  # ---------------------------------------------------------------------------------------------
  
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plot <- plotDVHs(plan = test_plan, plan.name = "test plan", title = FALSE)
  
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)

})


test_that("--plotDVHs-- function assigns the correct title when title = 'title' ", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "title" for "test plan" plan
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotDVHs" function with title = "title"
  # THEN: the function correctly assigns the specified title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plot <- plotDVHs(plan = test_plan, plan.name = "test plan", title = "test title")

  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotComparePlansDVHs-- function assigns the correct title when title = TRUE", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "Plan 1 vs. Plan 2" 
  #   when title = TRUE for Plan 1 and Plan 2 comparison
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotComparePlansDVHs" function with title = TRUE
  # THEN: the function correctly assigns the expected title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_plan1 <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plan2 <- readDVHs(dvhs.csv = "test_data/dvhs_comparison.csv")
  test_plans <- list("Plan 1" = test_plan1, "Plan 2" = test_plan2)
  test_plot <- plotComparePlansDVHs(plans = test_plans, title = TRUE)
  
  expected.title <- "Plan 1 vs. Plan 2"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotComparePlansDVHs-- function assigns the correct title when title = FALSE", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with no title when title = FALSE for 
  #   Plan 1 and Plan 2 comparison
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotComparePlansDVHs" function with title = FALSE
  # THEN: the function returns a plot with no title
  # ---------------------------------------------------------------------------------------------
  
  test_plan1 <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plan2 <- readDVHs(dvhs.csv = "test_data/dvhs_comparison.csv")
  test_plans <- list("Plan 1" = test_plan1, "Plan 2" = test_plan2)
  test_plot <- plotComparePlansDVHs(plans = test_plans, title = TRUE)
  
  test_plot <- plotComparePlansDVHs(plans = test_plans, title = FALSE)
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotComparePlansDVHs-- function assigns the correct title when title = 'title' ", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "title" for Plan 1 and Plan 2
  #   comparison
  #   
  # GIVEN: a plan output from "readDVHs" function
  # WHEN: I apply "plotComparePlansDVHs" function with title = "title"
  # THEN: the function correctly assigns the specified title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_plan1 <- readDVHs(dvhs.csv = "test_data/dvhs.csv")
  test_plan2 <- readDVHs(dvhs.csv = "test_data/dvhs_comparison.csv")
  test_plans <- list("Plan 1" = test_plan1, "Plan 2" = test_plan2)
  test_plot <- plotComparePlansDVHs(plans = test_plans, title = TRUE)
  
  test_plot <- plotComparePlansDVHs(plans = test_plans, title = "test title")
  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--readRobustness-- function correctly re-enumerates the columns of input data for default values", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function correctly re-enumerates the columns for each geometrical
  #   shift of the csv file output from FIonA from the robustness calculation
  #   
  # GIVEN: a csv file output from FIonA's robustness calculation
  # WHEN: I apply "readRobustness" function with default values
  # THEN: the function correctly re-enumerates the columns for geometrical shifts 
  # ---------------------------------------------------------------------------------------------

  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv", renamed.structures = NA)
  
  structures.names <- c("DIBH_Esophagus", "DIBH_CTV_bridge", "DIBH_Medulla", "Lungs", "DIBH_Heart", "DIBH_PTV_bridge")
  expected.colnames <- paste0(rep(structures.names, each = 9), "_", 1:9)
  actual.colnames <- colnames(test_robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  expect_equal(sum(is.na(test_robustness)), 0)
  
})


test_that("--readRobustness-- function correctly re-enumerates the columns of input data with new structures names", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function correctly re-enumerates the columns for each geometrical
  #   shift of the csv file output from FIonA from the robustness calculation when 
  #   renamed.structures vector is given in input
  #   
  # GIVEN: a csv file output from FIonA's robustness calculation
  # WHEN: I apply "readRobustness" function giving renamed.structures vector in input
  # THEN: the function correctly re-enumerates column with new structures names
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "Esophagus")
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv", renamed.structures = new.names)
  
  structures.names <- new.names
  expected.colnames <- paste0(rep(structures.names, each = 9), "_", 1:9)
  actual.colnames <- colnames(test_robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  expect_equal(sum(is.na(test_robustness)), 0)
  
})


test_that("--selectRobustnessStructures-- function works correctly", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the robustness DVHs of the selected structures
  #
  # GIVEN: a robustness datarame (output of "readRobustness") and a vector with structures names
  #   to keep
  # WHEN: I apply "selectRobustnessStructures" function
  # THEN: the function returns a plan with only the robustness DVHs of the selected structures 
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = test_robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  filtered.test_robustness <- selectRobustnessStructures(robustness = test_robustness, keep.structures = structures.to.keep)
  
  expected.colnames <- paste0(rep(structures.to.keep, each = 9), "_", 1:9)
  actual.colnames <- colnames(filtered.test_robustness)[-1]
  
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
  
  test_robustness <- readRobustness(robustness = test_robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  
  # test with title = TRUE
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test rob", title = TRUE)
  expected.title <- "Robustness DVHs for plan test rob"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test plan", title = FALSE)
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test plan", title = "test title")
  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
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
  
  test_robustness <- readRobustness(robustness = test_robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  test_spread <- findRobustnessSpread(test_robustness)
  
  expected.colnames <- paste0(rep(renamed.structures, each = 3), "_", c("nom", "max", "min"))
  actual.colnames <- colnames(test_spread)[-1]
  
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
  
  test_robustness <- readRobustness(robustness = test_robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  test_spread <- findRobustnessSpread(robustness = test_robustness)
  
  # test with title = TRUE
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test rob", title = TRUE)
  expected.title <- "Robustness spread for plan test rob"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = FALSE
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test plan", title = FALSE)
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
  # test with title = "test title"
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test plan", title = "test title")
  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
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
  test_structure <- "CTV"
  test_d = 95
  expected.Vd = 98.644
  
  test_plan <- readDVHs(dvhs.csv = test_dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Vd <- getVd(plan = test_plan, d = test_d, structure = test_structure) 
  
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
  test_structure <- "CTV"
  test_v = 2.201
  expected.Dv = 105.3
  
  test_plan <- readDVHs(dvhs.csv = test_dvhs.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.Dv <- getDv(plan = test_plan, v = test_v, structure = test_structure)
  
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
  test_structure <- "CTV"
  test_dose = 95
  expected.rob <- 95.859
  
  robustness <- readRobustness(robustness = test_robustness.csv, rename.structures = TRUE, structures.names = renamed.structures)
  actual.rob <- getStructureRobustness(robustness, dose = test_dose, structure = test_structure)
  
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
  
  test_energies <- getEnergies(energies.csv = test_energies.csv)
  
  expect_true(typeof(test_energies) == "list")
  expect_equal(length(test_energies), 4)
  expect_equal(names(test_energies), c("Total energies", "F0", "F1", "F2"))
  
})
