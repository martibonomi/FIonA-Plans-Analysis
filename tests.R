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
  
  test_plan <- readDVHs(dvhs.csv = "test_data/test_readDVHs.csv", renamed.structures = NA)
  
  expected.output <- list("DVHs" = data.frame("Dose" = c(0, 20, 40, 60, 80, 100),
                                              "DIBH_CTV_bridge" = c(100, 100, 100, 100, 99.998, 57.720),
                                              "DIBH_PTV_bridge" = c(100, 100, 100, 100, 99.851, 54.177),
                                              "DIBH_Esophagus" = c(100, 33.429, 29.159, 24.360, 17.697, 2.358),
                                              "DIBH_Heart" = c(100, 3.370, 1.649, 0.797, 0.302, 0.001),
                                              "DIBH_Medulla" = c(100, 35.306, 19.125, 8.062, 0, 0),
                                              "Lungs" = c(100, 21.733, 18.800, 16.343, 12.780, 3.898)),
                          "Volumes [cc]" = data.frame("DIBH_CTV_bridge" = c("Vol" = 377.402),
                                                      "DIBH_PTV_bridge" = c("Vol" = 578.878),
                                                      "DIBH_Esophagus" = c("Vol" = 31.822),
                                                      "DIBH_Heart" = c("Vol" = 441.778),
                                                      "DIBH_Medulla" = c("Vol" = 36.214),
                                                      "Lungs" = c("Vol" = 4081.253)))

  expect_equal(expected.output, test_plan)
  
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
  test_plan <- readDVHs(dvhs.csv = "test_data/test_readDVHs.csv", renamed.structures = new.names)
  
  # testing only colnames of "DVHs" because the renaming of structures is done before the original
  #   dataframe is split between "DVHs" and "Volumes [cc]", so column names are the same
  expect_equal(colnames(test_plan$DVHs)[-1], new.names) 

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
  test_plan <- readDVHs(dvhs.csv = "test_data/test_readDVHs.csv", renamed.structures = new.names)
  
  structures.to.keep <- c("CTV", "Lungs")
  filtered.test_plan <- selectDVHsStructures(test_plan, keep.structures = structures.to.keep)
  
  expected.output <- list("DVHs" = data.frame("Dose" = c(0, 20, 40, 60, 80, 100),
                                              "CTV" = c(100, 100, 100, 100, 99.998, 57.720),
                                              "Lungs" = c(100, 21.733, 18.800, 16.343, 12.780, 3.898)),
                          "Volumes [cc]" = data.frame("CTV" = c("Vol" = 377.402),
                                                      "Lungs" = c("Vol" = 4081.253)))
  
  expect_equal(filtered.test_plan, expected.output)
  
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
  
  structures.names <- c("DIBH_CTV_bridge", "DIBH_Esophagus")
  expected.colnames <- paste0(rep(structures.names, each = 9), "_", 1:9)
  actual.colnames <- colnames(test_robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)

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

})


test_that("--selectRobustnessStructures-- function returns the correct output", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function keeps only the robustness DVHs of the selected structures
  #
  # GIVEN: a robustness datarame (output of "readRobustness") and a vector with structures names
  #   to keep
  # WHEN: I apply "selectRobustnessStructures" function
  # THEN: the function returns a robustness dataframe with only the robustness DVHs of the 
  #   selected structures 
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "Esophagus")
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv", renamed.structures = new.names)
  
  structures.to.keep <- c("CTV")
  filtered.test_robustness <- selectRobustnessStructures(robustness = test_robustness, keep.structures = structures.to.keep)
  
  expected.colnames <- paste0(rep(structures.to.keep, each = 9), "_", 1:9)
  actual.colnames <- colnames(filtered.test_robustness)[-1]
  
  expect_equal(actual.colnames, expected.colnames)
  
})


test_that("--plotRobustness-- function assigns the correct title when title = TRUE", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "Robustness DVHs for plan 
  #   test rob" when title = TRUE 
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustness" function with title = TRUE
  # THEN: the function correctly assigns the expected title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test rob", title = TRUE)
  
  expected.title <- "Robustness DVHs for plan test rob"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})  


test_that("--plotRobustness-- function assigns the correct title when title = FALSE", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with no title when title = FALSE
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustness" function with title = FALSE
  # THEN: the function returns a plot with no title
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test rob", title = FALSE)
  
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotRobustness-- function assigns the correct title when title = 'title' ", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "title"
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustness" function with title = TRUE
  # THEN: the function correctly assigns the specified title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustness(robustness = test_robustness, robustness.name = "test rob", title = "test title")
  
  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--findRobustnessSpread-- function correctly finds maxima and minima", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a dataframe with three columns for each 
  #   structure: str_nom, str_max and str_min with nominal, maximum and minimum values of 
  #   the 9 robustness curves for each structures
  #
  # GIVEN: a dataframe of robustness DVHs output from "readRobustness"
  # WHEN: I apply "findRobustnessSpread" function
  # THEN: the function returns a dataframe with str_nom, str_max and str_min for each structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "Esophagus")
  test_robustness <- readRobustness(robustness = "test_data/test_findRobustnessSpread.csv", renamed.structures = new.names)
  test_spread <- findRobustnessSpread(test_robustness)
  
  expected.dataframe <- data.frame("Dose" = c(0, 20, 40, 60 ,80, 100),
                                   "CTV_nom" = c(100, 100, 100, 100, 99.998, 65.999),
                                   "CTV_max" = c(100, 100, 100, 100, 99.999, 65.999),
                                   "CTV_min" = c(100, 100, 100, 100, 99.766, 62.331),
                                   "Esophagus_nom" = c(49.158, 33.521, 29.369, 24.972, 18.070, 3.235),
                                   "Esophagus_max" = c(49.763, 34.444, 30.286, 25.816, 18.992, 5.081),
                                   "Esophagus_min" = c(48.367, 32.521, 28.201, 23.653, 16.260, 1.809))
  
  expect_equal(test_spread, expected.dataframe)
  
})


test_that("--plotRobustnessSpread-- function assigns the correct title when title = TRUE", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "Robustness spread for plan 
  #   test rob" when title = TRUE
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustnessSpread" function with title = TRUE
  # THEN: the function correctly assigns the expected title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test rob", title = TRUE)
  
  expected.title <- "Robustness spread for plan test rob"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})
  

test_that("--plotRobustnessSpread-- function assigns the correct title when title = FALSE", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with no title when title = FALSE
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustnessSpread" function with title = FALSE
  # THEN: the function returns a plot with no title
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test rob", title = FALSE)
  
  expected.title <- NULL
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--plotRobustnessSpread-- function assigns the correct title when title = 'title' ", {  
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a plot with title = "title" when title = "title"
  #   
  # GIVEN: a plan output from "readRobustness" function
  # WHEN: I apply "plotRobustnessSpread" function with title = "title"
  # THEN: the function correctly assigns the specified title to the plot
  # ---------------------------------------------------------------------------------------------
  
  test_robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv")
  test_plot <- plotRobustnessSpread(robustness = test_robustness, robustness.name = "test rob", title = "test title")
  
  expected.title <- "test title"
  actual.title <- test_plot[["labels"]][["title"]]
  expect_equal(actual.title, expected.title)
  
})


test_that("--getVd-- function returns the correct value of V95% for CTV", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V95% for the CTV
  #
  # GIVEN: a plan (output of "readPlan"), a value "d" for the dose and the structure name for 
  #   which you want to compute V[d%]
  # WHEN: I apply "getVd" function
  # THEN: the function returns the correct value of V[d%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/test_getVd.csv", renamed.structures = new.names)
  
  test_structure <- "CTV"
  test_d = 95
  expected.Vd = 98.644
  
  actual.Vd <- getVd(plan = test_plan, d = test_d, structure = test_structure) 
  
  expect_equal(actual.Vd, expected.Vd)
  
})


test_that("--getVd-- function returns the correct value of V50% for Esophagus", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V50% for the Esophagus
  #
  # GIVEN: a plan (output of "readPlan"), a value "d" for the dose and the structure name for 
  #   which you want to compute V[d%]
  # WHEN: I apply "getVd" function
  # THEN: the function returns the correct value of V[d%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/test_getVd.csv", renamed.structures = new.names)
  
  test_structure <- "Esophagus"
  test_d = 50
  expected.Vd = 27.105
  
  actual.Vd <- getVd(plan = test_plan, d = test_d, structure = test_structure) 
  
  expect_equal(actual.Vd, expected.Vd)
  
})


test_that("--getVd-- function returns the correct value of V15% for Esophagus", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V15% for the Lungs
  #
  # GIVEN: a plan (output of "readPlan"), a value "d" for the dose and the structure name for 
  #   which you want to compute V[d%]
  # WHEN: I apply "getVd" function
  # THEN: the function returns the correct value of V[d%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/test_getVd.csv", renamed.structures = new.names)
  
  test_structure <- "Lungs"
  test_d = 15
  expected.Vd = 22.968
  
  actual.Vd <- getVd(plan = test_plan, d = test_d, structure = test_structure) 
  
  expect_equal(actual.Vd, expected.Vd)
  
})


test_that("--getDv-- function returns the correct value of D5% for PTV", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value of D5% for PTV
  #
  # GIVEN: a plan (output of "readPlan"), a value "v" for the dose and the structure name for 
  #   which you want to compute D[v%]
  # WHEN: I apply "getDv" function
  # THEN: the function returns the correct value of D[v%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs_complete.csv", renamed.structures = new.names)
  
  test_structure <- "PTV"
  test_v = 5
  expected.Dv = 104.3
  
  actual.Dv <- getDv(plan = test_plan, v = test_v, structure = test_structure)
  
  # adding a tolerance as the approxfun in "getDv" might approximate values for fitting the curve
  expect_equal(actual.Dv, expected.Dv, tolerance = 1e-3)
  
})


test_that("--getDv-- function returns the correct value of D95% for PTV", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value of D95% for PTV
  #
  # GIVEN: a plan (output of "readPlan"), a value "v" for the dose and the structure name for 
  #   which you want to compute D[v%]
  # WHEN: I apply "getDv" function
  # THEN: the function returns the correct value of D[v%] for the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "PTV", "Esophagus", "Heart", "Medulla", "Lungs")
  test_plan <- readDVHs(dvhs.csv = "test_data/dvhs_complete.csv", renamed.structures = new.names)
  
  test_structure <- "PTV"
  test_v = 95
  expected.Dv = 95.0
  
  actual.Dv <- getDv(plan = test_plan, v = test_v, structure = test_structure)
  
  # adding a tolerance as the approxfun in "getDv" might approximate values for fitting the curve
  expect_equal(actual.Dv, expected.Dv, tolerance = 1e-3)
  
})


test_that("--getStructureRobustness-- function returns the correct value of robustness V90% for CTV", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V90% of robustness in 
  #   the worst case scenario for CTV
  #
  # GIVEN: a dataframe of structures' robustness dvhs (output of "readRobustness), a value for
  #   the dose and the name of a structure for which you want to calculate robustness
  # WHEN: I apply "getStructureRobustness" function
  # THEN: the function returns the correct value of V[d%] of robustness dvhs for the worst case
  #   scenario of the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "Esophagus")
  robustness <- readRobustness(robustness = "test_data/robustness_dvhs.csv", renamed.structures = new.names)
  
  test_structure <- "CTV"
  test_dose = 90
  expected.rob <- 98.363
  
  actual.rob <- getStructureRobustness(robustness, dose = test_dose, structure = test_structure)
  
  expect_equal(actual.rob, expected.rob)
  
})


test_that("--getStructureRobustness-- function returns the correct value of robustness V20% for Esophagus", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns the correct value for V80% of robustness in 
  #   the worst case scenario for Esophagus
  #
  # GIVEN: a dataframe of structures' robustness dvhs (output of "readRobustness), a value for
  #   the dose and the name of a structure for which you want to calculate robustness
  # WHEN: I apply "getStructureRobustness" function
  # THEN: the function returns the correct value of V[d%] of robustness dvhs for the worst case
  #   scenario of the selected structure
  # ---------------------------------------------------------------------------------------------
  
  new.names <- c("CTV", "Esophagus")
  robustness <- readRobustness(robustness.csv = "test_data/robustness_dvhs.csv", renamed.structures = new.names)
  
  test_structure <- "Esophagus"
  test_dose = 20
  expected.rob <- 32.521
  
  actual.rob <- getStructureRobustness(robustness, dose = test_dose, structure = test_structure)
  
  expect_equal(actual.rob, expected.rob)
  
})


test_that("--getEnergies-- function returns a list with expected values", {
  
  # ---------------------------------------------------------------------------------------------
  # This test asserts that the function returns a list with the sorted total energies used and the 
  #   sorted energies for each field
  #
  # GIVEN: a csv file with energy values and spots' weights output from FIonA
  # WHEN: I apply "getEnergies" function
  # THEN: the function returns a list with the sorted total energies used and the sorted energies 
  #   for each field of the plan
  # ---------------------------------------------------------------------------------------------
  
  test_energies <- getEnergies(energies.csv = "test_data/energies.csv")
  
  expected.output <- list("Total energies" = c(78.769, 80.970, 85.241, 89.352, 99.047, 106.304, 109.793, 150.621),
                          "F0" = c(89.352, 106.304),
                          "F1" = c(78.769, 85.241),
                          "F2" = c(80.970, 99.047, 109.793, 150.621))
  
  expect_equal(test_energies, expected.output)
  
})
