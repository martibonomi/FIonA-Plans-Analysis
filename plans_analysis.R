# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
# 
# Rscript: Analysis workflow example
# --------------------------------------------------------------------------------------

# upload required functions
source("functions.R")

renamed.structures <- c("CTV", "PTV", "Lungs", "Esophagus", "Heart", "Medulla")
structures.to.keep <- c("CTV", "PTV")

# plan DVHs visualization
my_plan <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plotDVHs(my_plan, plan.name = "My Plan", title = TRUE)

filtered_plan <- selectDVHsStructures(my_plan, keep.structures = structures.to.keep)
plotDVHs(filtered_plan, plan.name = "My Filtered Plan", title = "Filtered DVHs")

# find Vd and Dv values for different structures
v95 <- getVd(my_plan, d = 95, structure = "CTV")
v107 <- getVd(my_plan, d = 107, structure = "PTV")
d98 <- getDv(my_plan, v = 98, structure = "CTV")
d20 <- getDv(my_plan, v = 20, structure = "Lungs")

# compare DVHs of two different plans
plan1 <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plan2 <- readDVHs(dvhs.csv = "analysis_data/dvhs_comparison.csv", rename.structures = TRUE, structures.names = renamed.structures)
plans <- list("Plan 1" = plan1, "Plan 2" = plan2)
plotComparePlansDVHs(plans = plans, title = TRUE)

# robustness assessment
robustness <- readRobustness(robustness.csv = "analysis_data/robustness_dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plotRobustness(robustness = robustness, robustness.name = "My Plan", title = TRUE)

filtered.robustness <- selectRobustnessStructures(robustness = robustness, keep.structures = structures.to.keep)
plotRobustness(robustness = filtered.robustness, robustness.name = "My Robustness", title = FALSE)

rob_d95 <- getStructureRobustness(robustness, dose = 95, structure = "CTV")

# plan's energy layers
energies <- getEnergies("analysis_data/energies.csv")
