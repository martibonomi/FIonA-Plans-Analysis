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

# Plan's DVHs Visualization
my_plan <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plotDVHs(plan = my_plan, plan.name = "My Plan", title = TRUE)

filtered_plan <- selectDVHsStructures(plan = my_plan, keep.structures = structures.to.keep)
plotDVHs(plan = filtered_plan, plan.name = "My Filtered Plan", title = "Filtered DVHs")

# Quality Parameters
v95 <- getVd(plan = my_plan, d = 95, structure = "CTV")
v107 <- getVd(plan = my_plan, d = 107, structure = "PTV")
d98 <- getDv(plan = my_plan, v = 98, structure = "CTV")
d20 <- getDv(plan = my_plan, v = 20, structure = "Lungs")

# Plans' DVHs Comparison
plan1 <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plan2 <- readDVHs(dvhs.csv = "analysis_data/dvhs_comparison.csv", rename.structures = TRUE, structures.names = renamed.structures)
plans <- list("Plan 1" = plan1, "Plan 2" = plan2)
plotComparePlansDVHs(plans = plans, title = TRUE)

# Plan's Robustness Assessment
my_robustness <- readRobustness(robustness.csv = "analysis_data/robustness_dvhs.csv", rename.structures = TRUE, structures.names = renamed.structures)
plotRobustness(robustness = my_robustness, robustness.name = "My Plan", title = TRUE)

filtered.robustness <- selectRobustnessStructures(robustness = my_robustness, keep.structures = structures.to.keep)
plotRobustness(robustness = filtered.robustness, robustness.name = "My Robustness", title = FALSE)

plotRobustnessSpread(robustness = my_robustness, robustness.name = "My Plan", title = TRUE)

rob_d95 <- getStructureRobustness(robustness = my_robustness, dose = 95, structure = "CTV")

# Plan's Energy Layers
energies <- getEnergies("analysis_data/energies.csv")
