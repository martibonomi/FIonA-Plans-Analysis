# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
# 
# Rscript: Analysis workflow example
# --------------------------------------------------------------------------------------

# upload required functions
source("functions.R")

# ------------------------------ Plan's DVHs Visualization -----------------------------

# defining new names for renaming structures of our plan
new.names <- c("CTV", "PTV", "Lungs", "Esophagus", "Heart", "Medulla")
# load plan with new names
my_plan <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", renamed.structures = new.names)

# plot plan with title for my plan
plotDVHs(plan = my_plan, title = TRUE, plan.name = "My Plan")

# defining the structures to keep when filtering our plan
structures.to.keep <- c("CTV", "PTV")
# filter the original plan to keep only CTV and PTV structures
filtered_plan <- selectDVHsStructures(plan = my_plan, keep.structures = structures.to.keep)

# plot filtered plan with a specified title
plotDVHs(plan = filtered_plan, title = "Filtered DVHs")

# ---------------------------- Check quality plan parameters ---------------------------

# calculate V95% for CTV of my plan
v95 <- getVd(plan = my_plan, d = 95, structure = "CTV")

# calculate V107% for PTV of my plan
v107 <- getVd(plan = my_plan, d = 107, structure = "PTV")

# calculate D98% for CTV of my plan
d98 <- getDv(plan = my_plan, v = 98, structure = "CTV")

# calculate D2% for CTV of my plan
d2 <- getDv(plan = my_plan, v = 2, structure = "Lungs")

# ---------------------------- Plan's Robustness Assessment ----------------------------

# defining new names for renaming structures of our plan
new.names <- c("CTV", "PTV", "Lungs", "Esophagus", "Heart", "Medulla")
# load robustness dataframe output from FIonA renaming structures
my_robustness <- readRobustness(robustness.csv = "analysis_data/robustness_dvhs.csv", renamed.structures = new.names)

# plot robustness curves for my structures with no title
plotRobustness(robustness = my_robustness, title = FALSE)

# selecting structures to keep for robustness
structures.to.keep <- c("CTV", "Esophagus", "PTV")
# filtering robustness curves to keep only the ones for selected structures
filtered.robustness <- selectRobustnessStructures(robustness = my_robustness, keep.structures = structures.to.keep)

# plotting robustness curves for filtered structures
plotRobustness(robustness = filtered.robustness, title = "Filtered Robustness")

# plotting worst and best case scenario for robustness curves, together with nominal curves
plotRobustnessSpread(robustness = my_robustness, robustness.name = "My Plan", title = TRUE)

# calulating robustness value at V95% for the worst case scenario
rob_d95 <- getStructureRobustness(robustness = my_robustness, dose = 95, structure = "CTV")

# ------------------------------------- Plans Energies --------------------------------

# get plan's energy layers (total energies and energies per field)
energies <- getEnergies("analysis_data/energies.csv")

# -------------------------------- Plans' DVHs Comparison ------------------------------

# defining new names for renaming structures of our plans
new.names <- c("CTV", "PTV", "Lungs", "Esophagus", "Heart", "Medulla")
# load two different plans
plan1 <- readDVHs(dvhs.csv = "analysis_data/dvhs.csv", renamed.structures = new.names)
plan2 <- readDVHs(dvhs.csv = "analysis_data/dvhs_comparison.csv", renamed.structures = new.names)

# create a list with the two plans to set it as input of the plot function
plans <- list("Plan 1" = plan1, "Plan 2" = plan2)

# plot the DVHs comparison between the two plans
plotComparePlansDVHs(plans = plans, title = TRUE)
