# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
#
# Rscript: functions definition
# --------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(covr)
library(testthat)


readDVHs <- function(dvhs.csv, renamed.structures = NA){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a csv file (FIonA output) with the values of DVHs and the volumes for each structure 
  #   and stores the data into a list for plan's analysis
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # dvhs.csv -> csv file output from FIonA from DVHs visualization
  # renamed.structures [chr] -> vector with new structures' names, only if you want to rename 
  #   them (default is NA)
  # --------------------------------------------------------------------------------------------- 
  # Returns:
  # A list with two elements: a dataframe called "DVHs" with the DVHs for each structure and 
  #   a dataframe called "Volumes [cc]" with the volumes of each structure in cc units
  # --------------------------------------------------------------------------------------------- 
  
  dvhs <- read.csv(dvhs.csv)
  colnames(dvhs)[1] <- "Dose"
  
  # renaming structures
  if(all(!is.na(renamed.structures))){
    for (structure in renamed.structures) {
      curr.structure.idx <- which(str_detect(tolower(colnames(dvhs)), tolower(structure)))
      colnames(dvhs)[curr.structure.idx] <- structure
    }
  }
  
  # create dataframe with structures volumes [cc] from last row of plan
  vol.cc <- tail(dvhs, n = 1)
  vol.cc <- vol.cc[2:length(dvhs)]
  rownames(vol.cc) <- "Vol"
  
  # keep only dvhs dataframe
  dvhs <- dvhs[1:nrow(dvhs)-1,]
  dvhs$Dose <- as.numeric(dvhs$Dose) # convert dose to num (from chr)
  
  plan <- list(dvhs, vol.cc)
  names(plan) <- c("DVHs", "Volumes [cc]")
  
  return(plan)
  
}


selectDVHsStructures <- function(plan, keep.structures){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It selects only the DVHs for the selected structures from the plan you provide as input
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan [list] <- plan output from the "readDVHs" function
  # keep.structures [chr] -> vector with only structures you want to keep 
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # The same plan you provided as input (output of "readDVHs") but with only the DVHs and volumes 
  #   selected with keep.structures
  # --------------------------------------------------------------------------------------------- 
  
  plan[["DVHs"]] <- plan[["DVHs"]][, c("Dose", keep.structures)]
  plan[["Volumes [cc]"]] <- plan[["Volumes [cc]"]][, keep.structures]
  
  return(plan)
}


plotDVHs <- function(plan, plan.name, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a plan output from the "readDVHs" or "selectDVHsStructures" functions and plots the 
  #   DVHs for a single plan
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan [list] -> plan for which you want to plot the DVHs, output of "readDVHs" (all structures 
  #   plotted) or output of "selectDVHsStructures" (only selected structures are plotted)
  # plan.name [chr] <- name of the plan
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A plot with the DVHs of the plan you provided as input
  # --------------------------------------------------------------------------------------------- 
  
  plan.dvhs  <- plan[["DVHs"]]  %>%
    gather(key = "DVH_curves", value = "value", -Dose)
  
  # set DVHs' names as factors to plot them in a given order (CTV and PTV first, then organs at risk)
  if("PTV" %in% unique(plan.dvhs$DVH_curves) & "CTV" %in% unique(plan.dvhs$DVH_curves)){
    ptv.idx <- which(unique(plan.dvhs$DVH_curves == "PTV"))
    ctv.idx <- which(unique(plan.dvhs$DVH_curves == "CTV"))
    plan.dvhs$DVH_curves <- factor(plan.dvhs$DVH_curves, levels = c("CTV", "PTV", sort(unique(plan.dvhs$DVH_curves)[-c(ptv.idx, ctv.idx)])))
  }
  
  # create plot
  plot <- ggplot(plan.dvhs, aes(x = Dose, y = value)) +
    geom_line(data = plan.dvhs, aes(color = DVH_curves), linewidth = 0.8) +
    ylim(0,110) +
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    labs(colour = "DVHs") + 
    theme(text = element_text(size = 15))
  
  # add title
  if(title == TRUE){
    plot <- plot + ggtitle(paste("DVHs for plan", plan.name, sep = " "))
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  return(plot)
  
}


plotComparePlansDVHs <- function(plans, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads two or more plans output from the "readDVHs" or "selectDVHsStructures" functions and
  #   it plots the DVHs comparison between the plans
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plans [list] -> list of plans for which you want to plot the DVHs, output of "readDVHs" (all
  #   structures are plotted) or output of "selectDVHsStructures" (only selected structures are 
  #   plotted)
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A plot with the comparison of DVHs of the plans you provided as input
  # --------------------------------------------------------------------------------------------- 
  
  plans.names <- names(plans)
  
  plans.dvhs <- data.frame()
  
  # loop over plans to create dataframe with data to plot with plans' label 
  for(plan in plans.names){
    
    curr.plan  <- plans[[plan]][["DVHs"]]  %>%
      gather(key = "DVH_curves", value = "value", -Dose)
    
    curr.plan$Plan <- plan
    
    plans.dvhs <- rbind(plans.dvhs, curr.plan)
  }
  
  # set DVHs' names as factors to plot them in a given order (CTV and PTV first, then organs at risk)
  if("PTV" %in% unique(plans.dvhs$DVH_curves) & "CTV" %in% unique(plans.dvhs$DVH_curves)){
    ptv.idx <- which(unique(plans.dvhs$DVH_curves == "PTV"))
    ctv.idx <- which(unique(plans.dvhs$DVH_curves == "CTV"))
    plans.dvhs$DVH_curves <- factor(plans.dvhs$DVH_curves, levels = c("CTV", "PTV", sort(unique(plans.dvhs$DVH_curves)[-c(ptv.idx, ctv.idx)])))
  }
  
  # create plot
  plot <- ggplot(plans.dvhs, aes(x = Dose, y = value)) +
    geom_line(data = plans.dvhs, aes(color = DVH_curves, linetype = Plan), linewidth = 0.8) +
    ylim(0,110) +
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    labs(colour = "DVH curves", linetype = "Plan") + 
    theme(text = element_text(size = 15))
  
  # add title
  if(title == TRUE){
    title <- c()
    for(i in 1:length(plans.names)){
      if(i == length(plans.names)){
        title <- paste(title, plans.names[i], sep = "")
      } else {
        title <- paste(title, plans.names[i], " vs. ", sep = "")
      }
    }
    plot <- plot + ggtitle(title)
  } else if(title == FALSE){
    plot <- plot
  } else {
    plot <- plot + ggtitle(title)
  }
  
  return(plot)
  
}


readRobustness <- function(robustness.csv, renamed.structures = NA){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads FIonA's output from robustness calculation and re-enumerates the curves of each  
  #   shift scenario for each structure (curve "_5" is the nominal one)
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness.csv -> csv fie output from FIonA from robustness calculation
  # renamed.structures [chr] -> vector with new structures' names, only if you want to rename 
  #   them (default is NA)
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A dataframe with the robustness curves re-enumerated from 1 to 9 for each structure and 
  #   renamed (only if specified)
  # --------------------------------------------------------------------------------------------- 
  
  robustness <- read.csv(robustness.csv)
  colnames(robustness)[1] <- "Dose"
  
  # get unique names for structures
  structures.names <- unique(sapply(str_split(colnames(robustness)[-1], "[.]"), "[[", 1))
  
  # rename structures
  if(all(!is.na(renamed.structures))){
    for (structure in renamed.structures) {
      curr.structure.idx <- which(str_detect(tolower(structures.names), tolower(structure)))
      structures.names <- gsub(structures.names[curr.structure.idx], structure, structures.names)
    }
  }
  
  # re-enumerating curves
  for(i in 1:length(structures.names)){
    curr.idxs <- which(str_detect(colnames(robustness), structures.names[i]))
    for(j in 1:length(curr.idxs)){
      colnames(robustness)[curr.idxs[j]] <- paste(structures.names[i], j, sep = "_")
    }
  }
  
  robustness[is.na(robustness)] <- 0
  
  return(robustness)
  
}


selectRobustnessStructures <- function(robustness, keep.structures){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It selects only the robustness curves for the selected structures from FIonA's robustness 
  #   calculation
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness <- robustness dataframe output from "readRobustness" function
  # keep.structures [chr] -> vector with only structures you want to keep 
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # The same robustness dataframe you provided as input (output of "readRobustness") but with 
  #   only the robustness curves of structures selected with keep.structures
  # --------------------------------------------------------------------------------------------- 
  
  keep.idxs <- c()
  for(str in keep.structures){
    keep.idxs <- c(keep.idxs, which(str_detect(colnames(robustness), str)))
  }
  
  robustness <- robustness[, c(1, keep.idxs)] # idx 1 is for Dose
  
  return(robustness)
  
}


plotRobustness <- function(robustness, robustness.name, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a robustness object output from the "readRobustness" function and plots curves for 
  #   geometrical shifts for each curve
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the DVHs, output of 
  #   "readRobustness" (all structures plotted) or output of "selectRobustnessStructures"
  #   (only selected structures are plotted)
  # robustness.name [chr] <- name of the plan for which you calculated the robustness
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A plot with the robustness curves of the robustness dataframe you provided as input
  # --------------------------------------------------------------------------------------------- 
  
  robustness  <- robustness  %>%
    gather(key = "DVH_curves", value = "value", -Dose)
  
  # add unique structures column
  robustness$Structure <- NA
  structures.names <- c()
  curves.names <- robustness$DVH_curves
  for(i in 1:length(curves.names)){
    robustness$Structure[i] <- str_split(curves.names[i], pattern = "_")[[1]][1]
  }
  
  # set DVHs' names as factors to plot them in a given order (CTV and PTV first, then organs at risk)
  if("PTV" %in% unique(robustness$Structure) & "CTV" %in% unique(robustness$Structure)){
    ptv.idx <- which(unique(robustness$Structure) == "PTV")
    ctv.idx <- which(unique(robustness$Structure) == "CTV")
    robustness$Structure <- factor(robustness$Structure, levels = c("CTV", "PTV", sort(unique(robustness$Structure)[-c(ptv.idx, ctv.idx)])))
  }
  
  # create plot
  plot <- ggplot(robustness, aes(x = Dose, y = value, group = DVH_curves)) +
    geom_line(data = robustness, aes(group = DVH_curves, color = Structure), linewidth = 0.6) +
    ylim(0,110) +
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    labs(colour = "Robustness DVHs")
  
  # add title
  if(title == TRUE){
    plot <- plot + ggtitle(paste("Robustness DVHs for plan", robustness.name, sep = " "))
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  return(plot)
  
}


findRobustnessSpread <- function(robustness){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a robustness file output from the "readRobustness" function and finds best and worst
  #   case scenario of geometrical shifts (maxima and minima for each point of the 9 curves for 
  #   each structure)
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the robustness DVHs, output of 
  #   "readRobustness" (all structures plotted) or output of "selectRobustnessStructures"
  #   (only selected structures are plotted)
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A dataframe with three columns for each structure: *str*_nom is the nominal robustness curve
  #   (curve number 5 from "readRobustness" output), *str*_max contains the maximum values between
  #   all the 9 robustness curves for each dose value, and *str*_min contains the minimum values 
  #   between all the 9 robustness curves for each dose value
  # --------------------------------------------------------------------------------------------- 
  
  spread.df <- data.frame(Dose = robustness$Dose)
  
  structure.names <- unique(gsub("_[0-9]+$", "", names(robustness)[-1]))
  
  # find max and min for each point of the 9 curves of each structure
  for (str in structure.names) {
    col.indices <- grepl(paste0(str, "_"), names(robustness))
    curr.str.df <- data.frame(
      nom = robustness[, col.indices & grepl("_5", names(robustness))],
      max = apply(robustness[, col.indices], 1, max),
      min = apply(robustness[, col.indices], 1, min)
    )
    colnames(curr.str.df) <- paste0(str, "_", colnames(curr.str.df))
    
    spread.df <- cbind(spread.df, curr.str.df)
  }
  
  return(spread.df)
  
}


plotRobustnessSpread <- function(robustness, robustness.name, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a robustness dataframe output from the "readRobustness" function and plots curves for
  #   the worst and best case scenarios of geometrical shifts for each curve
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the spread, output 
  #   of "readRobustness" (all structures plotted) or output of "selectRobustnessStructures"
  #   (only selected structures are plotted)
  # robustness.name [chr] <- name of the plan for which you calculated the robustness spread
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A plot with the nominal robustness curve and the robustness best case scenario and robustness 
  #   worst case scenario curves for each structure
  # --------------------------------------------------------------------------------------------- 
  
  # find robustness spread
  robustness.spread <- findRobustnessSpread(robustness)
  
  robustness.spread <- robustness.spread %>%
    gather(key = "DVH_curves", value = "value", -Dose)
  
  # add structures to dataframe 
  structures.names <- unique(gsub("_(nom|min|max)+$", "", robustness.spread$DVH_curves))
  robustness.spread$Structure <- NA
  for(str in structures.names){
    robustness.spread$Structure[which(str_detect(robustness.spread$DVH_curves, str))] <- str
  }
  
  # set DVHs' names as factors to plot them in a given order (CTV and PTV first, then organs at risk)
  if("PTV" %in% unique(robustness.spread$Structure) & "CTV" %in% unique(robustness.spread$Structure)){
    ptv.idx <- which(unique(robustness.spread$Structure) == "PTV")
    ctv.idx <- which(unique(robustness.spread$Structure) == "CTV")
    robustness.spread$Structure <- factor(robustness.spread$Structure, levels = c("CTV", "PTV", sort(unique(robustness.spread$Structure)[-c(ptv.idx, ctv.idx)])))
  }
  
  # add curves' types to dataframe 
  curve.type <- c("nom", "max", "min")
  robustness.spread$Curve <- NA
  for(typ in curve.type){
    robustness.spread$Curve[which(str_detect(robustness.spread$DVH_curves, typ))] <- typ
  }
  
  robustness.spread$Curve <- gsub("max", "Best case scenario", robustness.spread$Curve)
  robustness.spread$Curve <- gsub("min", "Worst case scenario", robustness.spread$Curve)
  robustness.spread$Curve <- gsub("nom", "Nominal curve", robustness.spread$Curve)
  
  robustness.spread$Curve <- factor(robustness.spread$Curve, levels = c("Nominal curve", "Best case scenario", "Worst case scenario"))
  
  # create plot
  plot <- ggplot(robustness.spread, aes(x = Dose, y = value, group = DVH_curves)) +
    geom_line(data = robustness.spread, aes(group = DVH_curves, color = Structure, linetype = Curve), linewidth = 0.6) +
    ylim(0,110) +
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    labs(colour = "Structures", linetype = "Curve type")
  
  # add title
  if(title == TRUE){
    plot <- plot + ggtitle(paste("Robustness spread for plan", robustness.name, sep = " "))
  } else if(title == FALSE){
    plot <- plot
  } else{
    plot <- plot + ggtitle(title)
  }
  
  return(plot)
  
}


getVd <- function(plan, d, structure){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It finds the V[d%] value for the selected structure of the current plan you are analyzing
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan -> plan for which you want to calculate V[d%], output of "readDVHs" function
  # d [num] <- dose percentage for which you want to compute the volume
  # structure [chr] -> structure for which you want to compute V[d%] 
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A numerical value which corresponds to the V[d%] value for the selected structure of the 
  #   plan you are analyzing
  # --------------------------------------------------------------------------------------------- 
  
  structures <- colnames(plan[["DVHs"]][-1])
  
  str.idx <- which(str_detect(structures, structure))
  selected.structure <- plan[["DVHs"]][, str.idx + 1]
  
  dose.idx <- which(plan[["DVHs"]]$Dose == d)
  
  Vd <- selected.structure[dose.idx]
  
  return(Vd)
  
}


getDv <- function(plan, v, structure){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It finds the D[v%] value for the selected structure of the current plan you are analyzing
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan -> plan for which you want to calculate D[v%], output of "readDVHs" 
  #   function
  # v [num] <- volume percentage for which you want to compute the dose
  # structure [chr] -> structure for which you want to compute D[v%]
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A numerical value which corresponds to the D[v%] value for the selected structure of the 
  #   plan you are analyzing
  # --------------------------------------------------------------------------------------------- 
  
  structures <- colnames(plan[["DVHs"]][-1])
  
  str.idx <- which(str_detect(structures, structure))
  
  dose <- plan[["DVHs"]]$Dose
  vol = as.vector(plan[["DVHs"]][str.idx + 1])[[1]]
  
  # interpolate curve for volume as it has a finite number of points
  vol <- approxfun(dose, vol)
  
  # find intersection with v value
  Dv <- uniroot(function(dose) vol(dose) - v, c(min(dose), max(dose)))$root
  
  return(Dv)
  
}


getStructureRobustness <- function(robustness, dose, structure){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It finds the value of the robustness curve for the worst case scenario for the threshold 
  #   value you want to check for a selected structure (generally only CTV and PTV are checked)
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe from plan for which you want to check robustness
  # value [num] <- dose threshold value for which you want to check robustness
  # structure [chr] -> structure for which you want to calculate the robustness (generally only
  #   CTV and PTV are used)
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A numerical value which corresponds to the value of the robustness worst case scenario curve
  #   corresponding to the dose you are interested in
  # --------------------------------------------------------------------------------------------- 
  
  robustness.spread <- findRobustnessSpread(robustness)
  
  str.idx <- which(colnames(robustness.spread) == paste(structure, "min", sep = "_"))
  dose.idx <- which(robustness.spread$Dose == dose)
  
  value <- robustness.spread[dose.idx, str.idx]
  
  return(value)
  
}


getEnergies <- function(energies.csv){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It provides the list of total number of energy layers for the whole plan and for each field
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # energies.csv -> .csv file output from FIonA with energies, spots and weights
  # ---------------------------------------------------------------------------------------------
  # Returns:
  # A list with vectors: a vector "Total energies" with the sorted values of the total energy 
  #   layers, and vectors "FX" with the sorted values of the energy layers for fields "FX"
  #   (number of fields' vectors depends on the number of fields of your plan)
  # --------------------------------------------------------------------------------------------- 
  
  energies <- read.csv(energies.csv)
  
  total.energies <- sort(unique(energies$Energy..MeV.))
  energies$Field <- gsub(" ", "", energies$Field)
  fields <- sort(unique(energies$Field))

  list.energies <- list()
  list.energies[["Total energies"]] <- total.energies
  
  for(field in fields){
    curr.field <- energies[which(energies$Field %in% field),]
    list.energies[[field]] <- sort(unique(curr.field$Energy..MeV.))
  }
  
  return(list.energies)
  
}

