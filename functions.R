# -------------------------------------------------------------------------------------
# Author: Martina Bonomi
# Date: May 2023
# Rscript: functions definition
#
# Name of the project: Visualization and analysis of "FIonA"'s proton treatment plans
# Aim of the project: to process and visualize proton treatment plans
#     outcome from "FIonA" treatment planning system
# --------------------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
library(reconPlots)
library(covr)
library(assertthat)

readDVHs <- function(dvhs.csv, rename.structures = FALSE, structures.names = NA){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a csv file (FIonA output) with the values of DVHs and the volumes for each structure
  # It returns a list with the DVHs for each structure, the volumes and the names of organs
  # ---------------------------------------------------------------------------------------------
  ## Parameters:
  # dvhs.csv -> .csv fie output from FIonA from DVHs visualization
  # rename.structures [logical] -> TRUE if you want to change the structures names from FIonA,
  #   FALSE if you want to keep FIonA's names
  # structures.names [chr] -> vector with new structures names, only if rename.structures = TRUE
  # --------------------------------------------------------------------------------------------- 
  
  dvhs <- read.csv(dvhs.csv)
  colnames(dvhs)[1] <- "Dose"
  
  if(rename.structures == TRUE){
    for (structure in structures.names) {
      curr.structure.idx <- which(str_detect(colnames(dvhs), regex(structure, ignore_case = TRUE)))
      colnames(dvhs)[curr.structure.idx] <- structure
    }
  }
  
  # create dataframe with structures volumes [cc] from last row of plan
  vol.cc <- tail(dvhs, n = 1)
  vol.cc <- vol.cc[2:length(dvhs)]
  
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
  # It selects only the DVHs for the selected structures from the plan
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan [list] <- plan output from the "readDVHs" function
  # keep.dvhs [chr] -> vector with only structures you want to keep 
  # ---------------------------------------------------------------------------------------------
  
  plan[["DVHs"]] <- plan[["DVHs"]][, c("Dose", keep.structures)]
  plan[["Volumes [cc]"]] <- plan[["Volumes [cc]"]][, keep.structures]
  
  return(plan)
}


plotDVHs <- function(plan, plan.name, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a plan output from the "readDVHs" or "selectDVHsStructures" functions
  # It plots the DVHs for a single plan
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plan [list] -> plan for which you want to plot the DVHs, output of "readDVHs" (all structures 
  #   plotted) or output of "selectDVHsStructures" (only selected structures are plotted)
  # plan.name [chr] <- name of the plan
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
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
  # Reads two or more plans output from the "readDVHs" or "selectDVHsStructures" functions
  # It plots the DVHs comparison between the plans
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # plans [list] -> list of plans for which you want to plot the DVHs, output of "readDVHs" (all
  #   structures are plotted) or output of "selectDVHsStructures" (only selected structures are 
  #   plotted)
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
  # ---------------------------------------------------------------------------------------------
  
  # get names of plans and DVHs
  plans.names <- names(plans)
  
  # create empty dataframe where to store DVHs to plot for each plan
  plans.dvhs <- data.frame()
  
  # loop over plans to create dataframe with data to plot
  for(plan in plans.names){
    
    # gathering together DVHs to plot
    curr.plan  <- plans[[plan]][["DVHs"]]  %>%
      gather(key = "DVH_curves", value = "value", -Dose)
    
    # add column to current dataframe with plan name
    curr.plan$Plan <- plan
    
    # bind dataframe for current plan to final dataframe
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
        title <- paste(title, plans.names[i], sep = " ")
      } else {
        title <- paste(title, plans.names[i], "vs.", sep = " ")
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


readRobustness <- function(robustness.csv, rename.structures = FALSE, structures.names = NA){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads FIonA's output from robustness calculation and re-enumerates the curves of each  
  #   shift scenario for each structure (curve "_5" is the nominal one)
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness.csv -> .csv fie output from FIonA from robustness calculation
  # rename.structures [logical] -> TRUE if you want to change the structures names from FIonA,
  #   FALSE if you want to keep FIonA's names
  # structures.names [chr] -> vector with new structures names, only if rename.structures = TRUE
  # ---------------------------------------------------------------------------------------------
  
  robustness <- read.csv(robustness.csv)
  colnames(robustness)[1] <- "Dose"
  
  if(rename.structures == TRUE){
    
    for(i in 1:length(structures.names)){
      curr.idxs <- which(str_detect(colnames(robustness), regex(structures.names[i], ignore_case = TRUE)))
      for(j in 1:length(curr.idxs)){
        colnames(robustness)[curr.idxs[j]] <- paste(structures.names[i], j, sep = "_")
      }
    }
    
  } else {
    
    # get unique original names for structures
    orig.structures.names <- colnames(robustness)[-1]
    list.structures.names <- str_split(orig.structures.names, pattern = "[.]")
    structures.names <- c()
    for(i in 1:length(list.structures.names)){
      structures.names[i] <- list.structures.names[[i]][1]
    }
    structures.names <- unique(structures.names)
    
    for(i in 1:length(structures.names)){
      curr.idxs <- which(str_detect(colnames(robustness), structures.names[i]))
      for(j in 1:length(curr.idxs)){
        colnames(robustness)[curr.idxs[j]] <- paste(structures.names[i], j, sep = "_")
      }
    }
    
  }
  
  robustness[is.na(robustness)] <- 0
  
  return(robustness)
  
}


selectRobustnessStructures <- function(robustness, keep.structures){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It selects only the robustness shifts for the selected structures from the robustness 
  #   calculation
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness <- robustness dataframe output from "readRobustness" function
  # keep.structures [chr] -> vector with only structures you want to keep 
  # ---------------------------------------------------------------------------------------------
  
  for(str in keep.structures){
    keep.idxs <- c(1, which(str_detect(colnames(robustness), str))) # idx 1 is for Dose
  }
  
  robustness <- robustness[, keep.idxs]
  
  return(robustness)
  
}


plotRobustness <- function(robustness, robustness.name, title = TRUE){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # Reads a robustness file output from the "readRobustness" function and plots curves for 
  #   geometrical shifts for each curve
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the DVHs, output of 
  #   "readRobustness" (all structures plotted) or output of "selectDVHsStructures"
  #   (only selected structures are plotted)
  # robustness.name [chr] <- name of the plan for which you calculated the robustness
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
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
  #   case scenario of geometrical shifts
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the DVHs, output of 
  #   "readRobustness" (all structures plotted) or output of "selectRobustnessStructures"
  #   (only selected structures are plotted)
  # ---------------------------------------------------------------------------------------------
  
  spread.df <- data.frame(Dose = robustness$Dose)
  
  structure.names <- unique(gsub("_[0-9]+$", "", names(robustness)[-1]))
  
  # find max and min for each point of each structure
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
  # the worst and best case scenarios of geometrical shifts for each curve
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # robustness -> robustness dataframe for which you want to plot the spread, output 
  #   of "readRobustness" (all structures plotted) or output of "selectDVHsStructures"
  #   (only selected structures are plotted)
  # robustness.name [chr] <- name of the plan for which you calculated the robustness spread
  # title [chr or logical] -> title for the plot, either TRUE, FALSE or character
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


getVd <- function(dvhs, d, structure){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It finds the V[d%] value for the selected structure of the current plan you are using
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # dvhs -> dvhs list of the plan for which you want to calculate V[d%], output of "readPlan" 
  #   function
  # d [num] <- dose percentage for which you want to compute the volume, numeric value
  # structure [chr] -> structure for which you want to compute V[d%] 
  # ---------------------------------------------------------------------------------------------
  
  structures <- colnames(dvhs[["DVHs"]][-1])
  
  str.idx <- which(str_detect(structures, structure))
  selected.structure <- dvhs[["DVHs"]][, str.idx + 1]
  
  dose.idx <- which(dvhs[["DVHs"]]$Dose == d)
  
  Vd <- selected.structure[dose.idx]
  
  return(Vd)
  
}


getDv <- function(dvhs, v, structure){
  
  # ---------------------------------------------------------------------------------------------
  # Function's description:
  # It finds the D[v%] value for the selected structure
  # ---------------------------------------------------------------------------------------------
  # Parameters:
  # dvhs -> dvhs list of the plan for which you want to calculate D[v%], output of "readPlan" 
  #   function
  # v [num] <- volume percentage for which you want to compute the dose, numeric value
  # structure [chr] -> structure for which you want to compute D[v%]
  # ---------------------------------------------------------------------------------------------
  
  # get names of structures in the plan file
  structures <- colnames(dvhs[["DVHs"]][-1])
  
  # get column idx for wanted structure
  str.idx <- which(str_detect(structures, structure))
  
  # get structure volume and dose values
  dose <- dvhs[["DVHs"]]$Dose
  vol = as.vector(dvhs[["DVHs"]][str.idx + 1])[[1]]
  
  vol <- approxfun(dose, vol)
  
  # find intersection with v value
  Dv <- uniroot(function(dose) fun(dose) - v, c(min(dose), max(dose)))$root
  
  return(Dv)
  
}





