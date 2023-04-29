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
  
  plot <- ggplot(plan.dvhs, aes(x = Dose, y = value)) +
    geom_line(data = plan.dvhs, aes(color = DVH_curves), linewidth = 0.8) +
    ylim(0,110) +
    xlab("Dose [%]") +
    ylab("Volume [%]") +
    labs(colour = "DVH curves") + 
    theme(text = element_text(size = 15))
  
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


