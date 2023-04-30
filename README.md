# FIonA-Plans-Analysis

This repository contains functions implemented in R for processing and visualizing data from proton therapy treatment planning created with the in-house treatment planning system "FIonA" developed at the Paul Scherrer Institute (PSI), Switzerland.

## Introduction

The goal of treatment planning is to deliver a certain amount of dose to the target volume and to minimize the dose to healthy tissues while satisfying prescriptions approved by medical doctors which assign constraints on the maximum and minimum deliverable dose to organs at risk and to the tumor volume, respectively.

To create a plan, different steps must be followed: first, the patient's tumor and surrounding tissues must be accurately contoured to provide information about the location, size, and shape of the tumor as well as the surrounding healthy tissue. In this step the the clinical target volume (CTV) and the planning target volume (PTV) are defined: the CTV represents the volume of the tumor and surrounding tissues that need to be irradiated to achieve an effective treatment, while the PTV is an expansion of the CTV that accounts for uncertainties in the treatment delivery process, such as patient setup errors and organ motion, and it aims at guaranteeing that the correct amount of radiation is delivered to the whole CTV. After that, the plan can be designed using a TPS. All commercially available TPS for PBS follow the same steps: first, fields directions are selected, then plan's optimization is performed to place spots in optimal way to achieve a homogeneous dose distribution within the target volume, the delivered dose is normalized and, in the end, plans' quality is assessed by checking its robustness and whether all medical prescriptions are satisfied or not. After that, once the plan is ready, it is delivered to the patient.

The functions implemented for the analysis and visualization of plans are specific to the PSI's in-house treatment planning system "FIonA".

::: {style="text-align:center"}
![](images/Fiona.png){width="80%"}
:::

## Repository contents

## Installation

## Usage
