# BVR GLM-AED

Code for recreating GLM-AED analyses and figures in Beaverdam Reservoir from 2015-2022

# Repo structure

This repository contains 9 main folders: 1) analysis contains the summarized water quality, zooplankton, and phytoplankton output from the model, as well as R scripts needed to recreate all analyses and figures, 2) calibration contains .csv files that are contain the parameters and associated ranges in each module that are used to perform the sensitivity analysis, 3) field_data contains both the raw and cleaned .csv files for different water quality variables that are simulated, 4) glm.app contains the files and executables necessary to run the model on a local computer (Mac only), 5) inputs contains R scripts for interpolating bathymetry data and creating an inflow file, an .Rmd file for creating a daily water level .csv file used in the `InflowPreparation.R`, and associated .csv files necessary to run these scripts, 6) modeling contains all of the scripts that were used for running GLM-AED, performing sensitivity analyses, calibration, validation, and visualizing preliminary model output, 7) results contain .csv files and figures summarizing calibration results for each module, 8) sensitivity contains .csv files for only the sensitive parameters and their ranges that were identified following the sensitivity analysis for each module, and 9) contains the configuration files for the spin-up period vs. non-spin-up period for each of the climate warming scenarios (baseline, plus1, plus5, and plus10).

# Instructions to reproduce figures and analyses
