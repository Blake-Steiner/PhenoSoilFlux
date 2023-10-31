# "PhenoSoilFlux"
Repository for the reproduction of the work done in the manuscript titled, "Using Phenology to Unravel Differential Soil Water Use and Productivity in a Semiarid Savanna". 
-------------------------------------------------------------------------------------
Files and Scripts for "Using Phenology to Unravel Differential Soil Water Use and
Productivity in a Semiarid Savanna" by Steiner et al., 2023 paper accepted in _Ecosphere_.

Primary Author: Blake Steiner (bsteiner@odu.edu)

Date: April 2023
Modified: October 2023

Description:
	Welcome! This readme will serve as a guide to the data and code 
	used for the paper. Below, you will find descriptions of each folder and their 
	contents to provide context. For the sake of space, imagery is accessible in
	online repositories such as Google Earth Engine or the Phenocam Network and
	are freely accessible. However, I have included example scripts on how those
	images were processed. As described below, there is a "super script"
	that contains all the steps on how the resulting data from the processed
	images were joined, analyzed, and visualized. A critical component to this 
	is the r "SuperImage" image file under R_images. This is a copy of all
	data frames, vectors, and other R elements needed to reproduce the work and
	speeds things along. Thank you for your time!

-------------------------------------------------------------------------------------
Folders:

##################
"FluxTower_data"

	1.  Subfolers: Daily_Data, Data, and MetaData. Dail_Data is the flux data aggregated from 30 minutes to days.
 	Data is the 30 minute, gap-filled partitioned flux data. MetaData defines the headers for the two other 	folders. 
  	
   	2. USSRM_Daily_Flux_2004_2020.csv is the output of the R script in this directory and contains all the 
    	relavent soil water and flux data for analyses. 

     	3. USSRM_Fluxes_Processed.R is the R script that takes in the 30 minute data and outputs daily data and 	calculates other factors. 

##################
"Supplemental_Data_AppendixS1"

	1. Contains a csv file of all the correlations between phenometrics for understory, trees, and ecosystem and 	soil water content at different depeths across different seasons for Table S1 in supplemental material. 

#################
"modis_processing"
	
	1. GEE_MODIS_Script - This text file allows you to copy and paste the code into
	google earth engine (GEE) to retrieve MODIS data. 
	
	2. MODIS_Annual_Phenometric_Script - This script shows how the MODIS
	phenometrics were dervied using the phenopix R package. 
	
	3-5. modis_annual_phenometrics , MODIS_sat_data_all_samples_GCC , and
	MODIS_sat_data_INPUT - are spreadsheets that contain data for MODIS phenometrics,
	GCC for each MODIS pixel, and MODIS input data for the MODIS_Annual_Phenometric_Script
	respectively. 

#################
"phenocam_processing"

	1. Phenometrics - This folder contains the phenometrics derived from the R scripts
	using the phenopix packages.
	
	2. Example_GCC_Phenometric_Script - This script is an example workflow of how
	phenocam images were processed. 

	3. normalize_GCC_Phenocam_Script - This script shows how the data was normalized
	using equations two and three in the paper. 

	4. gcc_flux_2015-20_rerun_2_DOY285 - This spreadsheet is also copied into subfolder
	1. under "data". It contains phenocam and MODIS data joined by daily time stamp to
	flux tower data from 2014 to 2020. 

#################
"R_images"
	
	1. SuperImage - this is an R image file that allows the user to see what my
	R environment was to reproduce figures and analyses. 

#################
"scripts"

	1. Pheno_GPP_SWC_FigureScript - This R script is a shortended version of the SuperScript
	that focuses just on using the SuperImage to remake the manuscript's figures 
	(including supplemental). Use Control F "figure name, such as Figure 1" to go
	to the exact code. 

	2. Pheno_GPP_SWC_SuperScript_2023 - This R script is the super script; it contains 
	nearly all the code used for the paper with the exception of small, individual 
	scripts for processing imagery and deriving all the phenometric correlations
	(those are next). Use of the SuperImage is highly recommended to efficiently 
	connect the inputs and outputs given this was my first time coding. 

	3. Phenometric_Analysis_MODIS - This R script contains all the pairwise
	correlations between SWC and ecosystem level phenometrics. 

	4. Phenometrics_Analysis_phenocam - This R script contains all the pairwise
	correlations between SWC and plant level phenometrics. 

#################
