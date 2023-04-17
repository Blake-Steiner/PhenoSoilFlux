-------------------------------------------------------------------------------------
Files and Scripts for Steiner et al., Phenology, GPP, and Soil Water Content paper

Primary Author: Blake Steiner (bsteiner@odu.edu)

Date: April 2023

Description:
	Welcome! This readme will serve as a guide to the data and code 
	used for the paper. Below, you will find descriptions of each folder and their 
	contents to provide context. For the sake of space, raw imagery is accessible in
	online repositories such as Google Earth Engine or the Phenocam Network and
	are freely accessible. However, I have included example scripts on how those
	images were processed. As described below, there is a "super script"
	that contains all the steps on how the resulting data from the processed
	images were joined, analyzed, and visualized. A critical component to this 
	is the r "SuperImage" image file under R_images. This is a copy of all
	data frames, vectors, and other R elements needed to reproduce the work and
	speeds things along. Thank you for your time and service!

-------------------------------------------------------------------------------------
Folders:

##################
"data"

	1. Analyses_Figures - This folder has data for each 
	for each figure / table in the paper

	2. FluxTower_data - This folder contains US-SRM flux tower data from 2004-2020 and scripts 
	to process the data

	3. MODIS_GCC - This folder contains the Google Earth Engine Script to collect MODIS GCC 
	data from in and around the flux tower

	4. PhenoCam_GCC - This folder has the data and scripts to process phenocam imagery,
 	normalize it, and derive phenometrics

	5. phenometric_stats_data - This contains all the resulting correlation statistics
	from R scripts: "Phenometric_Analaysis_MODIS" and "Phenometrics_Analysis_Short"

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