###############################

# Name: Blake Steiner
# Date Updated: April 11th, 2023
# Objective:
#   Script to organize and process of all data in Steiner et al., 2023 paper

# BE SURE TO SET WORKING DIRECTORIES 

##############################

####################
# PACKAGES
###################

#specify the packages of required for this analysis
packages = c("tidyverse","dplyr","lubridate","RcppRoll",
             "reshape2", "gridExtra","ggpubr","lsmeans","plyr", 
             "RColorBrewer", "car", "PerformanceAnalytics",
             "GGally", "egg", "cowplot", "ggdist",
             "distributional", "broom")


#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
rm(package.check);rm(packages)


#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot") #for here, ignore updates
library("ggcorrplot")

#devtools::install_github("braverock/PerformanceAnalytics")

#######################
# FUNCTIONS
######################

#coefficient of variation 
coef_var <- function(x){
  (sd(x, na.rm = T) / mean(x, na.rm = T))
  
}

#standard error
standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x)) #Standard Error, Barnes et al., 2016 for Max EVI across all years

#covariance error
COV_error <- function(x) sd(x, na.rm = T) / mean(x, na.rm = T)

#function to copy and paste a data frame to excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#Substitute NAs to NaNs #function to find NaNs and make them NAs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#function to do a custom correlation matrix
mychart.Correlation <- function (R, method = c("pearson", "kendall", "spearman"), ...){
  
  x = PerformanceAnalytics::checkData(R, method = "matrix")
  if (missing(method)) method = method[1]
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", 
                        use = "pairwise.complete.obs", 
                        method = cormeth, 
                        cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                     symbols = c("***", "**", "*", ".", " "))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  
  pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, ...)
  
}

#######################
# PLOT THEMES
######################

#General theme
plot.theme <- theme(axis.title.x = element_text(color = "black", 
                                                size = 12), 
                    axis.title.y = element_text(color = "black", 
                                                size = 12),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    legend.position = c(0.3,1),
                    legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 14, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))


#Climatology
plot.theme.cli <- theme(axis.title.x = element_text(color = "black", 
                                                size = 14), 
                    axis.title.y = element_text(color = "black", 
                                                size = 14,
                                                hjust = 0.6,
                                                vjust = 1),                                                                       
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    legend.position = c(1,1),
                    legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 14, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Theme for models
plot.theme.model <- theme(axis.title.x = element_text(color = "black", 
                                                size = 14), 
                    axis.title.y = element_text(color = "black", 
                                                size = 14),                                                                       
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    legend.position = c(1,1),
                    legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 14, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))


#theme for phenometrics and GPP
plot.theme_phenoMet_gpp <- theme(axis.title.x = element_text(color = "black", 
                                                size = 16), 
                    axis.title.y = element_text(color = "black", 
                                                size = 16),                                                                       
                    axis.text.x = element_text(size = 16),
                    axis.text.y = element_text(size = 16),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 16, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="black", fill="white", linetype="solid"
                    )
)


#theme for plotting modis phenometrics
plot.theme.modis.phenometric <- theme(axis.title.x = element_text(color = "black", 
                                                size = 14), 
                    axis.title.y = element_text(color = "black", 
                                                size = 14),                                                                       
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 16, hjust = 0.5),
                    #panel.grid.major = element_blank(), 
                    #panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="black", fill="white", linetype="solid"
                    )
)



# time series (ts) theme
plot.theme.ts <- theme(axis.title.x = element_text(color = "black", 
                                                size = 16), 
                    axis.title.y = element_text(color = "black", 
                                                size = 16),                                                                       
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    legend.position = "top",
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 12, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 12, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 12, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="white", fill="white", linetype="solid"
                    )
)

#general phenometrics themes
plot.theme.phenometric <- theme(axis.title.x = element_text(color = "black", 
                                                size = 16), 
                    axis.title.y = element_text(color = "black", 
                                                size = 16),                                                                       
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 14),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 18, hjust = 0.5, face = "bold"),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 14, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="black", fill="white", linetype="solid"
                    )
)



##################################
# FLUX TOWER PROCESSING - Get Daily Data
#################################

#set working directory
setwd("F:/is/for/friends/")


#Set input and output directories 
DirInp <- "F:/ / /"

OutDir <- "F:/ / /"


#Read in a vector of file names from your input directory
files <- sapply(strsplit(list.files(paste(DirInp, "/Data/",  sep="")),"*.csv"),
                function(x) paste(x[1],sep=""))


#see file names in console
print(files)


# NOTE: If you want to work with just one file, see the functions INSIDE the loop

#Grab command line arguments
args <- commandArgs(trailingOnly=TRUE)

#Print statement for testing
print(args)

# test if there is at least one argument (i.e a file to work with): if not, return an error
if (length(args)>1) {
  whr_file <- as.integer(args[1]):as.integer(args[2])
} else {
  #which files to process?
  whr_file <- 1:17  
}


#Vector for output names
years <- c(2004:2020) #colon will return the first and last number in sequence, if want to skip numbers
                      # do c(2004, 2006, 2010:2020)

###
#start loop around files
runner <- 0 #this tells the loop which file to start from


#create mega data frame to rbind (bind by rows) all data to
OUT <- data.frame() 

for (FILE in whr_file) {
  
  #For testing purposes, remove the comment from FILE to just use a specific input csv or table
  FILE <- 1 
  runner <- runner + 1
  
  #For naming the year of the fluxes
  year <- years[FILE]
  
  #Read in the data
  
  flux_df <- read.csv(file = paste(DirInp, "Data/", files[FILE], ".csv", sep = ""), #what is the file name 
                      skip = 0, #do you want to skip any rows, starting from the top?
                      header = T, #are there column headers? 
                      na.strings = -9999.00, #What are NAs in the csv?
                      stringsAsFactors = FALSE, #should data be read in as categorical data?
                      sep = "," # what file format are things seperated by, a comma (csv), a '.', etc.?
                      ) 
  
  #-------Process file------


  #Create a grouped time vector
  flux_df$timeA <- substring(flux_df$TIMESTAMP_START, 1, 
                                nchar(flux_df$TIMESTAMP_START)) #pull out specific set of numbers 

  flux_df$timeA <- lubridate::ymd_hm(flux_df$timeA) #use this function from the lubridate package to organize time
  flux_df$timeA <- strptime(flux_df$timeA, format="%Y-%m-%d %H:%M", tz="GMT") #format it
  flux_df$timeA <- as.POSIXct(flux_df$timeA) #change time class to a POSIX data class for easier work


  #Calculate VPD:
  eo=0.611 #; %kPa
  To=273 #; % K
  L=2.5e6 #; % J kg^-1
  Rv=461 #; % J K^-1 kg^-1
  t_air = flux_df$TA_1_2_1 # in C, the $ calls to a specific column in a data frame
  vpres = flux_df$PA #in kPa
  rh = flux_df$RH_1_2_1 #% relative humidity

  e_sat = eo*exp(L/Rv*((1/To)-(1./(t_air+273.15))))

  e_actual = (rh * e_sat) / 100

  VPD = e_sat - e_actual

  rh = 100 * (e_actual / e_sat)#test it

  
  #TEST PLOTS
  #plot(x = t_air, y = VPD) #in kPa

  #plot(x = VPD_matt, y = VPD)

  flux_df <- cbind(VPD, flux_df) #binding the VPD vector just calcuted to the main data frame as a column

  #Rename variable

  flux_df <- plyr::rename(flux_df,
                  replace= c("timeA" = "Time"))


  #create day of year vector
  flux_df$DOY <- lubridate::yday(flux_df$Time)


  #Create Evaporative Fraction column from Dr. Young NAU

  EF <- flux_df$LE / (flux_df$LE + flux_df$H)

  flux_df$EF <- EF
 
  
  #-------Process file------
  #remove last day of previous year
  flux_df <- flux_df[-1,] #this removes the first row of a dataframe
  
  #Substitute NAs to NaNs

  flux_df[is.nan(flux_df)] <- NA #turns any NaNs into NAs
  
  
  #Create empty output data frame
  flux_daily_df <- data.frame()
  
  #test difference of day vs day and night values
  flux_Nofilter <- flux_df %>% #this takes the flux_df data frame and pipes it to another function
    select(DOY, PPFD_IN, PPFD_IN_F, #this selects certain columns
           GPP, RECO, Time) %>%
    #filter(PPFD_IN_F > 30) %>% #if you want to subset or filter by something
    group_by(DOY) %>% #how do you want to group the variables by
    dplyr::summarise(ppfd_in_avg = mean(PPFD_IN, na.rm = T),
              ppfd_in_sd =  sd(PPFD_IN, na.rm = T),
              ppfd_in_f_avg = mean(PPFD_IN_F, na.rm = T),
              ppfd_in_f_sd =  sd(PPFD_IN_F, na.rm = T),
              gpp_avg = mean(GPP, na.rm = T),
              gpp_sd = sd(GPP, na.rm = T),
              reco_avg = mean(RECO, na.rm = T),
              reco_sd = sd(RECO, na.rm = T) #create new columns based on a calculation
    ) %>%
    mutate(date = base::as.Date(x = DOY, origin = paste(year, "-", "01-01", sep = "")) - 1 #used to correct date
    ) #also a way to create a new column
  
  plot(x = flux_Nofilter$DOY, y = flux_Nofilter$gpp_avg) #basic plot

  #calcuate SWC from all and certain depths
  flux_env_df <- flux_df %>%
    select(DOY, VPD, TA_1_2_1, P,
           SWC_1_1_A, # 2.5 - 5 cm depth
           SWC_1_2_A, # 5 - 10 cm depth
           SWC_1_3_A, # 15 - 20 cm depth
           SWC_1_4_A, # 25 - 30 cm depth
           SWC_1_5_A, # 45 - 50 cm depth
           SWC_1_6_A, # 60 - 70 cm depth
           SWC_1_7_A, # 90 - 100 cm depth
           SWC_1_8_A, # 130 cm depth, grass not known to go this far,
           TS_1_1_A, # 2.5 - 5 cm depth soil temp
           NEE, EF, Time) %>%
    rowwise() %>% #Group all  columns via rows to average selected columns across one row together
    mutate(swc_all_avg = mean(c(SWC_1_1_A,
                                SWC_1_2_A,
                                SWC_1_3_A,
                                SWC_1_4_A,
                                SWC_1_5_A,
                                SWC_1_6_A,
                                SWC_1_7_A,
                                SWC_1_8_A), 
                              na.rm = T),
           swc_shallow_avg = mean(c(SWC_1_1_A,
                                    SWC_1_2_A,
                                    SWC_1_3_A,
                                    SWC_1_4_A), 
                                  na.rm = T),
           swc_deep_avg = mean(c(SWC_1_5_A,  #DEEP BEING 45 cm to 130 cm
                                 SWC_1_6_A,
                                 SWC_1_7_A,
                                 SWC_1_8_A), 
                              na.rm = T)
    )
    
    #Bring all daily averages together
    
    flux_env_df <- flux_env_df %>%
    group_by(DOY) %>%
    dplyr::summarise(vpd_avg = mean(VPD, na.rm = T),
              vpd_sd =  sd(VPD, na.rm = T),
              Tair_avg = mean(TA_1_2_1, na.rm = T),
              Tair_sd =  sd(TA_1_2_1, na.rm = T),
              nee_avg = mean(NEE, na.rm = T),
              nee_sd = sd(NEE, na.rm = T),
              swc_2.5_10_avg = mean(SWC_1_1_A, na.rm = T),
              swc_2.5_10_sd = sd(SWC_1_1_A, na.rm = T),
              swc_5_10_avg = mean(SWC_1_2_A, na.rm = T),
              swc_5_10_sd = sd(SWC_1_2_A, na.rm = T),
              swc_15_20_avg = mean(SWC_1_3_A, na.rm = T),
              swc_15_20_sd = sd(SWC_1_3_A, na.rm = T),
              swc_25_30_avg = mean(SWC_1_4_A, na.rm = T),
              swc_25_30_sd = sd(SWC_1_4_A, na.rm = T),
              swc_45_50_avg = mean(SWC_1_5_A, na.rm = T),
              swc_45_50_sd = sd(SWC_1_5_A, na.rm = T),
              swc_60_70_avg = mean(SWC_1_6_A, na.rm = T),
              swc_60_70_sd = sd(SWC_1_6_A, na.rm = T),
              swc_90_100_avg = mean(SWC_1_7_A, na.rm = T),
              swc_90_100_sd = sd(SWC_1_7_A, na.rm = T),
              swc_130_avg = mean(SWC_1_8_A, na.rm = T),
              swc_130_sd = sd(SWC_1_8_A, na.rm = T),
              Tsoi_2.5_10_avg = mean(TS_1_1_A, na.rm = T),
              Tsoi_2.5_10_sd = sd(TS_1_1_A, na.rm = T),
              precip = sum(P, na.rm = T),
              ef_avg = mean(EF, na.rm = T),
              ef_sd = sd(EF, na.rm = T),
              swc_all_sd = sd(swc_all_avg, na.rm = T),
              swc_all_avg = mean(swc_all_avg, na.rm = T),
              swc_shallow_sd = sd(swc_shallow_avg, na.rm = T),
              swc_shallow_avg = mean(swc_shallow_avg, na.rm = T),
              swc_deep_avg = mean(swc_deep_avg, na.rm = T)
              
              
    ) %>%
    mutate(date = base::as.Date(x = DOY, origin = paste(year, "-", "01-01", sep = "")) - 1 #used to correct date
    )
  
  #plot(x = flux_env_df$DOY, flux_env_df$nee_avg)
  
  #Join data frames (dfs)
  
  flux_daily_df <- rbind(flux_env_df) 
  
  flux_daily_df <- full_join(flux_Nofilter, flux_env_df)
  
  #Clean up
  flux_daily_df[is.nan(flux_daily_df)] <- NA
  
  rm(flux_env_df, flux_filter, flux_df) #removes stuff from your global environment
  
  dev.off() #clears plots
  
  #if else statement to know how to loop files together 
  if(runner == 1){
    
    OUT <- flux_daily_df
    
  } else{
    
    OUT <- rbind(OUT, flux_daily_df)
    
  }
  
  
  #############Save##############

  #Save each year as a CSV
  write.csv(flux_daily_df, file = paste0(OutDir, "USSRM_DailyFlux_", year, ".csv", sep = ""),
          row.names = FALSE)


## END LOOP ##

}


#Save all the years as a CSV: 

write.csv(OUT, file = "USSRM_DailyFlux_2004_2020.csv", #note that this saves to your current working directory
          row.names = FALSE)


#Clear workspace

rm(list = ls())



##################################
# FLUX TOWER PROCESSING - Get Seasonal Data
#################################
##########################
#-------Set up for plots of site weather data
##########################


#SUB_SEASON CODE
#  break into "3" seasons: july, aug, sept, oct = monsoon; 
#  nov, dec, jan, feb = post; other = pre

# SET WD
setwd("F:/ / /")

#read in MODIS data from Google Earth Engine
main_df <- read.csv(file = "MODIS_sat_data_INPUT_modified_waterYear_forMet.csv",
                    stringsAsFactors = FALSE)


#read in
flux_df <- read.csv(file = "USSRM_DailyFlux_2004_2020.csv",
                    stringsAsFactors = FALSE)

#convert from character to date class
main_df$date <- lubridate::mdy(main_df$date)

flux_df$date <- lubridate::ymd(flux_df$date)

#Complete date sequence

main_df <- main_df %>%
  tidyr::complete(date = seq(min(date), max(date), by = "day"),
           fill = list(value = NA))

#Join data frames
main_df <- dplyr::full_join(main_df, flux_df)

#Create month column
main_df <- main_df %>%
  mutate(month = lubridate::month(date))


# SENSITIVITY

#binary
main_df$season <- "non-monsoon"
main_df$season[main_df$month <= 6] <- "non-monsoon" 
main_df$season[main_df$month >= 7] <- "monsoon"
main_df$season[main_df$month >= 11] <- "non-monsoon"

#trinary
main_df$sub_season <- "winter"
main_df$sub_season[main_df$month >= 3 & main_df$month <=6] <- "spring" #chosen due to Barron-Gafford et al., 2013
main_df$sub_season[main_df$month >= 7 & main_df$month <=10] <- "summer"
main_df$sub_season[main_df$month >= 11 & main_df$month <=2] <- "winter"
main_df$sub_season[main_df$DOY >= 60 & main_df$DOY <= 65] <- "winter" #adjusted due earliest start date for trees and Figure 2
main_df$sub_season[main_df$DOY >= 188 & main_df$DOY <= 285] <- "summer" #adjusted from 306 to 286 and 175 to 188 latest finish and start dates for understory and Figure 2 

main_df <- read.csv(file = "main_data.csv", stringsAsFactors = FALSE)


#convert from character to date class
main_df$date <- lubridate::mdy(main_df$date)

#Factoring
main_df$season <- as.factor(main_df$season)


main_df$month <- as.factor(main_df$month)

main_df$water_year <- as.factor(main_df$water_year)

main_df <- main_df %>%
  mutate(year = lubridate::year(date))

main_df$year <- as.factor(main_df$year)

#Check summary stats
summary(main_df)

#add a week column
main_df <- main_df %>%
  mutate(week = lubridate::week(date))

main_df$week <- as.factor(main_df$week)


#Factor subseason
main_df$sub_season <- as.factor(main_df$sub_season)


#---------------------- CREATE SEASONAL DF FOR MET FIGURE 

season_df <- main_df %>%
  select(precip, month, season, sub_season, water_year, vpd_avg, Tair_avg, #select the relevant variables
         swc_all_avg, swc_shallow_avg, swc_deep_avg) %>%
  group_by(water_year, sub_season) %>% #we want to group the data by the water year such that data from november to feb. are grouped together
  dplyr::summarize(season_P_sum = sum(precip, na.rm = T), #summarize the data
                   season_P_IQR = IQR(precip, na.rm = T, type = 5),
                   vpd_IQR = IQR(vpd_avg, na.rm = T, type = 5),
                   swc_shallow_IQR = IQR(swc_shallow_avg, na.rm = T, type = 5),
                   swc_deep_IQR = IQR(swc_deep_avg, na.rm = T, type = 5),
                   swc_all_IQR = IQR(swc_all_avg, na.rm = T, type = 5),
                   Tair_IQR = IQR(Tair_avg, na.rm = T, type = 5),
                   precip_IQR = IQR(precip, na.rm = T, type = 5),
                   precip_cov = coef_var(precip),
                   swc_shallow_cov = coef_var(swc_shallow_avg),
                   swc_deep_cov = coef_var(swc_deep_avg),
                   Tair_cov = coef_var(Tair_avg),
                   vpd_avg = mean(vpd_avg, na.rm = T),
                   Tair_avg = mean(Tair_avg, na.rm = T),
                   swc_shallow_avg = mean(swc_shallow_avg, na.rm = T),
                   swc_deep_avg = mean(swc_deep_avg, na.rm = T),
                   swc_all_avg = mean(swc_all_avg, na.rm = T)
                   
  ) %>%
  filter(water_year != 2021) #this includes water from November and December in 2021, outside scope


#---------------------- CREATE WEATHER PLOTS

#SWC shallow

swc.seas.shallow.plot <- season_df %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(year != 2021) %>%
  ggplot(aes(x = year, y = swc_shallow_avg,
             fill = sub_season,
             ymin = swc_shallow_avg - swc_shallow_IQR,
             ymax = swc_shallow_avg + swc_shallow_IQR)) +
  
  geom_bar(na.rm = T, stat = "identity", position = "dodge") +
  geom_errorbar(width = 0.1, color = "black", 
                position = position_dodge(0.9)) +
  plot.theme  +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("SWC (%) from 2.5 - 30 cm")

#SWC Deep
swc.seas.deep.plot <- season_df %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(year != 2021) %>%
  ggplot(aes(x = year, y = swc_deep_avg,
             fill = sub_season,
             ymin = swc_deep_avg - swc_deep_IQR,
             ymax = swc_deep_avg + swc_deep_IQR)) +
  
  geom_bar(na.rm = T, stat = "identity", position = "dodge") +
  geom_errorbar(width = 0.1, color = "black", 
                position = position_dodge(0.9)) +
  plot.theme  +
  theme(legend.position = "none") +
  xlab("Year") +
  ylab("SWC (%) from 45 - 130 cm")

#Temperature
temp.seas.plot <- season_df %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(year != 2021) %>%
  ggplot(aes(x = year, y = Tair_avg,
             fill = sub_season,
             ymin = Tair_avg - Tair_IQR,
             ymax = Tair_avg + Tair_IQR)) +
  
  geom_bar(na.rm = T, stat = "identity", position = "dodge") +
  geom_errorbar(width = 0.1, color = "black", 
                position = position_dodge(0.9)) +
  plot.theme  +
  scale_fill_discrete(name = "Season") +
  theme(legend.position = "none") +
  xlab("Date") +
  ylab("Air Temperature (C)") 

#Precipitation
precip.seas.plot <- season_df %>%
  mutate(year_num = as.numeric(as.character(year))) %>%
  filter(year != 2021) %>%
  ggplot(aes(x = year, y = season_P_sum, fill = sub_season,
             ymin = season_P_sum - season_P_IQR,
             ymax = season_P_sum + season_P_IQR)) +
  geom_bar(stat = "identity", na.rm = T, position = "dodge") +
  geom_errorbar(width = 0.1, color = "black", 
                position = position_dodge(0.9)) +
  scale_fill_discrete(name = "Season") +
  plot.theme  +
  theme(legend.position = "top") +
  xlab("Year") +
  ylab("Rain (mm)")


###########
# Calculate Yearly Averages for study period
###########

season_df %>%
  select(sub_season, season_P_sum, Tair_avg) %>%
  group_by(sub_season) %>%
  dplyr::summarise(P_avg = mean(season_P_sum, na.rm = T),
                   P_stdev = sd(season_P_sum, na.rm = T),
                   Tair_mean = mean(Tair_avg, na.rm = T),
                   Tair_stdev = sd(Tair_avg, na.rm = T))


######### PLOT METEROLOGY ACROSS TIME ############
ggpubr::ggarrange(temp.seas.plot, #  modis_pheno.plot
                  precip.seas.plot,
                  swc.seas.shallow.plot,
                  swc.seas.deep.plot,
                  ncol=2, nrow=2, common.legend = FALSE, legend="none",
                  labels = "AUTO",
                  hjust = -0.7,
                  vjust = 1)

#save data
write.csv(x = season_df, file = "seasonal_MetDf_2004_2020.csv", 
          row.names = FALSE)

#clean up
rm(precip.seas.plot, swc.seas.deep.plot, swc.seas.shallow.plot,
   temp.seas.plot)


##################################
# PHENOCAM and SAT DATA - set up
#################################

#read in camera data derived from example phenocam GCC script using phenopix
cam_df <- read.csv(file = "E:/2019_Data_MS/Pheno_GPP_SWC_paper_redo/PhenoCam_GCC/gcc_flux_2015-20_rerun_2_DOY285.csv",
                   stringsAsFactors = FALSE)


#convert from character to date class
cam_df$date <- lubridate::mdy(cam_df$date)

#Complete date sequence

cam_df <- cam_df %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(value = NA))

#Create month column
cam_df <- cam_df %>%
  mutate(month = lubridate::month(date))


# SENSITIVITY

#binary
cam_df$season <- "non-monsoon"
cam_df$season[cam_df$month <= 6] <- "non-monsoon" 
cam_df$season[cam_df$month >= 7] <- "monsoon"
cam_df$season[cam_df$month >= 11] <- "non-monsoon"


#cam_df$month <- as.numeric(cam_df$month)

#Factoring
cam_df$season <- as.factor(cam_df$season)


cam_df$month <- as.factor(cam_df$month)

#Create year vector
cam_df <- cam_df %>%
  mutate(year = lubridate::year(date))

#factor year
cam_df$year <- as.factor(cam_df$year)

#Check summary stats
summary(cam_df)

#add a week column
cam_df <- cam_df %>%
  mutate(week = lubridate::week(date))

#factor week
cam_df$week <- as.factor(cam_df$week)


#Factor subseason
cam_df$sub_season <- as.factor(cam_df$sub_season)

#Simplify dataframe
cam_df <- cam_df %>%
  filter(year != 2014) #does not have relevant data 

##########################
#-------Normalize Phenocam GCC with tree AND grass values PER YEAR
##########################

#Rescale GCC values BY RANGE

###
#Create vector to loop around
years <- c(2015:2020)

#start loop around files
runner <- 0 #this tells the loop which file to start from

#Substitute NAs to NaNs #function to find NaNs and make them NAs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#create mega data frame to rbind (bind by rows) all data to
rescale_gcc_df <- data.frame() 

#LOOP for rescale GCC by range for each year (Cite: https://doi.org/10.3390/rs9101071)
for (i in years) {
  ####START LOOP
  
  #test it
  #i = 2015 
  
  #for rbind later
  runner <- runner + 1
  
  
  #test on 2019
  
  temp_df <- cam_df %>%
    filter(year == i)
  
  #get the min, max of either tree or grass gcc for rescaling PER YEAR
  min_gcc = min(temp_df$grass_gcc_mean, temp_df$tree_gcc_mean, na.rm = TRUE) #used to include both types
  
  max_gcc = max(temp_df$tree_gcc_mean, temp_df$grass_gcc_mean, na.rm = TRUE) ##used to include both types
  
  
  #get the min, max of either tree or grass gcc for rescaling PER YEAR
  min_gcc_grass = min(temp_df$grass_gcc_mean, na.rm = TRUE) #used to include both types
  
  max_gcc_grass = max(temp_df$tree_gcc_mean, temp_df$grass_gcc_mean, na.rm = TRUE) ##used to include both types
  
  
  #Get the combined rescale values
  temp_df <- temp_df %>%
    mutate(tree_gcc_rescale = (tree_gcc_mean - min_gcc) / (max_gcc - min_gcc),
           grass_gcc_rescale = (grass_gcc_mean - min_gcc_grass) / (max_gcc - min_gcc),
           tree_z_norm = ((tree_gcc_mean - mean(tree_gcc_mean, na.rm = T)) / sd(tree_gcc_mean, na.rm = T)),
           grass_z_norm = ((grass_gcc_mean - mean(grass_gcc_mean, na.rm = T)) / sd(grass_gcc_mean, na.rm = T)),
           tree_scale = scale(tree_gcc_mean),
           grass_scale = scale(grass_gcc_mean)
    )
  
  #Clean up
  temp_df[is.nan(temp_df)] <- NA
  
  
  #if else statement to know how to loop files together 
  if(runner == 1){
    
    rescale_gcc_df <- temp_df
    
  } else{
    
    rescale_gcc_df <- rbind(rescale_gcc_df, temp_df)
    
  }
  
  
  ###END LOOP  
  
}

rm(temp_df, i)


#Test plot to see if it makes sense and worked for all years
par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(rescale_gcc_df$date, rescale_gcc_df$tree_gcc_rescale, pch = 16, col = 2)  # Create first plot
par(new = TRUE)                             # Add new plot
plot(rescale_gcc_df$date, rescale_gcc_df$grass_gcc_rescale, pch = 17, col = 3, # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")

##################################
# PHENOCAM and SAT DATA - set up
#################################

#Select for key variables to create a relative GCC timeseries between ecosystem (modis),
# understory, and trees

modis_phenocam_df <- rescale_gcc_df %>%
  select(date, tree_gcc_rescale, grass_gcc_rescale,
         modis_gcc_srm) %>%
  mutate(modis_rescale_gcc = rescale_gcc(modis_gcc_srm),
         tree_cv = coef_var(tree_gcc_rescale),
         grass_cv = coef_var(grass_gcc_rescale),
         modis_cv = coef_var(modis_rescale_gcc)) %>%
  select(date, tree_gcc_rescale, grass_gcc_rescale, modis_rescale_gcc,
         tree_cv, grass_cv, modis_cv) 



######### PLOT MODIS AND PHENOCAM GCC TOGETHER ############

# Line Alternative plot
modis_phenocam_df %>%
  select(date, tree_gcc_rescale, grass_gcc_rescale, modis_rescale_gcc) %>%
  mutate(year = lubridate::year(date)) %>%
  melt(id.vars = c("date", "year"),
       variable.name = "Variable",
       value.name = "Value") %>%
  filter(date > "2015-01-01") %>%
  ggplot(aes(x = date, y = Value,
             color = Variable)) +
  geom_line(na.rm = T, alpha = 0.7, size = 1.2) +
  plot.theme +
  theme(legend.position = "top") +
  scale_color_discrete(name = "GCC Source",
                       labels = c("Trees", "Understory", "MODIS"),
                       type =   c("darkgreen", "orange", "black")) +
  scale_shape_manual(name = "GCC Source",
                     labels = c("Trees", "Understory", "MODIS"),
                     values = c(1, 17, 0)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  xlab("Year") +
  ylim(0, 1.0) +
  ylab("GCC (rescaled)")

#clean up
rm(max_gcc, min_gcc, min_gcc_grass,
   max_gcc_grass, runner, years,
   gcc_names)

##########################
#-------CLIMATETOLOGIES
##########################

##########################
#-------Calculate Climatolgy
##########################

#make variable compatible
main_df$water_year <- as.integer(as.character(main_df$water_year))


#drop gcc_avg from rescale_gcc_df
rescale_gcc_df <- rescale_gcc_df %>%
  subset(select= -c(gcc_avg, gcc_sd))

#Join new rescaled phenocam data with longer time series
main_df_rescale <- main_df %>%
  full_join(., rescale_gcc_df)


#simplify modis df
modis_df <- modis_df %>%
  select(date, gcc_avg)

# join with the simplified modis df, really just adding GCC from MODIS (ecosystem scale)
main_df_rescale <- main_df_rescale %>%
  full_join(., modis_df)

#Create the big climatology DF
cli_df <- main_df_rescale %>%
  select(DOY, gcc_avg, swc_all_avg, gpp_avg, tree_gcc_rescale, grass_gcc_rescale,
         swc_shallow_avg, swc_deep_avg, precip, Tair_avg) %>%
  group_by(DOY) %>%
  dplyr::summarize(gcc_min = min(gcc_avg, na.rm = TRUE),
                   gcc_max = max(gcc_avg, na.rm = TRUE),
                   gcc_avg = mean(gcc_avg, na.rm = TRUE),
                   swc_all_max = max(swc_all_avg, na.rm = TRUE),
                   swc_all_min = min(swc_all_avg, na.rm = TRUE),
                   swc_all = mean(swc_all_avg, na.rm = TRUE),
                   swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
                   swc_shallow_max = max(swc_shallow_avg, na.rm = TRUE),
                   swc_shallow_min = min(swc_shallow_avg, na.rm = TRUE),
                   swc_deep = mean(swc_deep_avg, na.rm = TRUE),
                   swc_deep_max = max(swc_deep_avg, na.rm = TRUE),
                   swc_deep_min = min(swc_deep_avg, na.rm = TRUE),
                   gpp_min = min(gpp_avg, na.rm = TRUE),
                   gpp_max = max(gpp_avg, na.rm = TRUE),
                   gpp_avg = mean(gpp_avg, na.rm = TRUE),
                   tree_min = min(tree_gcc_rescale, na.rm = TRUE),
                   tree_max = max(tree_gcc_rescale, na.rm = TRUE),
                   tree_avg = mean(tree_gcc_rescale, na.rm = TRUE),
                   grass_min = min(grass_gcc_rescale, na.rm = TRUE),
                   grass_max = max(grass_gcc_rescale, na.rm = TRUE),
                   grass_avg = mean(grass_gcc_rescale, na.rm = TRUE),
                   precip_avg = mean(precip, na.rm = TRUE),
                   precip_min = min(precip, na.rm = TRUE),
                   precip_max = max(precip, na.rm = TRUE),
                   temp_avg = mean(Tair_avg, na.rm = TRUE),
                   temp_min = min(Tair_avg, na.rm = TRUE),
                   temp_max = max(Tair_avg, na.rm = TRUE),
                   precip_events = sum(precip > 5, na.rm = TRUE)
  )

#Change DOY to integer
cli_df$DOY <- as.integer(cli_df$DOY)

#Reorganize sub_seasons
#trinary
cli_df$sub_season <- "winter"
cli_df$sub_season[cli_df$DOY >= 65 & cli_df$DOY <=188] <- "spring" #chosen due to Barron-Gafford et al., 2013 for end of spring, 
cli_df$sub_season[cli_df$DOY >= 189 & cli_df$DOY <=285] <- "summer"
#cli_df$sub_season[cli_df$DOY >= 305 & cli_df$DOY <= 59] <- "winter"

#factor sub_season
cli_df$sub_season <- as.factor(cli_df$sub_season)


################
#-- CLIMATOLOGY PLOTS
################

# ECOSYSTEM GCC
modis_gcc_cli.plot <- cli_df %>%
  #filter(year == 2019) %>%
  ggplot(mapping = aes(x = DOY, 
                       y = gcc_avg)) +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +  #MODIS mean
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  #geom_linerange(alpha = 1, aes(ymin = gcc_min, ymax = gcc_max), color = "grey70") +
  geom_ribbon(alpha = 0.3, aes(ymin = gcc_min, ymax = gcc_max)) +
  geom_point(aes(color = sub_season), 
             size = 3, alpha = 0.7) +
  xlab("") +
  ylab("GCC MODIS") +
  plot.theme.cli +
  theme(legend.position = c(1, 1))

#Avg DOY from phenocam start date: = 80 (about March 21)
#Avg DOY start from MODIS: start date at about 90 (about March 31)


#TREE and GRASS CLIMATOLOGY
plant_gcc_cli.plot <- cli_df %>%
  ggplot() +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 82, color = "purple", size = 1.5, 
             linetype = "dashed") +  #MODIS mean
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, 
             linetype = "dashed") +
  geom_ribbon(alpha = 0.3, aes(x = DOY, y = tree_avg, 
                               ymin = tree_min, ymax = tree_max), 
              fill = "#999999") +
  geom_ribbon(alpha = 0.6, aes(x = DOY, y = grass_avg, 
                                  ymin = grass_min, ymax = grass_max), 
                 fill = "#99CCCC") +
  geom_point(aes(x = DOY, y = tree_avg, color = sub_season), 
             size = 3, alpha = 1, shape = 24, fill = NA) +
  geom_point(aes(x = DOY, y = grass_avg, color = sub_season),  
             size = 3, alpha = 0.8, shape = "circle") +
  xlab("") +
  ylab("Relative GCC") +
  scale_shape_manual(name = "Plant Type", values = c(24, "circle")) +
  plot.theme.cli +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.margin = margin())

#SOIL WATER CONTENT SHALLOW (< 45 cm)
swc_cli_shallow.plot <- cli_df %>%
  ggplot(mapping = aes(x = DOY, 
                       y = swc_shallow)) +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +
  #geom_linerange(alpha = 1, aes(ymin = swc_shallow_min, ymax = swc_shallow_max), color = "grey70") +
  geom_ribbon(alpha = 0.3, aes(ymin = swc_shallow_min, ymax = swc_shallow_max)) +
  geom_point(aes(color = sub_season), size = 3, alpha = 0.7) +
  xlab("") +
  ylab("SWC (%) from 2.5 - 30 cm ") +
  plot.theme.cli +
  theme(legend.position = c(1, 1))

#SOIL WATER CONTENT DEEP (>= 45 cm)
swc_cli_deep.plot <- cli_df %>%
  ggplot(mapping = aes(x = DOY, 
                       y = swc_deep)) +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +
  geom_ribbon(alpha = 0.3, aes(ymin = swc_deep_min, ymax = swc_deep_max)) +
  geom_point(aes(color = sub_season), size = 3, alpha = 0.7) +
  ylab("SWC (%) from 45 - 130 cm") +
  plot.theme.cli +
  theme(legend.position = c(1, 1))

#GPP FROM US-SRM FLUX TOWER
gpp_cli.plot <- cli_df %>%
  #filter(year == 2019) %>%
  ggplot(mapping = aes(x = DOY, 
                       y = gpp_avg)) +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) +
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +
  geom_ribbon(alpha = 0.3, aes(ymin = gpp_min, ymax = gpp_max)) +
  geom_point(aes(color = sub_season), size = 3, alpha = 0.7) +
  xlab("DOY") +
  labs(y = expression(Tower~GPP~(g~CO[2]~m^-2~d^-1))) +
  plot.theme.cli +
  theme(legend.position = c(1, 1))


# TOTAL PRECIPITATION AT US-SRM
precip_cli.plot <- cli_df %>%
  #filter(year == 2019) %>%
  ggplot(mapping = aes(x = DOY, 
                       y = precip_avg)) +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +  #MODIS mean
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  geom_ribbon(alpha = 0.3, aes(ymin = precip_min, ymax = precip_max)) +
  geom_point(aes(color = sub_season), 
             size = 3, alpha = 0.7) +
  xlab("") +
  ylab("Precipitation (mm)") +
  plot.theme.cli +
  theme(legend.position = c(1, 1))

#GROUP THE PLOTS TOGETHER
ggpubr::ggarrange(modis_gcc_cli.plot,
                  precip_cli.plot, 
                  plant_gcc_cli.plot,
                  swc_cli_shallow.plot,
                  gpp_cli.plot,
                  swc_cli_deep.plot,
                  align = "hv", #align in both directions / axes
                  ncol=2, nrow=3, common.legend = TRUE, legend="top",
                  labels =  c("A", "D", "B", "E", "C", "F"),   #"AUTO",
                  hjust = -1,
                  vjust = -0.9)


dev.off()

#################
#-- GPP and MODIS GCC Correlation
#################

#Create a linear model that predicts annual GPP via ecosystem GCC from MODIS
annual.model <- lm(gpp_avg ~ gcc_avg,
                   data = main_df)
summary(annual.model)


test.model <- lm(gpp_avg ~ (tree_gcc_mean) + (grass_gcc_mean),
                   data = rescale_gcc_df)
summary(test.model)

##########################
#-------Seasonality and Depth Modeling Figure
##########################

#set wd

setwd("F:/ / /")

#Create an annual df for GPP sums
annual_df <- main_df %>%
  mutate(gpp_zscore = (gpp_avg - mean(gpp_avg, na.rm = T) / sd(gpp_avg, na.rm = T))
  ) %>%
  dplyr::select(year, gpp_avg, gpp_zscore, nee_avg, swc_all_avg) %>%
  group_by(year) %>%
  dplyr::summarise(annual_gpp = sum(gpp_avg, na.rm = T),
                   annual_gpp_zscore = mean(gpp_zscore, na.rm = T),
                   annual_nee_avg = mean(nee_avg, na.rm = T),
                   annual_swc_all_avg = mean(swc_all_avg, na.rm = T),
                   annual_swc_all_sd = sd(swc_all_avg, na.rm = T))


#Create an annual DF with GPP and percent GPP across seasons
yearly_gpp_df <- main_df %>%
  dplyr::select(year, sub_season, date, gpp_avg, nee_avg) %>%
  group_by(year, sub_season) %>%
  dplyr::summarise(seas_gpp = sum(gpp_avg, na.rm = T)) %>%
  spread(sub_season, seas_gpp) %>%
  merge(., annual_df) %>%
  dplyr::rename(spring_gpp = spring,
         winter_gpp = winter,
         summer_gpp = summer) %>%
  mutate(spring_per = (spring_gpp / annual_gpp) * 100, #percent contribution of spring GPP to the year
         summer_per = (summer_gpp / annual_gpp) * 100,
         winter_per = (winter_gpp / annual_gpp) * 100)

#25th percentiles for spring gpp
quantile(yearly_gpp_df$spring_gpp, probs = c(0.25, 0.75)) 

#25th percentiles for summer gpp
quantile(yearly_gpp_df$summer_gpp, probs = c(0.25, 0.75))

#25th percentiles for TOTAL gpp
quantile(annual_df$annual_gpp, probs = c(0.25, 0.75)) #310.3141, 450.6895

#Creating a df for seasonal total (sum) gpp per year
yearly_gpp_df <- yearly_gpp_df %>%
  mutate(spring_status = ifelse(spring_gpp < 44.06431, "Low GPP (0 to 42)", ifelse(spring_gpp > 111.13559, "High GPP (111 to 210)", "Normal GPP (43 to 110)")), #25th percentiles
         summer_status = ifelse(summer_gpp < 211.5991, "low", ifelse(summer_gpp > 282.6845, "high", "normal")),
         annual_status = ifelse(annual_gpp < 310.3141, "Low GPP (0 to 310)", ifelse(annual_gpp > 450.6895, "High GPP (451 to 561)", "Normal GPP (311 to 450)"))) %>%
  filter(year != 2004) %>%
  dplyr::select(year, annual_gpp, annual_status, annual_swc_all_avg,
                annual_gpp_zscore, spring_gpp, spring_per, spring_status, 
                summer_gpp, summer_per, summer_status, annual_nee_avg,
                winter_gpp, winter_per) #simplified

#rename year column to a common name
season_df <- season_df %>%
  dplyr::rename(year = water_year)


#Create seasonal dfs and join them
spring_season_df <- season_df %>%
  filter(sub_season == "spring") %>% #filter for just spring season variables from season_df
  full_join(., yearly_gpp_df) %>% #join with yearly gpp created earlier by year
  filter(year != 2004) %>% #get rid of 2004 since don't have in situ water data in 2003
  select(-c(summer_gpp, summer_per, summer_status)) #drop unused columns


summer_season_df <- season_df %>%
  filter(sub_season == "summer") %>%
  full_join(., yearly_gpp_df) %>%
  filter(year != 2004) %>%
  select(-c(spring_gpp, spring_per, spring_status))


winter_season_df <- season_df %>%
  filter(sub_season == "winter") %>%
  full_join(., yearly_gpp_df) %>%
  filter(year != 2004)  

#Join all seasonal data
season_gpp_corr_df <- rbind(spring_season_df,
                            summer_season_df,
                            winter_season_df)

#create correlation DF where columns are assigned the different seasons
season_gpp_corr_simple <- season_gpp_corr_df %>%
  select(year, sub_season, season_P_sum, swc_all_avg, swc_shallow_avg, swc_deep_avg) %>%
  nest(c(swc_all_avg, season_P_sum, swc_shallow_avg, swc_deep_avg), 
       .key = 'value_col') %>%
  spread(key = sub_season, value = 'value_col') %>%
  unnest(c(spring, summer, winter), .sep = '_') 


# Join tables
season_gpp_corr_simple <- yearly_gpp_df %>%
  filter(year != 2004) %>%
  full_join(., season_gpp_corr_simple) %>% 
  select(year, spring_gpp, summer_gpp, winter_gpp, annual_gpp,
         annual_swc_all_avg, spring_swc_all_avg, summer_swc_all_avg, winter_swc_all_avg,
         spring_season_P_sum, summer_season_P_sum, winter_season_P_sum,
         spring_swc_shallow_avg, spring_swc_deep_avg,
         summer_swc_shallow_avg, summer_swc_deep_avg,
         winter_swc_shallow_avg, winter_swc_deep_avg)

#remove year column
season_gpp_corr_simple <- subset(season_gpp_corr_simple, select = -year)

#add annual  precip columns

annual_P_df <- season_df %>%
  select(sub_season, season_P_sum, swc_shallow_avg, swc_deep_avg) %>%
  group_by(year) %>%
 dplyr::summarize(annual_P_sum = sum(season_P_sum, na.rm = T),
            annual_swc_shallow_avg = mean(swc_shallow_avg, na.rm = T),
            annual_swc_deep_avg = mean(swc_deep_avg, na.rm = T)) %>%
  filter(year != 2021 & year != 2004)

season_gpp_corr_simple$annual_P_sum <- annual_P_df$annual_P_sum

season_gpp_corr_simple$annual_swc_shallow_avg <- annual_P_df$annual_swc_shallow_avg

season_gpp_corr_simple$annual_swc_deep_avg <- annual_P_df$annual_swc_deep_avg


#Visualize correlations

cor(season_gpp_corr_simple)


#IT WORKS, subset for either 1:8 or the rest for either rain or swc_all
names(season_gpp_corr_simple) <- gsub(" ", "\n", names(season_gpp_corr_simple))

mychart.Correlation(season_gpp_corr_simple[c(1:4, 12:17)], histogram = F, pairs = NULL,
                  method = "pearson", cex.labels = 1.3)

mtext("Correlation Matrix between SWC and GPP", side = 3, line = 3)


chart.Correlation(season_gpp_corr_simple[c(1:8)], histogram = F, pairs = NULL)

#############
# MODEL COMPARISONS FOR RAIN, SWC, DEPTH, AND SUM GPP
#############

#Create a linear model that predicts annual GPP via total precip for the year
annual.model <- lm(annual_gpp ~ annual_P_sum,
                   data = season_gpp_corr_simple)
summary(annual.model)
AIC(annual.model)

# Adding spring rain does not do anything but make the model worse, interesting
seasonal_rain.model <- lm(annual_gpp ~ summer_season_P_sum + winter_season_P_sum,
                          data = season_gpp_corr_simple)
summary(seasonal_rain.model)
car::vif(seasonal_rain.model)
AIC(seasonal_rain.model)

#Test relationship between spring SWC and winter P
cor.test(x = season_gpp_corr_simple$winter_season_P_sum, 
         y = season_gpp_corr_simple$spring_swc_all_avg)

seasonal_winterRain_SpringSWC.model <- lm(spring_swc_all_avg ~  winter_season_P_sum,
                          data = season_gpp_corr_simple)
summary(seasonal_winterRain_SpringSWC.model)

# Looking at SWC in general across different seasons
seasonal_swc_all.model <- lm(annual_gpp ~ summer_swc_all_avg + spring_swc_all_avg + winter_swc_all_avg,
                             data = season_gpp_corr_simple)
summary(seasonal_swc_all.model)
car::vif(seasonal_swc_all.model)
AIC(seasonal_swc_all.model)

# Model with average SWC across all layers
annual_swc_all.model <- lm(annual_gpp ~ annual_swc_all_avg,
                           data = season_gpp_corr_simple)
summary(annual_swc_all.model)
AIC(annual_swc_all.model)

#Adding depth to annual and seasonal

#Model with depths and seasons predicting annual GPP
seasonal_swc_depth.model <- lm(annual_gpp ~ #spring_swc_shallow_avg + spring_swc_deep_avg +
                                 summer_swc_shallow_avg + summer_swc_deep_avg +
                                 winter_swc_shallow_avg + winter_swc_deep_avg,
                               data = season_gpp_corr_simple)
summary(seasonal_swc_depth.model)

AIC(seasonal_swc_depth.model) # = 174 with spring
                              # = 174 without spring

car::vif(seasonal_swc_depth.model) #looking at value inflation factor 

  #Without spring variables, the VIF for summer and winter SWCs is less than 5
  #With spring, VIF is above 5 for spring and winter deep SWC variables

#Spring GPP with spring and winter SWC and depth

seasonal_swc_depth_Spring_GPP.model <- lm(spring_gpp ~  #spring_swc_shallow_avg + spring_swc_deep_avg +
                                 winter_swc_shallow_avg + winter_swc_deep_avg,
                               data = season_gpp_corr_simple)

summary(seasonal_swc_depth_Spring_GPP.model)
AIC(seasonal_swc_depth_Spring_GPP.model)
car::vif(seasonal_swc_depth_Spring_GPP.model) # ViF less than 5 is acceptable:
                                              #James G, Witten D, Hastie T, Tibshirani R. An Introduction to Statistical Learning: With Applications in R. 1st ed. 2013, Corr. 7th printing 2017 edition. Springer; 2013.


#Spring GPP with spring and winter SWC and depth

seasonal_swc_deep_Spring_GPP.model <- lm(spring_gpp ~  #spring_swc_shallow_avg + spring_swc_deep_avg +
                                           winter_swc_deep_avg,
                                          data = season_gpp_corr_simple)

summary(seasonal_swc_deep_Spring_GPP.model)
AIC(seasonal_swc_deep_Spring_GPP.model)


#Spring GPP with spring and winter SWC and depth

seasonal_swc_shallow_Spring_GPP.model <- lm(spring_gpp ~  #spring_swc_shallow_avg + spring_swc_deep_avg +
                                           winter_swc_shallow_avg,
                                         data = season_gpp_corr_simple)

summary(seasonal_swc_shallow_Spring_GPP.model)
AIC(seasonal_swc_shallow_Spring_GPP.model)



#Summer GPP with spring and winter SWC and depth
# can use this to test depth separately
seasonal_swc_depth_Summer_GPP.model <- lm(summer_gpp ~  #spring_swc_shallow_avg + spring_swc_deep_avg +
                                            summer_swc_shallow_avg + summer_swc_deep_avg,
                                          data = season_gpp_corr_simple)

summary(seasonal_swc_depth_Summer_GPP.model)
AIC(seasonal_swc_depth_Summer_GPP.model)
car::vif(seasonal_swc_depth_Summer_GPP.model)

# summer gpp and summer shallow swc
seasonal_swc_shallow_Summer_GPP.model <- lm(summer_gpp ~  #spring_swc_shallow_avg + spring_swc_deep_avg +
                                            summer_swc_shallow_avg,
                                          data = season_gpp_corr_simple)

summary(seasonal_swc_shallow_Summer_GPP.model)
AIC(seasonal_swc_shallow_Summer_GPP.model)

# summer gpp and summer deep swc
seasonal_swc_deep_Summer_GPP.model <- lm(summer_gpp ~  #spring_swc_deep_avg + spring_swc_deep_avg +
                                              summer_swc_deep_avg,
                                            data = season_gpp_corr_simple)

summary(seasonal_swc_deep_Summer_GPP.model)
AIC(seasonal_swc_deep_Summer_GPP.model)



#Annual Depth model
annual_swc_depth.model <- lm(annual_gpp ~  annual_swc_shallow_avg + annual_swc_deep_avg,
                             data = season_gpp_corr_simple)
summary(annual_swc_depth.model) # R2 = 0.67
AIC(annual_swc_depth.model) # = 184

car::vif(annual_swc_depth.model)


#Annual model with just shallow SWC
annual_swc_shallow.model <- lm(annual_gpp ~  annual_swc_shallow_avg,
                               data = season_gpp_corr_simple)
summary(annual_swc_shallow.model) # R2 = 0.66
AIC(annual_swc_shallow.model) # = 183

#Annual model with DEEP swc
annual_swc_deep.model <- lm(annual_gpp ~  annual_swc_deep_avg,
                            data = season_gpp_corr_simple)
summary(annual_swc_deep.model) # R2 = 0.50
AIC(annual_swc_deep.model) # = 189


#Annual GPP predicted by deep SWC across seasons
seasonal_swc_deep.model <- lm(annual_gpp ~ spring_swc_deep_avg + summer_swc_deep_avg + winter_swc_deep_avg,
                              data = season_gpp_corr_simple)
summary(seasonal_swc_deep.model) # 0.75
AIC(seasonal_swc_deep.model) # = 182

#Annual GPP predicted by shallow SWC across seasons
seasonal_swc_shallow.model <- lm(annual_gpp ~ spring_swc_shallow_avg + summer_swc_shallow_avg + winter_swc_shallow_avg,
                                 data = season_gpp_corr_simple)
summary(seasonal_swc_shallow.model) # R2 = 0.82
AIC(seasonal_swc_shallow.model) # = 176


# Spring SWC and winter rainfall

spring_swc_winter_rainfall.model <- lm(spring_swc_all_avg ~  spring_season_P_sum,
                               data = season_gpp_corr_simple)

summary(spring_swc_winter_rainfall.model)

##################
# Supplementary Plot for spring swc and winter rainfall
##################
#SCATTER PLOT
spring_swc.winter_P.plot <- season_gpp_corr_simple %>%
  #filter(year != 2019 & year != 2020) %>%
  ggplot(mapping = aes(x = winter_season_P_sum, 
                       y = spring_swc_all_avg)) +
  geom_point(alpha = 1, size = 2) +
  geom_smooth(method = 'lm', formula = "y ~ x", se = F, color = "black") + 
  #ggtitle("Spring SWC (all layers) and Winter Precipitation") +
  xlab("Yearly Total Winter Precipitation (mm)") +
  #ylim(0, 600) +
  labs(y = "SWC (%)") +
  annotate(geom = "text", x = 50, y = 7, size = 5,
           label = expression(R^2~'='~'0.64***')) +
  #geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme_phenoMet_gpp 




####################
# TIME LAGS BETWEEN SWC AND PHENOCAM/MODIS GCC
###################

############# USING MONTHLY VARIBLES #################

# take daily and turn into monthly data for the monthly time lags as suggested by Barnes et al., 2016
monthly_rescale_df <- rescale_gcc_df %>%
  mutate(week = lubridate::week(date)) %>%
  select(week, precip, month, date, sub_season, water_year, tree_gcc_rescale, grass_gcc_rescale,
         swc_shallow_avg, swc_deep_avg, Tsoi_2.5_10_avg, gpp_avg) %>%
  group_by(water_year, month, sub_season) %>%
  dplyr::summarise(monthly_P_sum = sum(precip, na.rm = T),
                   tree_gcc_avg = mean(tree_gcc_rescale, na.rm = T), #WAS MEDIAN
                   grass_gcc_avg = mean(grass_gcc_rescale, na.rm = T), #was median
                   gpp_mean = mean(gpp_avg, na.rm = T),
                   swc_shallow_mean = mean(swc_shallow_avg, na.rm = T),
                   swc_deep_mean = mean(swc_deep_avg, na.rm = T),
                   Tsoi_avg = mean(Tsoi_2.5_10_avg, na.rm = T))

#remove out of bounds data
clean_monthly_df <- filter_at(monthly_rescale_df, vars(tree_gcc_avg, grass_gcc_avg), 
                              all_vars((.) != -Inf))

#clean_monthly_df <- unique(clean_monthly_df)


#  FOR GRASS
month_grass_df <- monthly_rescale_df %>%
  filter(month == 3 | month == 4 | month == 5 | month == 6 | month == 7 | month == 8 | month == 9 | month == 10) %>%
  filter(sub_season == "summer" | sub_season == "spring") %>%
  #filter(water_year == 2015 | water_year == 2016) %>%
  drop_na()

# FOR TREE AND ECOSYSTEM/MODIS
month_tree_modis_df <- monthly_rescale_df %>%
  drop_na()

#convert to a timeseries object
month_grass_gcc_ts <- ts(month_grass_df)

#timeseries object for tree and modis gcc
month_gcc_ts <- ts(month_tree_modis_df)

#test 
plot.ts(month_gcc_ts[,c(7:8)])

#Remember, not forecasting, looking at time lags
forecast::tsdisplay(month_grass_gcc_ts [,"grass_gcc_avg"])

#how understory GCC responds to current and past shallow SWC
t <- forecast::Acf(month_grass_gcc_ts [,c("grass_gcc_avg", "swc_shallow_mean")], lag.max = 12, ci = 0.99) #chosen due to Barnes et al., 2016
t$acf[, , 2][,1] #how you can get the values


#how understory GCC responds to current and past DEEP SWC
t_2 <- forecast::Acf(month_grass_gcc_ts[,c("grass_gcc_avg", "swc_deep_mean")], lag.max = 12, ci = 0.99) #chosen due to Barnes et al., 2016
t_2$acf[, , 2][,1] #how you can get the values


#SET UP FOR THE TREE GCC response to past and current shallow/deep SWC

tree_deep_acf <- forecast::Acf(month_gcc_ts[,c("tree_gcc_avg", "swc_deep_mean")], lag.max = 12, ci = 0.99)

tree_shallow_acf <- forecast::Acf(month_gcc_ts[,c("tree_gcc_avg", "swc_shallow_mean")], lag.max = 12, ci = 0.99)


#---------CREATE TIME MONTHLY LAG DATA FRAME---------------------
#TO CALCULATE THE CONFIDENCE LINES:
# getS3method("plot", "acf") look for qnorm

#MAKE THE TIME LAG DATA FRAME FOR PLOTTING
R_statistic <- t$acf[, , 2][,1] # get the correlations
Lag <- t$lag[, , 2][,1] #get the lags
plant_type <- rep("understory", 13) #create plant type vector
water_variable <- rep("swc_shallow", 13) #create swc shallow vector label

timelag_df <- data.frame(R_statistic, Lag, plant_type, water_variable) #put it all together in DF

#Find first order autocorrelation where the lag is "0"
t$acf[, , 2][,1]

# Check first order lag

cor.test(x = month_gcc_ts[,c("swc_shallow_mean")], y = month_gcc_ts[,c("grass_gcc_avg")],
         alternative = "two.sided", conf.level = 0.99)


#How to get the confidence intervals based on the R function in a simple manner 
#   NOTE: not useful for long and extended time series in which you want to predict future

qnorm((1.99)/2)/sqrt(t$n.used) # - Based on the function for plotting acf's
 #0.3642773 for 0.01 alpha level

timelag_df <- timelag_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.3642773 & R_statistic < 0.3642773, "Not Sig.", "Sig."))


#Grass DEEP WINTER
R_statistic <- t_2$acf[, , 2][,1]
Lag <- t_2$lag[, , 2][,1]
plant_type <- rep("understory", 13)
water_variable <- rep("swc_deep", 13)


grass_deep_timelag_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

# Check first order lag

cor.test(x = month_gcc_ts[,c("swc_deep_mean")], y = month_gcc_ts[,c("grass_gcc_avg")],
         alternative = "two.sided", conf.level = 0.99)


qnorm((1.99)/2)/sqrt(t_2$n.used) # - Based on the function for plotting acf's
# 0.3642773

grass_deep_timelag_df <- grass_deep_timelag_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.3642773 & R_statistic < 0.3642773, "Not Sig.", "Sig.")) #old r:  | R_statistic < 0.2538189

#Add to time lag
timelag_df <- rbind(timelag_df, grass_deep_timelag_df)


#TREE DEEP Spring and Summer
R_statistic <- tree_deep_acf$acf[, , 2][,1]
Lag <- tree_deep_acf$lag[, , 2][,1]
plant_type <- rep("tree", 13)
water_variable <- rep("swc_deep", 13)

# Check first order lag

cor.test(x = month_gcc_ts[,c("swc_deep_mean")], y = month_gcc_ts[,c("tree_gcc_avg")],
         alternative = "two.sided", conf.level = 0.99)

#r = abs(-0.3265583)

qnorm((1.99)/2)/sqrt(tree_deep_acf$n.used) # - Based on the function for plotting acf's
# 0.3056947

tree_timelag_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

tree_timelag_df <- tree_timelag_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.3056947 & R_statistic < 0.3056947, "Not Sig.", "Sig.")) # OLD  | R_statistic < 0.2871118

timelag_df <- rbind(timelag_df, tree_timelag_df)


#TREE SHALLOW SUMMER and SPRING
R_statistic <- tree_shallow_acf$acf[, , 2][,1]
Lag <- tree_shallow_acf$lag[, , 2][,1]
plant_type <- rep("tree", 13)
water_variable <- rep("swc_shallow", 13)


#-1/tree_shallow_acf$n.used + c(-2,2)/sqrt(tree_shallow_acf$n.used)
# -0.3028427  0.2628427


cor.test(x = month_gcc_ts[,c("swc_shallow_mean")], y = month_gcc_ts[,c("tree_gcc_avg")],
         alternative = "two.sided", conf.level = 0.99)


qnorm((1.99)/2)/sqrt(tree_shallow_acf$n.used) # - Based on the function for plotting acf's
# 0.3056947

tree_timelag_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

tree_timelag_df <- tree_timelag_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.3056947 & R_statistic < 0.3056947, "Not Sig.", "Sig.")) # OLD  | R_statistic < 0.2719235

timelag_df <- rbind(timelag_df, tree_timelag_df)


rm(R_statistic, Lag, plant_type, water_variable)

##########################
#-------TS regressions WITH MODIS http://web.vu.lt/mif/a.buteikis/wp-content/uploads/2020/04/Example_05.html#1_plot_the_data_series_and_examine_their_stationarity
##########################
#SET WD
setwd("F:/ / /")

#read in
modis_flux_df <- read.csv(file = "MODIS_sat_data_INPUT_modified_waterYear_forMet.csv",
                          stringsAsFactors = FALSE)


#read in
flux_df <- read.csv(file = "USSRM_DailyFlux_2004_2020.csv",
                    stringsAsFactors = FALSE)

#convert from character to date class
modis_flux_df$date <- lubridate::mdy(modis_flux_df$date)

flux_df$date <- lubridate::ymd(flux_df$date)

#Complete date sequence

modis_flux_df <- modis_flux_df %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(value = NA))

#Join data frames
modis_flux_df <- full_join(modis_flux_df, flux_df)

#Create month column
modis_flux_df <- modis_flux_df %>%
  mutate(month = lubridate::month(date))

#Factoring
modis_flux_df$season <- as.factor(modis_flux_df$season)


modis_flux_df$month <- as.factor(modis_flux_df$month)


#Create year variable
modis_flux_df <- modis_flux_df %>%
  mutate(year = lubridate::year(date))

#factor year variable
modis_flux_df$year <- as.factor(modis_flux_df$year)

#Check summary stats
summary(modis_flux_df)

#add a week column
modis_flux_df <- modis_flux_df %>%
  mutate(week = lubridate::week(date))

#factor week variable
modis_flux_df$week <- as.factor(modis_flux_df$week)


#Factor subseason
modis_flux_df$sub_season <- as.factor(modis_flux_df$sub_season)


#### MODIS MONTHLY LAGS = CLEAR SEASONAL PATTERNS IN ACF, WHAT DOES IT SAY?  ############
#monthly_modis_df_old <- monthly_modis_df

monthly_modis_df <- modis_flux_df %>%
  mutate(year = lubridate::year(date)) %>%
  #filter(month == 3 | month == 4 | month == 5 | month == 6 | month == 7 | month == 8 | month == 9 | month == 10) %>%
  select(year, precip, month, date, sub_season, gcc_avg, ndvi_avg,
         swc_shallow_avg, swc_deep_avg, Tsoi_2.5_10_avg, gpp_avg) %>%
  group_by(year, month) %>%
  dplyr::summarise(monthly_P_sum = sum(precip, na.rm = T),
                   gcc_mean = mean(gcc_avg, na.rm = T),
                   ndvi_mean = mean(ndvi_avg, na.rm = T),
                   gpp_mean = mean(gpp_avg, na.rm = T),
                   swc_shallow_mean = mean(swc_shallow_avg, na.rm = T),
                   swc_deep_mean = mean(swc_deep_avg, na.rm = T),
                   Tsoi_avg = mean(Tsoi_2.5_10_avg, na.rm = T)) %>%
  #filter(year == 2019) %>%
  drop_na()

#convert modis df into timeseries (ts) object
monthly_modis_ts <- ts(monthly_modis_df, frequency = 1)

plot.ts(monthly_modis_ts[,c(5)])

#Look at ACF and intereactions

tseries::adf.test(monthly_modis_ts[,"gcc_mean"], alternative = "stationary")
#p-value = 0.01, no unit root

tseries::adf.test(monthly_modis_ts[,"swc_shallow_mean"], alternative = "stationary")
#p-value = 0.01, no unit root

#Create AcF objects for MODIS

mod <- forecast::Acf(monthly_modis_ts[,c("gcc_mean", "swc_shallow_mean")], lag.max = 12, 
                     ci = 0.99 #used as the CI is calculated assuming a two-tailed, using 0.9 makes it a one tailed with a=0.05 by 0.1 / 2 = 0.05.
) #chosen due to Barnes et al., 2016
mod$acf[, , 2][,1] #how you can get the values


mod_2 <- forecast::Acf(monthly_modis_ts[,c("gcc_mean", "swc_deep_mean")], lag.max = 12, ci = 0.99) #chosen due to Barnes et al., 2016
mod_2$acf[, , 2][,1] #how you can get the values


#

#MODIS SHALLOW
R_statistic <- mod$acf[, , 2][,1]
Lag <- mod$lag[, , 2][,1]
plant_type <- rep("modis", 13)
water_variable <- rep("swc_shallow", 13)


cor.test(x = monthly_modis_ts[,c("swc_shallow_mean")], y = monthly_modis_ts[,c("gcc_mean")],
         alternative = "two.sided", conf.level = 0.99)

#Get the 95% confidence intervals
qnorm((1.99)/2)/sqrt(mod$n.used) # - Based on the function for plotting acf's
# 0.1803441

#Create the dataframe
modis_timelag_shallow_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

modis_timelag_shallow_df <- modis_timelag_shallow_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.1803441 & R_statistic < 0.1803441, "Not Sig.", "Sig.")) #OLD R: R_statistic > -0.140044 & R_statistic < 0.1308699

timelag_df <- rbind(timelag_df, modis_timelag_shallow_df)


#MODIS DEEP
R_statistic <- mod_2$acf[, , 2][,1]
Lag <- mod_2$lag[, , 2][,1]
plant_type <- rep("modis", 13)
water_variable <- rep("swc_deep", 13)


#Find first order autocorrelation where the lag is "0"

# Check first order lag

cor.test(x = monthly_modis_ts[,c("swc_deep_mean")], y = monthly_modis_ts[,c("gcc_mean")],
         alternative = "two.sided", conf.level = 0.99)

#Find the confidence intervals
qnorm((1.99)/2)/sqrt(mod_2$n.used) # - Based on the function for plotting acf's
# 0.1803441

modis_timelag_deep_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

modis_timelag_deep_df <- modis_timelag_deep_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.1803441 & R_statistic < 0.1803441, "Not Sig.", "Sig.")) # OLD R_statistic > -0.140044 & R_statistic < 0.1308699

timelag_df <- rbind(timelag_df, modis_timelag_deep_df)


######################
# GPP TIME LAGS similar to the previous time lag code
######################


gpp <- forecast::Acf(monthly_modis_ts[,c("gpp_mean", "swc_shallow_mean")], lag.max = 12, ci = 0.99) #chosen due to Barnes et al., 2016
gpp$acf[, , 2][,1] #how you can get the values


gpp_2 <- forecast::Acf(monthly_modis_ts[,c("gpp_mean", "swc_deep_mean")], lag.max = 12, ci = 0.99) #chosen due to Barnes et al., 2016
gpp_2$acf[, , 2][,1] #how you can get the values


#GPP SHALLOW
R_statistic <- gpp$acf[, , 2][,1]
Lag <- gpp$lag[, , 2][,1]
plant_type <- rep("tower", 13)
water_variable <- rep("swc_shallow", 13)


#-1/gpp$n.used + c(-2,2)/sqrt(gpp$n.used)
# -0.1400442  0.1308699

#Find first order autocorrelation where the lag is "0"

# Check first order lag

cor.test(x = monthly_modis_ts[,c("swc_shallow_mean")], y = monthly_modis_ts[,c("gpp_mean")],
         alternative = "two.sided", conf.level = 0.99)


#Get confidence interval
qnorm((1.99)/2)/sqrt(gpp$n.used) # - Based on the function for plotting acf's
# 0.1803441


gpp_timelag_shallow_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

gpp_timelag_shallow_df <- gpp_timelag_shallow_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.1803441 & R_statistic < 0.1803441, "Not Sig.", "Sig.")) # OLD R_statistic > -0.140044 & R_statistic < 0.1308699

timelag_df <- rbind(timelag_df, gpp_timelag_shallow_df)


#GPP DEEP
R_statistic <- gpp_2$acf[, , 2][,1]
Lag <- gpp_2$lag[, , 2][,1]
plant_type <- rep("tower", 13)
water_variable <- rep("swc_deep", 13)


#-1/gpp$n.used + c(-2,2)/sqrt(gpp$n.used)
# -0.1400442  0.1308699

#Find first order autocorrelation where the lag is "0"

# Check first order lag

cor.test(x = monthly_modis_ts[,c("swc_deep_mean")], y = monthly_modis_ts[,c("gpp_mean")],
         alternative = "two.sided", conf.level = 0.99)


#Get confidence interval
qnorm((1.99)/2)/sqrt(gpp_2$n.used) # - Based on the function for plotting acf's
# 0.1803441


gpp_timelag_deep_df <- data.frame(R_statistic, Lag, plant_type, water_variable)

gpp_timelag_deep_df <- gpp_timelag_deep_df %>%
  mutate(sig_level = ifelse(R_statistic > -0.1803441 & R_statistic < 0.1803441, "Not Sig.", "Sig.")) # OLD R_statistic > -0.140044 & R_statistic < 0.1308699

timelag_df <- rbind(timelag_df, gpp_timelag_deep_df)


#REORDER VARIABLES
timelag_df$water_variable <- factor(timelag_df$water_variable, levels = c("swc_shallow", "swc_deep"))

timelag_df$plant_type <- factor(timelag_df$plant_type, levels = c('tower', 'modis', 'tree', 'understory'))

#-----------------PLOTS FOR MONTHLY VARIABLES----------------

# New facet label names for supp variable
water.labs <- c("SWC Shallow", "SWC Deep")
names(water.labs) <- c("swc_shallow", "swc_deep")

plant.lag.labs <- c("Tower GPP", "MODIS GCC", "Trees GCC", "Understory GCC")
names(plant.lag.labs) <- c("tower", "modis", "tree", "understory")


time_lag.plot <- timelag_df  %>%
  ggplot(mapping = aes(x = Lag, 
                       y = R_statistic
  )
  ) +
  geom_point(alpha = 1, size = 2) +
  geom_line(aes()) +
  plot.theme.ts +
  xlab("Time Lag (months)") +
  ylab("Correlation Coefficient (R)") +
  geom_hline(yintercept = 0) +
  ylim(-1, 1) +
  xlim(0,8) +
  geom_text(aes(label = ifelse(sig_level=="Sig.", "*", "")),
            color = "black", angle = 0, size = 8,
            vjust = 0.2) +
  labs(shape = "Water Variable") +
  scale_shape_discrete(name = "Water Variable", labels = water.labs) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  facet_grid(plant_type ~ water_variable
             #,labeller = labeller(water_variable = water.labs,
             #                     plant_type = plant.lag.labs)
             ) +
  theme(strip.text.x = element_blank(),
        strip.text.y = element_blank())

capletters <- c("A: SWC Shallow and Tower GPP", 
                "B: SWC Deep and Tower GPP",
                "C: SWC Shallow and MODIS GCC",
                "D: SWC Deep and MODIS GCC",
                "E: SWC Shallow and Trees GCC", 
                "F: SWC Deep and Trees GCC",
                "G: SWC Shallow and Understory GCC",
                "H: SWC Deep and Understory GCC")


#Add tags
tag_facet(time_lag.plot , size = 5, tag_pool = capletters,
          open = " ", close = " ", vjust = 0.95, hjust = -0.1) 



rm(time_lag.plot)


#########################
#Phenometrics - SET UP - GPP Versus UD, RD, GSL
########################

#SET WD 
setwd("F:/ / /")

#Read in the phenometric data
pheno_met <- read.csv(file = "phenometrics_median_2015-20_rerun.csv",
                      stringsAsFactors = FALSE)

#Seperate phenometrics
pheno_met_tree <- pheno_met %>%
  filter(type == "tree")

pheno_met_grass <- pheno_met %>%
  filter(type == "grass")

#read in modis phenometrics derived from the full year instead of subsetting the year
# for the two growth cycles (trees and understory signals in ecosystem)
modis_met <- read.csv(file = "modis_annual_phenometrics.csv",
                      stringsAsFactors = FALSE)

#modis_met <- read.csv(file = "modis_annual_phenometrics_springStart.csv",
#                      stringsAsFactors = FALSE)


pheno_modis_comb <- modis_met  #NOTE THAT THIS MODIS HAS SPRING UD DATA FROM SPRING MODIS CSV AND DD, RD, AND PSR from 2020 SUMMER MODIS DATA

rm(modis_met)

#########################
#GPP Phenometrics vs MODIS Phenometrics
########################

#Filter out 2004 since dont have 2003 winter water; add GSL
pheno_modis_comb <- pheno_modis_comb %>%
  filter(year != 2004) %>% #do not need 2004 because cannot use it since no SWC 2003 data
  mutate(GSL = RD - UD) 


#Get annual totals of gpp

pheno_gpp_annual <- pheno_modis_comb %>%
  mutate(gpp_total = yearly_gpp_df$annual_gpp)

#SCATTER PLOT
gsl.modis.gpp.plot <- pheno_gpp_annual %>%
  #filter(year != 2019 & year != 2020) %>%
  ggplot(mapping = aes(x = GSL, 
                       y = gpp_total)) +
  geom_point(alpha = 1, size = 2) +
  #geom_smooth(method = 'lm', formula = "y ~ x") + 
  #ggtitle("Growing Season Length vs. Total GPP") +
  xlab("GSL (days)") +
  ylim(0, 600) +
  labs(y = expression(Total~GPP~(g~CO[2]~m^-2~d^-1))) +
  #annotate(geom = "text", x = 80, y = 500, label = "R2 adj.= 0.18") +
  #geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme_phenoMet_gpp 


#SCATTER PLOT
ud.modis.gpp.plot <- pheno_gpp_annual %>%
  #filter(year != 2019 & year != 2020) %>%
  ggplot(mapping = aes(x = UD, 
                       y = gpp_total)) +
  geom_point(alpha = 1, size = 2) +
  #geom_smooth(method = 'lm', formula = "y ~ x") + 
  #ggtitle("Upturn Date (UD) vs. Total GPP") +
  xlab("UD (DOY)") +
  ylim(0, 600) +
  labs(y = NULL) +
  #annotate(geom = "text", x = 100, y = 500, label = "R2 adj.= -0.06") +
  #geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme_phenoMet_gpp 


#SCATTER PLOT
rd.modis.gpp.plot <- pheno_gpp_annual %>%
  #filter(year != 2019 & year != 2020) %>%
  ggplot(mapping = aes(x = RD, 
                       y = gpp_total)) +
  geom_point(alpha = 1, size = 2) +
  geom_smooth(method = 'lm', formula = "y ~ x", se = F, color = "black") + 
  #ggtitle("Recession Date (RD) vs. Total GPP") +
  xlab("RD (DOY)") +
  ylim(0, 600) +
  labs(y = NULL) +
  annotate(geom = "text", x = 250, y = 500, size = 5,
           label = expression(R^2~'='~'0.69*')) +
  #geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme_phenoMet_gpp 

#Group the plots together
ggpubr::ggarrange(gsl.modis.gpp.plot,
                  ud.modis.gpp.plot, 
                  rd.modis.gpp.plot,
                  align = "hv", #align in both directions / axes
                  ncol=3, nrow=1, common.legend = TRUE, legend="top",
                  labels = "AUTO")


#STATS
##
modis_GSL_gpp_total.lm <- lm(pheno_gpp_annual$gpp_total ~ pheno_gpp_annual$GSL)
summary(modis_GSL_gpp_total.lm ) 

cor.test(x = pheno_gpp_annual$GSL, y = pheno_gpp_annual$gpp_total)
##

##
modis_RD_gpp_total.lm <- lm(pheno_gpp_annual$gpp_total ~ pheno_gpp_annual$RD)
summary(modis_RD_gpp_total.lm ) 

cor.test(x = pheno_gpp_annual$RD, y = pheno_gpp_annual$gpp_total)
##

##
modis_UD_gpp_total.lm <- lm(pheno_gpp_annual$gpp_total ~ pheno_gpp_annual$UD)
summary(modis_UD_gpp_total.lm ) 

cor.test(x = pheno_gpp_annual$UD, y = pheno_gpp_annual$gpp_total)
##

##
modis_SD_gpp_total.lm <- lm(pheno_gpp_annual$gpp_total ~ pheno_gpp_annual$SD)
summary(modis_SD_gpp_total.lm ) 

cor.test(x = pheno_gpp_annual$SD, y = pheno_gpp_annual$gpp_total)
##

##
modis_DD_gpp_total.lm <- lm(pheno_gpp_annual$gpp_total ~ pheno_gpp_annual$DD)
summary(modis_DD_gpp_total.lm ) 

cor.test(x = pheno_gpp_annual$DD, y = pheno_gpp_annual$gpp_total)
##


#CLEAN UP

rm(rd.modis.gpp.plot, ud.modis.gpp.plot, gsl.modis.gpp.plot,
   sd.modis.gpp.plot)

#############
# EXAMPLE PHENOMETRIC CORRELATION SET UP
# For Tree RD, UD, and GSL and Winter Water
###############
#get rolling over spring water and test correlations with Tree phenometrics 
phenoMet_spring_water_df <-  main_df %>%
  filter(water_year == 2015 | water_year == 2016 | water_year == 2018 | water_year == 2019 | water_year == 2020) %>% #2017 ignored because it did not capture full tree cycle
  filter(sub_season == "spring") %>%
  #filter(month == 11 | month == 12 | month == 1 | month == 2) %>%
  select(water_year, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, swc_deep_avg) %>%
  group_by(water_year) %>%
  dplyr::summarize(
    spring_swc_130 = mean(swc_130_avg, na.rm = TRUE),
    spring_swc_130_sd = standard_error(swc_130_avg),
    spring_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    spring_swc_deep_sd = standard_error(swc_deep_avg),
    spring_precip = sum(precip, na.rm = TRUE),
    spring_swc_all = mean(swc_all_avg, na.rm = TRUE),
    spring_swc_all_sd = standard_error(swc_all_avg),
    spring_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    spring_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5, na.rm = T)
  ) %>%
  dplyr::rename(year = water_year) %>%
  #filter(roll_season != "NA" & roll_season != "cont_2016_2017") %>%
  #cbind(pheno_met_tree) #alterantive method
  merge(., pheno_met_tree) 


#FOR NON-spring, INSTEAD OF WATER YEAR TO CAPTURE THE OVERLAP, SIMPLY USE YEAR 

#Plot deep water in spring vs GSL
rd_tree_deep.plot  <- phenoMet_spring_water_df %>%
  #filter(year != 2018) %>%
  ggplot(mapping = aes(x = spring_swc_deep, 
                       y = RD_med)) +
  #geom_errorbar(aes(ymin = GSL_med - GSL_mad, ymax = GSL_med + GSL_mad),
  #              color = "gray") +
  #geom_errorbarh(aes(xmin = spring_swc_deep - spring_swc_deep_sd, 
  #                   xmax = spring_swc_deep + spring_swc_deep_sd),
  #               color = "gray") +
  geom_point(alpha = 1, size = 2) +
  #geom_smooth(method = 'lm', formula = "y ~ x") + 
  ggtitle("End of Season vs. spring SWC") +
  xlab("November to Feburary SWC: 45-130 cm") +
  #xlim(5, 10) +
  ylab("RD (days)") +
  geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme 

# STATS
gsl_deep <- lm(phenoMet_spring_water_df$GSL_med ~ phenoMet_spring_water_df$spring_swc_deep)
summary(gsl_deep) 

rd_deep <- lm(phenoMet_spring_water_df$RD_med ~ phenoMet_spring_water_df$spring_swc_deep)
summary(rd_deep) 

#Pearson's correlation test, deafults to a two.sided test
cor.test(x = phenoMet_spring_water_df$spring_swc_deep, y = phenoMet_spring_water_df$RD_med)

#Then write to Excel (skills at this point was not the best yet :( )

write.excel(phenoMet_spring_water_df)

#########################
#EXAMPLE Phenometrics GRASS WITH  AUGUST VARIABLES
########################

#GET JulAug RAIN AND July and JulAugust
grass_phenoMet_JulAug_water_df <-  main_df %>%
  select(year, DOY, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, gpp_avg) %>%
  #group_by(year, month) %>%
  filter(month == 7 | month == 8) %>%
  #filter(DOY >= 202 & DOY < 243) %>% # Testing the month timing but with months overlapping, results in a stronger correlation
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    JulAug_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    JulAug_swc_deep_sd = standard_error(swc_deep_avg),
    JulAug_precip = sum(precip, na.rm = TRUE),
    JulAug_swc_all = mean(swc_all_avg, na.rm = TRUE),
    JulAug_swc_all_sd = standard_error(swc_all_avg),
    JulAug_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    JulAug_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events_5 = sum(precip > 5),
    precip_events_10 = sum(precip > 10),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg),
    JulAug_swc_shallow_cov = COV_error(swc_shallow_avg),
    gpp_mean = mean(gpp_avg, na.rm = T),
    swc_events_7 = sum(swc_shallow_avg > 7) #how many days were above 7 % swc_shallow for grass - tree interaction
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)


#Shallow spring and SWC

#INVERSE THE X VARIABLE THE PATTERN "MAKES SENSE" but is that right? Can Read as more water is being used OR competition with trees
gsl_grass_shallow_JulAug.plot  <- grass_phenoMet_JulAug_water_df %>%
  filter(year != 2020) %>%
  ggplot(mapping = aes(x =  JulAug_swc_shallow,  # (1/JulAug_swc_shallow)
                       y = RD_med)) +
  geom_point(alpha = 1, size = 2
             #aes(color = T_mean)
  ) +
  #geom_smooth(method = 'lm', formula = "y ~ x") + 
  #ggtitle("RD vs. Jul-Aug SWC") +
  xlab("JulAug SWC: 2.5-20 cm") +
  #xlim(5, 10) +
  ylab("GSL (days)") +
  geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  #geom_text(aes(label = JulAug_precip), hjust = -1.0, vjust = -0.6) +
  plot.theme

RD_JulAug_shallow <- lm(grass_phenoMet_JulAug_water_df$RD_med ~ grass_phenoMet_JulAug_water_df$JulAug_swc_shallow)
summary(RD_JulAug_shallow) #adj. Rsq = 0.8992  and p-value = 0.009  and RSE = 7.176 ; F-tat = 36.7


#Correlation
cor.test(grass_phenoMet_aug_water_df$aug_swc_shallow, grass_phenoMet_aug_water_df$RD_med)



#########################
#EXAMPLE Phenometrics MODIS WITH Winter VARIABLES
########################

#GET Winter water and test correlation with MODIS phenometrics
modis_phenoMet_winter_water_df <-  flux_df %>% #using flux df since they have same length in time
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  select(year, month, DOY, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(month == 11 | month == 12 | month == 1 | month == 2) %>% #month == 3 | month == 4 |
  #filter(DOY >= 175 & DOY < 198) %>% #DOY >= 175 & DOY < 198
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  summarize(
    winter_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    winter_swc_deep_sd = standard_error(swc_deep_avg),
    winter_precip = sum(precip, na.rm = TRUE),
    winter_swc_all = mean(swc_all_avg, na.rm = TRUE),
    winter_swc_all_sd = standard_error(swc_all_avg),
    winter_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    winter_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg)
  ) %>%
  merge(pheno_modis_comb) %>%
  mutate(GSL = RD - UD)

#Shallow spring and SWC

gsl_grass_shallow_winter.plot  <- modis_phenoMet_winter_water_df %>%
  filter(year != 2020) %>%
  ggplot(mapping = aes(x = winter_swc_deep, 
                       y =  RD)) +
  geom_point(alpha = 1, size = 2) +
  geom_smooth(method = 'lm', formula = "y ~ x", se = F, color = "black") + 
  ggtitle("Response of SavannaGSL to Deep Winter Soil Moisture") +
  xlab("Winter Soil Moisture (%)") +
  #xlim(5, 10) +
  ylab("Growing Season Length (days)") +
  annotate(geom = "text", x = 6, y = 340, label = "R2 adj.= 0.52*", size = 6) +
  #geom_text(aes(label = year), hjust = -0.3, vjust = -0.5) +
  plot.theme


modis_rd_winter_shallowSwc.lm <- lm(modis_phenoMet_winter_water_df$RD ~ modis_phenoMet_winter_water_df$winter_swc_shallow)
summary(modis_rd_winter_shallowSwc.lm) 

# the [-16] removes 2020 since RD did not model 
modis_rd_winter_deepSwc.lm <- lm(modis_phenoMet_winter_water_df$RD[-16] ~ modis_phenoMet_winter_water_df$winter_swc_deep[-16])
summary(modis_rd_winter_deepSwc.lm) # adj. Rsq = 0.52  and p-value < 0.01 and RSE = 20.35 ; F-tat: 15.35



modis_dd_winter_shallowSwc.lm <- lm(modis_phenoMet_winter_water_df$GSL ~ modis_phenoMet_winter_water_df$winter_swc_deep)
summary(modis_dd_winter_shallowSwc.lm) # adj. Rsq = 0.3962  and p-value = 0.007 and RSE = 20.33 ; F-tat: 10.19

#Correlation test for R
cor.test(x = modis_phenoMet_winter_water_df$winter_swc_deep, y=modis_phenoMet_winter_water_df$GSL)

######################################
#BOX PLOT with each ROI phenometrics - Supplemental 
######################################
#SET WD 
setwd("F:/ / /")

#Read in the phenometric data
pheno_met_ind <- read.csv(file = "Individual_Phenometric_Output_2015-20.csv",
                      stringsAsFactors = FALSE)

pheno_met_ind$type[which(pheno_met$type == "grass")] <- "Understory"
set.seed(123) #set the randomness 
type.labs <- c("tree" = "Trees", "Understory" = "Understory")

class(pheno_met_ind$type)

pheno_met_ind$type <- as.factor(pheno_met_ind$type)

#create boxplot to see distribution of UD for start of seasons
pheno_met_ind %>%
  #filter(type != "tree") %>%
  melt(id.vars = c("type", "year"),
       variable.name = "variable",
       value.name = "value") %>%
  #dplyr::filter(substr(variable, 4, 6) == "med") %>%
  dplyr::filter(variable == "UD" | variable == "RD") %>%
  drop_na() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(alpha = 0) +
  facet_wrap(~type, labeller = labeller(type = type.labs)) +
  ylab("DOY") +
  xlab("Phenometric") +
  ggtitle("Boxplots of Tree and Understory Upturn and Recession Dates") +
  theme(axis.title.x = element_text(color = "black", 
                                    size = 16), 
        axis.title.y = element_text(color = "black", 
                                    size = 16),                                                                       
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        #legend.title = element_blank(),
        legend.text = element_text(size = 14),
        #legend.position = c(1,1),
        #legend.justification = c(1,1),
        plot.title = element_text(color = "black",
                                  size = 16, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 14, color = "black", face = "bold"),
        strip.background = element_rect(
          color="black", fill="white", linetype="solid"
        )
  )


#MODIS boxplot of upturn and recession dates
pheno_modis_comb %>%
  #filter(type != "tree") %>%
  melt(id.vars = c("year"),
       variable.name = "variable",
       value.name = "value") %>%
  #dplyr::filter(substr(variable, 4, 6) == "med") %>%
  dplyr::filter(variable == "UD" | variable == "RD") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot(alpha = 0) +
  geom_jitter() +
  #facet_wrap(~type, labeller = labeller(type = type.labs)) +
  ylab("DOY") +
  xlab("Phenometric") +
  ggtitle("Boxplots of MODIS (Ecosystem) Upturn and Recession Dates") +
  theme(axis.title.x = element_text(color = "black", 
                                    size = 16), 
        axis.title.y = element_text(color = "black", 
                                    size = 16),                                                                       
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        #legend.title = element_blank(),
        legend.text = element_text(size = 14),
        #legend.position = c(1,1),
        #legend.justification = c(1,1),
        plot.title = element_text(color = "black",
                                  size = 16, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        strip.text.x = element_text(
          size = 14, color = "black", face = "bold"),
        strip.text.y = element_text(
          size = 14, color = "black", face = "bold"),
        strip.background = element_rect(
          color="black", fill="white", linetype="solid"
        )
  )

#########################
#Pheno_Modis Bar Chart
########################
# SET UP
#SET WD 
setwd("F:/ / /")

# SET CORRECT PATH TO CSV
#Corrected data for water year 
pheno_modis_stats <- read.csv(file = "F:/.../Phenometric_R_stats_Updated_2_DOY285.csv",
                              stringsAsFactors = FALSE)

#Labels
plant.labs = c("Modis")

water.labs <- c("Shallow SWC", "Deep SWC")

time.grass.labs <- c("JanFeb", "MAMJ", "MarApr", 
                     "L-June_E-July", "July", "August", "JulyAug",
                     "Sept", "Oct", "JJASO", 
                     "JASO", "NDJF", "GPP")

#Add symbology for graph
pheno_modis_stats <- pheno_modis_stats %>%
  mutate(sig_value = ifelse(p_value <= 0.05, "*", ifelse(p_value <= 0.1, "", "")
  ))

#Factoring
pheno_modis_stats$sig_value <- as.factor(pheno_modis_stats$sig_value)

pheno_modis_stats$phenometric <- factor(pheno_modis_stats$phenometric, levels=c("UD", "UD_annual", "SD", "DD", "RD", "GSL",
                                                                                "gpp UD", "gpp SD", "gpp DD", 
                                                                                "gpp RD", "gpp CUP", "gpp summer sum",
                                                                                "gpp annual sum")
)

#Factoring
pheno_modis_stats$variable <- factor(pheno_modis_stats$variable, levels=c("Shallow SWC", "Deep SWC")
)


pheno_modis_stats$time_period_f <- factor(pheno_modis_stats$time_period, levels=time.grass.labs
)

##########
# PLOT FOR MODIS BAR CHART BETWEEN DEPTH AND SEASON 
#########

pheno_modis_stats.modis_plot  <- pheno_modis_stats %>%
  filter(plant_type == "Modis") %>%
  filter(phenometric != "SD" & phenometric != "DD") %>%
  ggplot(mapping = aes(x = phenometric, 
                       y = r_statistic,
                       fill = ifelse(phenometric == "UD" & sub_season == "Summer", "Sub Seasonal",
                                     ifelse(phenometric == "SD" & sub_season == "Summer", "Sub Seasonal",
                                            "Seasonal")
                       )
  )) +
  geom_bar(stat = "identity", position = "dodge", size = 1, 
           color = "black", fill = "white",
           width = 0.2) +
  #ggtitle("Pearson Correlations between MODIS Phenometrics and Water") +
  xlab("Phenometrics") +
  #xlim(5, 10) +
  geom_hline(yintercept = 0) +
  ylab("R Correlation Coefficient") +
  #labs(title = expression(Pearson~Correlations~between~bold(MODIS)~Phenometrics~and~Water)) +
  ylim(-1.0, 1.0) +
  geom_text(aes(label = sig_value), hjust = 0.5, vjust = 0.4, 
            position = position_dodge(0.9), check_overlap = F, size = 6) +
  plot.theme.modis.phenometric + 
  #geom_text(aes(label = r_sq_thresh)) +
  facet_grid(sub_season ~ variable) +
  #scale_color_manual(values=c('orange' )) +
  #scale_fill_manual(name = "Critical Climate Period", values = c("white", "yellow")) 
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  labs(fill = "Phenometrics",
       color = "Time Period")


#Add tags
capletters <- c("A", "B" , "C", "D", "E", "F")
  
pheno_modis_stats.modis_plot <- tag_facet(pheno_modis_stats.modis_plot, 
                                          size = 5, tag_pool = capletters,
                                          open = " ", close = " ", vjust = 2.0) 



###################
# Phenometric of RD versus SWC between Trees and Understory
###################
setwd("F:/ / /")

#Read in the data created from example script
pheno_trend_df <- read.csv(file = "Pheno_Trend_Fig.csv",
                           stringsAsFactors = FALSE)

#create labels
depth.labs = c(`Spring` = "Spring Deep SWC (depth > 31 cm)",
  `Jul_Aug` = "July and August Shallow SWC (depth < 31 cm)")
               
names(depth.labs) <- c('Spring', 'Jul_Aug')


pheno_trend_df$time_period <- factor(pheno_trend_df$time_period, levels = c('Spring', 'Jul_Aug'))

#Plot deep water in winter vs GSL
pheno_trend_fig <- pheno_trend_df %>%
  ggplot(mapping = aes(x = SWC, 
                       y = RD_med, color = plant_type,
                       shape = plant_type#, label = year
                       )) +
  geom_point(alpha = 0.7, size = 4) +
  geom_smooth(data = pheno_trend_df %>% filter(plant_type == "Tree" & time_period == "Spring"),
              method = 'lm', formula = "y ~ x", se = FALSE, color = "blue") +
  geom_smooth(data = pheno_trend_df %>% filter(plant_type == "Understory" & time_period == "Jul_Aug"),
              method = 'lm', formula = "y ~ x", se = FALSE, color = "red") +
  geom_smooth(data = pheno_trend_df %>% filter(plant_type == "MODIS" & time_period == "Spring"),
              method = 'lm', formula = "y ~ x", se = FALSE, color = "#666666") +
  #ggtitle("Response of Recession Date to Soil Water Content") +
  #geom_text(hjust = -0.3, vjust = 0) +
  xlab("SWC (%)") +
  xlim(3, 9) +
  ylab("Recession Date (DOY)") +
  facet_wrap(~time_period
             ,labeller = labeller(time_period = depth.labs)
             ) +
  theme(legend.position = "top") +
  #scale_color_discrete(name = "Medium") +
  scale_color_manual(name = "Medium", 
                     values = c("#666666", "blue", "red"),
                                labels = c("MODIS", "Tree", "Understory")) +
  scale_shape_manual(name = "Medium", labels = c("MODIS", "Tree", "Understory"),
                     values = c(15, 19, 17)) +
  guides(shape = guide_legend(override.aes = list(color = c("#666666", "blue", "red")))) +
  plot.theme.phenometric 

#Trends

tree_phenoSpring_df <- pheno_trend_df %>%
  filter(plant_type == "Tree" & time_period == "Spring")

tree_phenoJulAug_df <- pheno_trend_df %>%
  filter(plant_type == "Tree" & time_period == "Jul_Aug")

tree_swc_Spring.lm <- lm(tree_phenoSpring_df$RD_med ~ tree_phenoSpring_df$SWC)
summary(tree_swc_Spring.lm) 

tree_swc_JulAug.lm <- lm(tree_phenoJulAug_df$RD_med ~ tree_phenoJulAug_df$SWC)
summary(tree_swc_JulAug.lm ) 



grass_phenoSpring_df <- pheno_trend_df %>%
  filter(plant_type == "Understory" & time_period == "Spring")

grass_phenoJulAug_df <- pheno_trend_df %>%
  filter(plant_type == "Understory" & time_period == "Jul_Aug")

grass_swc_Spring.lm <- lm(grass_phenoSpring_df$RD_med ~ grass_phenoSpring_df$SWC)
summary(grass_swc_Spring.lm) 

grass_swc_JulAug.lm <- lm(grass_phenoJulAug_df$RD_med ~ grass_phenoJulAug_df$SWC)
summary(grass_swc_JulAug.lm) 

