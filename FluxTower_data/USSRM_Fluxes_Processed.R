#Script to organize and process flux tower data

# Blake Steiner


#Install and load packages, aka groups of functions

# install.packages("tidyverse")

library("tidyverse")
library("dplyr")
library("lubridate")
library("RcppRoll")
#library("gmodels")
options(digits.secs=10) #this changes the representation of subseconds, if you have them, from a few
                        # digits to 10 digits.

#set working directory
setwd("E:/2019_Data_MS/Pheno_GPP_SWC_paper_redo/FluxTower_data/")


#Set input and output directories 
DirInp <- "E:/2019_Data_MS/Pheno_GPP_SWC_paper_redo/FluxTower_data/"

OutDir <- "E:/2019_Data_MS/Pheno_GPP_SWC_paper_redo/FluxTower_data/Daily_Data/"



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

#Substitute NAs to NaNs #function to find NaNs and make them NAs
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#create mega data frame to rbind (bind by rows) all data to
OUT <- data.frame() 

for (FILE in whr_file) {
  
  #For testing purposes, remove the comment from FILE to just use a specific input csv or table
  #FILE <- 1 
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
  
  #flux_df_2018 <- read.csv("GapfilledPartitionedFluxes_US-SRM_HH_201712312330_201812312330.csv",
  #                         header = T, na.strings = -9999.00)

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

  flux_df <- rename(flux_df,
                  Time = timeA)



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
  
  #filter for daily values of GPP and respiration
  #flux_filter <- flux_df %>%
  #  select(DOY, PPFD_IN, PPFD_IN_F,
  #         GPP, RECO, Time) %>%
  #  filter(PPFD_IN_F > 30) %>%
  #  group_by(DOY) %>%
  #  summarise(ppfd_in_avg = mean(PPFD_IN, na.rm = T),
  #            ppfd_in_sd =  sd(PPFD_IN, na.rm = T),
  #            ppfd_in_f_avg = mean(PPFD_IN_F, na.rm = T),
  #            ppfd_in_f_sd =  sd(PPFD_IN_F, na.rm = T),
  #            gpp_avg = mean(GPP, na.rm = T),
  #            gpp_sd = sd(GPP, na.rm = T),
  #            reco_avg = mean(RECO, na.rm = T),
  #            reco_sd = sd(RECO, na.rm = T)
  #            ) %>%
  #  mutate(date = base::as.Date(x = DOY, origin = paste(year, "-", "01-01", sep = "")) - 1 #used to correct date
  #         )
  
  #plot(x = flux_filter$DOY, flux_filter$gpp_avg)
  
  #test difference of day vs day and night values
  flux_Nofilter <- flux_df %>% #this takes the flux_df data frame and pipes it to another function
    select(DOY, PPFD_IN, PPFD_IN_F, #this selects certain columns
           GPP, RECO, Time) %>%
    #filter(PPFD_IN_F > 30) %>% #if you want to subset or filter by something
    group_by(DOY) %>% #how do you want to group the variables by
    summarise(ppfd_in_avg = mean(PPFD_IN, na.rm = T),
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
  
  
  # Multiple box plots
  #par(mfrow = c(1,2))
  #title('GPP with night values (left) and with PPFD > 30 (right)')
  #boxplot(flux_Nofilter$gpp_avg)
  #boxplot(flux_filter$gpp_avg)
  
  #Histogram for distributions 
  #hist(flux_df$SWC_1_1_A)
  #hist(flux_df$SWC_1_5_A)
  #hist(flux_df$SWC_1_8_A)
  
  
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
           #swc_all_sd = sd(c(SWC_1_1_A,  # COMMENTED OUT variability measures as that should be applied to the daily, not across layers, metric
            #                    SWC_1_2_A,
            #                    SWC_1_3_A,
            #                    SWC_1_4_A,
            #                    SWC_1_5_A,
            #                    SWC_1_6_A,
            #                    SWC_1_7_A,
            #                    SWC_1_8_A), 
            #                  na.rm = T),
           swc_shallow_avg = mean(c(SWC_1_1_A,
                                    SWC_1_2_A,
                                    SWC_1_3_A,
                                    SWC_1_4_A), 
                                  na.rm = T),
           #swc_shallow_sd = sd(c(SWC_1_1_A,
            #                        SWC_1_2_A,
            #                        SWC_1_3_A,
            #                     SWC_1_4_A), 
            #                      na.rm = T),
          # swc_middle_avg = mean(c(SWC_1_4_A,  # REMOVED TO HAVE SHALLOW VS. DEEP
          #                         SWC_1_5_A,
          #                         SWC_1_6_A), 
          #                    na.rm = T),
          # swc_middle_sd = sd(c(SWC_1_4_A,
          #                         SWC_1_5_A,
          #                         SWC_1_6_A), 
          #                       na.rm = T),
           swc_deep_avg = mean(c(SWC_1_5_A,  #DEEP BEING 45 cm to 130 cm
                                 SWC_1_6_A,
                                 SWC_1_7_A,
                                 SWC_1_8_A), 
                              na.rm = T)
           #swc_deep_sd = sd(c(SWC_1_5_A,
          #                    SWC_1_6_A,
          #                    SWC_1_7_A,
          #                    SWC_1_8_A), 
          #                     na.rm = T)
    )
    
    #Bring all daily averages together
    
    flux_env_df <- flux_env_df %>%
    group_by(DOY) %>%
    summarise(vpd_avg = mean(VPD, na.rm = T),
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
              #swc_middle_avg = mean(swc_middle_avg, na.rm = T),
              #swc_middle_sd = sd(swc_middle_sd, na.rm = T),
              swc_deep_sd = sd(swc_deep_avg, na.rm = T),
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



