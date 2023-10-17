##########################
#-------Read in PhenoCam GCC
##########################
library("lubridate")

#set working directory
setwd("E:/2019_Data_MS/Pheno_GPP_SWC_paper_redo/PhenoCam_GCC/")


#read in
cam_df <- read.csv(file = "gcc_flux_2015-20_rerun.csv",
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

#trinary
cam_df$sub_season <- "summer"
cam_df$sub_season[cam_df$month >= 3 & cam_df$month <=6] <- "spring" #chosen due to Barron-Gafford et al., 2013
cam_df$sub_season[cam_df$month >= 7 & cam_df$month <=10] <- "summer"
cam_df$sub_season[cam_df$month >= 11 & cam_df$month <=2] <- "winter"
cam_df$sub_season[cam_df$DOY >= 60 & cam_df$DOY < 66] <- "winter" #adjusted due to climatologies on phenocam
cam_df$sub_season[cam_df$DOY >= 175 & cam_df$DOY < 306] <- "summer" #adjusted due to climatologies on phenocam


#cam_df$month <- as.numeric(cam_df$month)

#Factoring
cam_df$season <- as.factor(cam_df$season)


cam_df$month <- as.factor(cam_df$month)



cam_df <- cam_df %>%
  mutate(year = lubridate::year(date))

cam_df$year <- as.factor(cam_df$year)

#Check summary stats
summary(cam_df)

#add a week column
cam_df <- cam_df %>%
  mutate(week = lubridate::week(date))

cam_df$week <- as.factor(cam_df$week)


#Factor subseason
cam_df$sub_season <- as.factor(cam_df$sub_season)

#Simplify dataframe
cam_df <- cam_df %>%
  filter(year != 2014) %>%
  select(DOY, year, date, month, tree_gcc_mean, grass_gcc_mean,
         swc_shallow_avg, swc_deep_avg, precip)

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
           grass_gcc_rescale = (grass_gcc_mean - min_gcc_grass) / (max_gcc_grass - min_gcc_grass),
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
