#---------GCC Analysis--------#
#load package
install.packages("phenopix")
library("phenopix")
#library("RColorBrewer")
setwd("E:/2019_Data_MS/Final_Processed_Datasets/Sensitivity")


##########################
#-------Set up CSV 
##########################
#read in
modis_df <- read.csv(file = "MODIS_sat_data_all_samples_GCC.csv",
                    stringsAsFactors = FALSE)


#######
# MODIS ANNUAL
#######

modis_df$date <- as.Date.character(modis_df$date, tryFormats = "%m/%d/%Y")

modis_df <- modis_df %>%
  mutate(year = lubridate::year(date))

years <- c(2004:2020)


#Start with Centerpixel, then NE pixel, then S, SW, W

#choose variable out of gcc_tower_center, gcc_NE, gcc_S, gcc_SW, gcc_W
variable <- "gcc_W"

modis_df_pixel <- modis_df %>%
  select(date, year, variable)

runner = 0

modis_annual_df_to_zoo <- list()

#LOOP FOR SEPERATE YEARS
for (i in years) {
  
  runner = runner + 1
  
  
  modis_temp <- modis_df_pixel %>% 
    filter(year == i) %>%
    select(date, variable) %>%
    drop_na()
  
  
  modis_annual_df_to_zoo[[paste0("modis_df_annual_", i, "_", variable)]] <- modis_temp
  
  
}


rm(runner, modis_temp, modis_df_pixel)

#Turn dataframe objects in list to zoo objects
library("zoo")
zoo_modis_annual_data <- lapply(modis_annual_df_to_zoo, read.zoo)


#Loop around years in pre-monsoon for variables

fit_modis_annual <- lapply(seq_along(zoo_modis_annual_data), 
                           function(x) greenProcess(zoo_modis_annual_data[[x]],
                                                    fit = 'spline', # 
                                                    threshold =  'gu', # 
                                                    plot = TRUE,
                                                    uncert = FALSE, #uncertainty calculation
                                                    nrep = 500, #number of replications
                                                    envelope = 'quantiles', #One between quantiles and min-max
                                                    hydro = FALSE, #determines DOY start, being Jan 1st or Oct 1st
                                                    sf=quantile(zoo_modis_annual_data[[x]], 
                                                                na.rm=TRUE, prob=c(0.05, 0.95)),
                                                    ncores = 4 #number of cores for processing
                           )
) 



#Extract dates for CSV
modis_annual_list <- lapply(seq_along(fit_modis_annual), function(x) rbind(
  fit_modis_annual[[x]][["metrics"]]
))

modis_annual_phenometrics <- do.call("rbind", modis_annual_list)

modis_annual_phenometrics <- data.frame(modis_annual_phenometrics)

modis_annual_phenometrics <- modis_annual_phenometrics %>%
                              mutate(year = years,
                                     pix_local = variable)


#Add each pixel's data to excel to get the statistics
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(modis_annual_phenometrics)

dev.off()

#write.csv(modis_annual_phenometrics,
#          file = "modis_annual_phenometrics.csv",
#          row.names = FALSE)



#paste0("modis_df_annual_", variable)

