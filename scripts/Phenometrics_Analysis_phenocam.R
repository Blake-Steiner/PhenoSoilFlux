##########################
#-------Set up directory
##########################
library("lubridate")
library("tidyverse")
library("reshape2")
library("gridExtra")
library("ggpubr")
library("lsmeans")
library("RColorBrewer")
#library("RColorBrewer")
setwd("F: / / /")


##########################
#-------Set up CSV 
##########################

#read in
main_df <- read.csv(file = "gcc_flux_2015-20_rerun_2_DOY285.csv",
                    stringsAsFactors = FALSE)


#convert from character to date class
main_df$date <- lubridate::mdy(main_df$date)

#Complete date sequence

main_df <- main_df %>%
  complete(date = seq(min(date), max(date), by = "day"),
           fill = list(value = NA))

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
main_df$sub_season[main_df$DOY >= 60 & main_df$DOY < 66] <- "winter" #adjusted due to climatologies on phenocam
main_df$sub_season[main_df$DOY >= 175 & main_df$DOY < 285] <- "summer"

#Factoring
main_df$season <- as.factor(main_df$season)


main_df$month <- as.factor(main_df$month)



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


#main_df$roll_season <- as.factor(main_df$roll_season)


#SUB_SEASON CODE
#  break into "3" seasons: july, aug, sept, oct = monsoon; 
#  nov, dec, jan, feb = post; other = pre




##########################
#-------Functions
##########################

standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x)) #Standard Error, Barnes et al., 2016 for Max EVI across all years


COV_error <- function(x) sd(x, na.rm = T) / mean(x, na.rm = T)


# Create function to correlate through different x and y variables
reg_stats_2x <- function(data, x1, x2, y_vars) {
  
  models_x1 <- lapply(y_vars, function(y) {
    lm(paste(y, x1, sep = " ~ "), data = na.omit(data))
  })
  
  models_x2 <- lapply(y_vars, function(y) {
    lm(paste(y, x2, sep = " ~ "), data = na.omit(data))
  })
  
  stats_x1 <- lapply(models_x1, broom::glance)
  stats_x2 <- lapply(models_x2, broom::glance)
  
  coef_x1 <- lapply(models_x1, broom::tidy)
  coef_x2 <- lapply(models_x2, broom::tidy)
  
  cor_x1 <- lapply(y_vars, function(y) {
    cor(x = data[[x1]], y = data[[y]])
  })
  
  cor_x2 <- lapply(y_vars, function(y) {
    cor(x = data[[x2]], y = data[[y]])
  })
  

  tibble(y_var = y_vars,
         r_squared_x1 = round(sapply(stats_x1, "[[", "r.squared"), 3), 
         adjusted_r_squared_x1 = round(sapply(stats_x1, "[[", "adj.r.squared"), 3),
         p_value_x1 = round(sapply(stats_x1, "[[", "p.value"), 3), 
         f_statistic_x1 = round(sapply(stats_x1, "[[", "statistic"), 3),
         df_residual_x1 = round(sapply(stats_x1, "[[", "df.residual"), 3),
         slope_x1 = round(unlist(sapply(coef_x1, function(coef) coef[2, "estimate"])), 3),
         pearson_corr_x1 = round(unlist(cor_x1), 3),
         r_squared_x2 = round(sapply(stats_x2, "[[", "r.squared"), 3), 
         adjusted_r_squared_x2 = round(sapply(stats_x2, "[[", "adj.r.squared"), 3),
         p_value_x2 = round(sapply(stats_x2, "[[", "p.value"), 3), 
         f_statistic_x2 = round(sapply(stats_x2, "[[", "statistic"), 3),
         df_residual_x2 = round(sapply(stats_x2, "[[", "df.residual"), 3),
         slope_x2 = round(unlist(sapply(coef_x2, function(coef) coef[2, "estimate"])), 3),
         pearson_corr_x2 = round(unlist(cor_x2), 3)
  )
}

#Excel function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


##########################
#-------Plot themes
##########################

plot.theme <- theme(axis.title.x = element_text(color = "black", 
                                                size = 14), 
                    axis.title.y = element_text(color = "black", 
                                                size = 14),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 16, hjust = 0.5, face = "bold"),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 12, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 12, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="black", fill="white", linetype="solid"
                    )
)



#########################
#Phenometrics data loading
########################
setwd("F: / / /")

#Read in the data
pheno_met <- read.csv(file = "phenometrics_median_2015-20_rerun.csv",
                      stringsAsFactors = FALSE)

modis_met <- read.csv(file = "modis_annual_phenometrics.csv",
                      stringsAsFactors = FALSE)

pheno_modis <- modis_met 

rm(modis_met)

#Seperate phenometrics
pheno_met_tree <- pheno_met %>%
  filter(type == "tree")

pheno_met_grass <- pheno_met %>%
  filter(type == "grass")


#########################
#Phenometrics TREES WINTER DOUBLE CHECK LMs FOR ALL SECTIONS
########################

#get rolling winter water
phenoMet_winter_water_df <-  main_df %>%
  filter(water_year == 2015 | water_year == 2016 | water_year == 2018 | water_year == 2019 | water_year == 2020) %>%
  filter(sub_season == "winter") %>%
  #filter(month == 11 | month == 12 | month == 1 | month == 2) %>%
  select(water_year, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, swc_deep_avg) %>%
  group_by(water_year) %>%
  dplyr::summarize(
    winter_swc_130 = mean(swc_130_avg, na.rm = TRUE),
    winter_swc_130_sd = standard_error(swc_130_avg),
    winter_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    winter_swc_deep_sd = standard_error(swc_deep_avg),
    winter_precip = sum(precip, na.rm = TRUE),
    winter_swc_all = mean(swc_all_avg, na.rm = TRUE),
    winter_swc_all_sd = standard_error(swc_all_avg),
    winter_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    winter_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5, na.rm = T)
  ) %>%
  dplyr::rename(year = water_year) %>%
  #filter(roll_season != "NA" & roll_season != "cont_2016_2017") %>%
  #cbind(pheno_met_tree)
  merge(., pheno_met_tree) 

tree.winter.swc_df <- reg_stats_2x(data = phenoMet_winter_water_df, 
             x1 = "winter_swc_shallow",
             x2 = "winter_swc_deep",
             y_vars = c("UD_med", "SD_med",
                        "DD_med", "RD_med",
                        "GSL_med"))

tree.winter.swc_df <- tree.winter.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_winter_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_winter_swc_deep"), .)
                      ))

write.csv(x = tree.winter.swc_df, file = "full_phenometric_stats_tree_winter.csv",
          row.names = F)

#########################
#Phenometrics TREES SUMMER JUST DD
########################

#get rolling summer water
phenoMet_summer_water_df <-  main_df %>%
  filter(year != 2017) %>%
  #filter(month != 11) %>%
  #filter(month != 12) %>%
  #filter(month == 7 | month == 8 | month == 9 | month == 10) %>%
  filter(sub_season == "summer") %>%
  select(year, swc_130_avg, precip, gpp_avg,
         swc_all_avg, swc_shallow_avg, swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  group_by(year) %>%
  dplyr::summarize(
    summer_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    summer_swc_deep_sd = standard_error(swc_deep_avg),
    summer_precip = sum(precip, na.rm = TRUE),
    summer_swc_all = mean(swc_all_avg, na.rm = TRUE),
    summer_swc_all_sd = standard_error(swc_all_avg),
    summer_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    summer_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    gpp_sum = sum(gpp_avg, na.rm = T)
  ) %>%
  #filter(roll_season != "NA" & roll_season != "cont_2016_2017") %>%
  #cbind(pheno_met_tree)
  merge(., pheno_met_tree)

tree.summer.swc_df <- reg_stats_2x(data = phenoMet_summer_water_df, 
                                   x1 = "summer_swc_shallow",
                                   x2 = "summer_swc_deep",
                                   y_vars = c("DD_med", "RD_med", #UD and SD for trees not included because they happened before summer
                                              "GSL_med"))

tree.summer.swc_df <- tree.summer.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_summer_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_summer_swc_deep"), .)
  ))

write.csv(x = tree.summer.swc_df, file = "full_phenometric_stats_trees_summer.csv",
          row.names = F)

#########################
#Phenometrics TREES July and August 
########################

#get JulAug water
tree_phenoMet_JulAug_water_df <- main_df %>%
  filter(year != 2017) %>%
  filter(month == 7 | month == 8) %>%
  #filter(DOY >= 202 & DOY < 243) %>%
  select(year, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  group_by(year) %>%
  dplyr::summarize(
    JulAug_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    JulAug_swc_deep_sd = standard_error(swc_deep_avg),
    JulAug_precip = sum(precip, na.rm = TRUE),
    JulAug_swc_all = mean(swc_all_avg, na.rm = TRUE),
    JulAug_swc_all_sd = standard_error(swc_all_avg),
    JulAug_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    JulAug_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T)
  ) %>%
  #filter(roll_season != "NA" & roll_season != "cont_2016_2017") %>%
  merge(pheno_met_tree)

tree.JulAug.swc_df <- reg_stats_2x(data = tree_phenoMet_JulAug_water_df, 
                                   x1 = "JulAug_swc_shallow",
                                   x2 = "JulAug_swc_deep",
                                   y_vars = c("DD_med", "RD_med", #UD and SD removed as they occur before July
                                              "GSL_med"))

tree.JulAug.swc_df <- tree.JulAug.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_JulAug_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_JulAug_swc_deep"), .)
  ))

write.csv(x = tree.JulAug.swc_df, file = "full_phenometric_stats_trees_JulAug.csv",
          row.names = F)

#########################
#Phenometrics TREES SPRING 
########################

#get rolling MarApr water
tree_phenoMet_MarApr_water_df <- main_df %>%
  filter(year == 2015 | year == 2016 | year == 2018 | year == 2019 | year == 2020) %>%
  filter(year != 2014) %>%
  #filter(water_year != 2017 & water_year != 2004 & water_year != 2005
  #       & water_year !=2006 & water_year != 2007 & water_year != 2008
  #       & water_year !=2009 & water_year !=2010 & water_year !=2011
  #       & water_year !=2012 & water_year !=2013 & water_year !=2014) %>%
  filter(sub_season == "spring") %>%
  #filter(month == 3 | month == 4| month == 5 | month == 6) %>% # | month == 5 | month == 6
  #filter(DOY >= 202 & DOY < 243) %>%
  select(year, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  group_by(year) %>%
  dplyr::summarize(
    MarApr_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    MarApr_swc_deep_sd = standard_error(swc_deep_avg),
    MarApr_precip = sum(precip, na.rm = TRUE),
    MarApr_swc_all = mean(swc_all_avg, na.rm = TRUE),
    MarApr_swc_all_sd = standard_error(swc_all_avg),
    MarApr_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    MarApr_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T)
  ) %>%
 dplyr::rename(water_year = year) %>%
  #filter(roll_season != "NA" & roll_season != "cont_2016_2017") %>%
  #merge(., pheno_met_tree) 
  cbind(pheno_met_tree)

tree.MarApr.swc_df <- reg_stats_2x(data = tree_phenoMet_MarApr_water_df, 
                                   x1 = "MarApr_swc_shallow",
                                   x2 = "MarApr_swc_deep",
                                   y_vars = c("UD_med", "SD_med",
                                              "DD_med", "RD_med",
                                              "GSL_med"))

tree.MarApr.swc_df <- tree.MarApr.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_MarApr_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_MarApr_swc_deep"), .)
  ))

write.csv(x = tree.MarApr.swc_df, file = "full_phenometric_stats_trees_MarApr.csv",
          row.names = F)

#########################
#Phenometrics GRASS SUMMER
########################
#GET SUMMER RAIN

grass_phenoMet_summer_water_df <- main_df %>%
  select(year, sub_season, month, swc_130_avg, precip, gpp_avg,
         swc_all_avg, swc_shallow_avg,
         swc_deep_avg, ppfd_in_avg, Tair_avg, Tsoi_2.5_10_avg) %>%
  #group_by(year, month) %>%
  filter(year == 2015 | year == 2016 | year == 2017 | year == 2018 | year == 2019) %>%
  filter(sub_season == "summer") %>%
  #filter(month == 7 | month == 8 | month == 9 | month == 10) %>%
  group_by(year) %>%
  dplyr::summarize(
    summer_precip = sum(precip, na.rm = TRUE),
    summer_swc_all = mean(swc_all_avg, na.rm = TRUE),
    #summer_swc_5_10 = mean(swc_5_10_avg, na.rm = TRUE),
    summer_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    summer_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    summer_tair_max = max(Tair_avg, na.rm = TRUE),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    precip_events = sum(precip > 5),
    Tsoi_avg = mean(Tsoi_2.5_10_avg, na.rm = T),
    gpp_sum = sum(gpp_avg, na.rm = T),
    gpp_mean = mean(gpp_avg, na.rm = T)
    #summer_grass_gcc_max = max(modis_gcc_srm, na.rm = T)
  ) %>%
  #filter(year != 2017) %>% # uncomment when using grass instead of MODIS
  merge(pheno_met_grass)


grass.summer.swc_df <- reg_stats_2x(data = grass_phenoMet_summer_water_df, 
                                   x1 = "summer_swc_shallow",
                                   x2 = "summer_swc_deep",
                                   y_vars = c("UD_med", "SD_med",
                                              "DD_med", "RD_med",
                                              "GSL_med"))

grass.summer.swc_df <- grass.summer.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_summer_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_summer_swc_deep"), .)
  ))

write.csv(x = grass.summer.swc_df, file = "full_phenometric_stats_grass_summer.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH SPRING VARIABLES NOTHING
########################

#GET APRIL AND MARCH RAIN AND MAY AND JUNE
grass_phenoMet_MarApr_water_df <-  main_df %>%
  filter(sub_season == "spring") %>%
  select(year, month, DOY, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  #filter(month == 3 | month == 4 | month == 5 | month == 6) %>% 
  filter(year != 2020) %>%
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    MarApr_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    MarApr_swc_deep_sd = standard_error(swc_deep_avg),
    MarApr_precip = sum(precip, na.rm = TRUE),
    MarApr_swc_all = mean(swc_all_avg, na.rm = TRUE),
    MarApr_swc_all_sd = standard_error(swc_all_avg),
    MarApr_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    MarApr_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)

grass.MarApr.swc_df <- reg_stats_2x(data = grass_phenoMet_MarApr_water_df, 
                                    x1 = "MarApr_swc_shallow",
                                    x2 = "MarApr_swc_deep",
                                    y_vars = c("UD_med", "SD_med",
                                               "DD_med", "RD_med",
                                               "GSL_med"))

grass.MarApr.swc_df <- grass.MarApr.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_MarApr_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_MarApr_swc_deep"), .)
  ))

write.csv(x = grass.MarApr.swc_df, file = "full_phenometric_stats_grass_MarApr.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH  JULY VARIABLES
########################


#GET July RAIN AND MAY AND JUNE
grass_phenoMet_july_water_df <-  main_df %>%
  select(year, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(month == 7) %>%
  filter(year != 2020) %>%
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    july_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    july_swc_deep_sd = standard_error(swc_deep_avg),
    july_precip = sum(precip, na.rm = TRUE),
    july_swc_all = mean(swc_all_avg, na.rm = TRUE),
    july_swc_all_sd = standard_error(swc_all_avg),
    july_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    july_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T)
  ) %>%
  merge(pheno_met_grass)

grass.july.swc_df <- reg_stats_2x(data = grass_phenoMet_july_water_df, 
                                    x1 = "july_swc_shallow",
                                    x2 = "july_swc_deep",
                                    y_vars = c("UD_med", "SD_med",
                                               "DD_med", "RD_med",
                                               "GSL_med"))

grass.july.swc_df <- grass.july.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_july_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_july_swc_deep"), .)
  ))

write.csv(x = grass.july.swc_df, file = "full_phenometric_stats_grass_july.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH  AUGUST VARIABLES
########################

#GET aug RAIN 
grass_phenoMet_aug_water_df <-  main_df %>%
  select(year, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, vpd_avg) %>%
  #group_by(year, month) %>%
  filter(month == 8) %>%
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    aug_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    aug_swc_deep_sd = standard_error(swc_deep_avg),
    aug_precip = sum(precip, na.rm = TRUE),
    aug_swc_all = mean(swc_all_avg, na.rm = TRUE),
    aug_swc_all_sd = standard_error(swc_all_avg),
    aug_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    aug_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    vpd_mean = mean(vpd_avg, na.rm = T)
  ) %>%
  filter(year != 2020) %>%
  merge(pheno_met_grass)

#Stats
grass.aug.swc_df <- reg_stats_2x(data = grass_phenoMet_aug_water_df, 
                                  x1 = "aug_swc_shallow",
                                  x2 = "aug_swc_deep",
                                  y_vars = c("UD_med", "SD_med",
                                             "DD_med", "RD_med",
                                             "GSL_med"))

grass.aug.swc_df <- grass.aug.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_aug_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_aug_swc_deep"), .)
  ))

write.csv(x = grass.aug.swc_df, file = "full_phenometric_stats_grass_aug.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH July and AugusT VARIABLES
########################

#GET JulAug RAIN AND July and JulAugust
grass_phenoMet_JulAug_water_df <-  main_df %>%
  select(year, DOY, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, gpp_avg) %>%
  #group_by(year, month) %>%
  filter(month == 7 | month == 8) %>%
  filter(year != 2020) %>%
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

#Stats
grass.JulAug.swc_df <- reg_stats_2x(data = grass_phenoMet_JulAug_water_df, 
                                 x1 = "JulAug_swc_shallow",
                                 x2 = "JulAug_swc_deep",
                                 y_vars = c("UD_med", "SD_med",
                                            "DD_med", "RD_med",
                                            "GSL_med"))

grass.JulAug.swc_df <- grass.JulAug.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_JulAug_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_JulAug_swc_deep"), .)
  ))

write.csv(x = grass.JulAug.swc_df, file = "full_phenometric_stats_grass_JulAug.csv",
          row.names = F)


#########################
#Phenometrics GRASS WITH Late June and Early July VARIABLES
########################



#GET APRIL AND MARCH RAIN AND MAY AND JUNE
grass_phenoMet_JunJul_water_df <-  main_df %>%
  select(year, month, DOY, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, gpp_avg) %>%
  #group_by(year, month) %>%
  #filter(month == 6) %>% #month == 3 | month == 4 |
  filter(year != 2020) %>%
  filter(DOY >= 175 & DOY < 198) %>% #DOY DOY >= 175 & DOY < 198
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    JunJul_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    JunJul_swc_deep_sd = standard_error(swc_deep_avg),
    JunJul_precip = sum(precip, na.rm = TRUE),
    JunJul_swc_all = mean(swc_all_avg, na.rm = TRUE),
    JunJul_swc_all_sd = standard_error(swc_all_avg),
    JunJul_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    JunJul_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg),
    precip_cov = COV_error(precip),
    JunJul_gpp_avg = mean(gpp_avg, na.rm = T)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)

#Stats
grass.JunJul.swc_df <- reg_stats_2x(data = grass_phenoMet_JunJul_water_df, 
                                    x1 = "JunJul_swc_shallow",
                                    x2 = "JunJul_swc_deep",
                                    y_vars = c("UD_med", "SD_med",
                                               "DD_med", "RD_med",
                                               "GSL_med"))

grass.JunJul.swc_df <- grass.JunJul.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_JunJul_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_JunJul_swc_deep"), .)
  ))

write.csv(x = grass.JunJul.swc_df, file = "full_phenometric_stats_grass_JunJul.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH JUNE TO OCTOBER VARIABLES
########################

#GET 
grass_phenoMet_JJASO_water_df <-  main_df %>%
  select(year, month, DOY, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, gpp_avg) %>%
  #group_by(year, month) %>%
  filter(month == 6 | month == 7 | month == 8 | month == 9 | month == 10) %>% #month == 3 | month == 4 |
  filter(year!= 2020) %>%
  #filter(DOY >= 175 & DOY < 198) %>% #DOY >= 179 & DOY < 198
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  summarize(
    JJASO_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    JJASO_swc_deep_sd = standard_error(swc_deep_avg),
    JJASO_precip = sum(precip, na.rm = TRUE),
    JJASO_swc_all = mean(swc_all_avg, na.rm = TRUE),
    JJASO_swc_all_sd = standard_error(swc_all_avg),
    JJASO_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    JJASO_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg),
    JJASO_gpp_sum = sum(gpp_avg, na.rm = T)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)

#Stats
grass.JJASO.swc_df <- reg_stats_2x(data = grass_phenoMet_JJASO_water_df, 
                                    x1 = "JJASO_swc_shallow",
                                    x2 = "JJASO_swc_deep",
                                    y_vars = c("UD_med", "SD_med",
                                               "DD_med", "RD_med",
                                               "GSL_med"))

grass.JJASO.swc_df <- grass.JJASO.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_JJASO_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_JJASO_swc_deep"), .)
  ))

write.csv(x = grass.JJASO.swc_df, file = "full_phenometric_stats_grass_JJASO.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH AUGUST AND SEPT VARIABLES
########################

#GET AugSept RAIN AND  AugSept
grass_phenoMet_AugSept_water_df <-  main_df %>%
  select(year, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(year != 2020) %>%
  filter(month == 8 | month == 9) %>%
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    AugSept_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    AugSept_swc_deep_sd = standard_error(swc_deep_avg),
    AugSept_precip = sum(precip, na.rm = TRUE),
    AugSept_swc_all = mean(swc_all_avg, na.rm = TRUE),
    AugSept_swc_all_sd = standard_error(swc_all_avg),
    AugSept_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    AugSept_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg),
    AugSept_swc_shallow_cov = COV_error(swc_shallow_avg)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)

#Stats
grass.AugSept.swc_df <- reg_stats_2x(data = grass_phenoMet_AugSept_water_df, 
                                   x1 = "AugSept_swc_shallow",
                                   x2 = "AugSept_swc_deep",
                                   y_vars = c("UD_med", "SD_med",
                                              "DD_med", "RD_med",
                                              "GSL_med"))

grass.AugSept.swc_df <- grass.AugSept.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_AugSept_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_AugSept_swc_deep"), .)
  ))

write.csv(x = grass.AugSept.swc_df, file = "full_phenometric_stats_grass_AugSept.csv",
          row.names = F)

#########################
#Phenometrics GRASS WITH SEPT VARIABLES
########################

#GET SEPT RAIN AND MAY AND JUNE
grass_phenoMet_sept_water_df <-  main_df %>%
  select(year, month, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg, vpd_avg) %>%
  #group_by(year, month) %>%
  filter(year != 2020) %>%
  filter(month == 9) %>%
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    sept_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    sept_swc_deep_sd = standard_error(swc_deep_avg),
    sept_precip = sum(precip, na.rm = TRUE),
    sept_swc_all = mean(swc_all_avg, na.rm = TRUE),
    sept_swc_all_sd = standard_error(swc_all_avg),
    sept_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    sept_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    vpd_mean = mean(vpd_avg, na.rm = T)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)


#Stats
grass.sept.swc_df <- reg_stats_2x(data = grass_phenoMet_sept_water_df, 
                                     x1 = "sept_swc_shallow",
                                     x2 = "sept_swc_deep",
                                     y_vars = c("UD_med", "SD_med",
                                                "DD_med", "RD_med",
                                                "GSL_med"))

grass.sept.swc_df <- grass.sept.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_sept_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_sept_swc_deep"), .)
  ))

write.csv(x = grass.sept.swc_df, file = "full_phenometric_stats_grass_sept.csv",
          row.names = F)


#########################
#Phenometrics GRASS WITH OCT VARIABLES
########################

#GET OCT
grass_phenoMet_oct_water_df <-  main_df %>%
  select(year, month, swc_130_avg, precip, DOY,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(year != 2020) %>%
  filter(month == 10) %>%
  #filter(DOY <= 285 ) %>% #to only go up to water before average senescence date
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    oct_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    oct_swc_deep_sd = standard_error(swc_deep_avg),
    oct_precip = sum(precip, na.rm = TRUE),
    oct_swc_all = mean(swc_all_avg, na.rm = TRUE),
    oct_swc_all_sd = standard_error(swc_all_avg),
    oct_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    oct_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_met_grass)


#Stats
grass.oct.swc_df <- reg_stats_2x(data = grass_phenoMet_oct_water_df, 
                                  x1 = "oct_swc_shallow",
                                  x2 = "oct_swc_deep",
                                  y_vars = c("DD_med", "RD_med", #october too late for UD and SD
                                             "GSL_med"))

grass.oct.swc_df <- grass.oct.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_oct_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_oct_swc_deep"), .)
  ))

write.csv(x = grass.oct.swc_df, file = "full_phenometric_stats_grass_oct.csv",
          row.names = F)
