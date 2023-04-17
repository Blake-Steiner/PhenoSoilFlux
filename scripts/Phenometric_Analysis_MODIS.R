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
library("egg")
#library("RColorBrewer")


# MAKE SURE TO LOAD R IMAGE

setwd("F:/ / /")


##########################
#-------Functions
##########################
#Rescale gpp values

# create rescale function
rescale_gpp <- function(x){
  (x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))
}

standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x)) #Standard Error, Barnes et al., 2016 for Max EVI across all years


COV_error <- function(x) sd(x, na.rm = T) / mean(x, na.rm = T)

# Create function to correlate through different x and y variables

data = modis_phenoMet_spring_water_df
x1 = "spring_swc_shallow"
x2 = "spring_swc_deep"
y_vars = c("UD", "SD","DD", "RD", "GSL")
rm(data, x1, x2, y_vars, models_x1)
y = "UD"

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
    cor(x = data[[x1]], y = data[[y]], use = "pairwise.complete.obs")
  })
  
  cor_x2 <- lapply(y_vars, function(y) {
    cor(x = data[[x2]], y = data[[y]], use = "pairwise.complete.obs")
  })

  #cor_x1 <- lapply(models_x1, function(model) {
  #  y <- data[[model$terms[[2]]]]
  #  fitted <- model$fitted.values
  #  na_mask <- !is.na(y) & !is.na(fitted)
  #  if (sum(na_mask) > 1) {
  #    cor(y[na_mask], fitted[na_mask])
  #  } else {
  #    NA
  #  }
  #})
  
  
  #cor_x2 <- lapply(models_x2, function(model) {
  #  y <- data[[model$terms[[2]]]]
  #  fitted <- model$fitted.values
  #  na_mask <- !is.na(y) & !is.na(fitted)
  #  if (sum(na_mask) > 1) {
  #    cor(y[na_mask], fitted[na_mask])
  #  } else {
  #    NA
  #  }
  #})
  
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


##########################
#-------Plot themes 
##########################

plot.theme <- theme(axis.title.x = element_text(color = "black", 
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
#Phenometrics 
########################

#################################
#READ IN CSVs
#################################
main_df <- read.csv(file = "main_data.csv", stringsAsFactors = FALSE)

modis_met <- read.csv(file = "modis_annual_phenometrics.csv",
                      stringsAsFactors = FALSE)


pheno_modis_comb <- modis_met 

rm(modis_met)

#########################
#Phenometrics MODIS WITH SPRING VARIABLES  USING ANNUAL MODIS METRICS
########################

#GET APRIL AND MARCH RAIN AND MAY AND JUNE
modis_phenoMet_spring_water_df <-  main_df %>% #flux_df was old one, use if main_df not work
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  select(year, sub_season, month, DOY, swc_130_avg, precip, 
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(sub_season == "spring") %>%
  #filter(month == 3 | month == 4 | month == 5 | month == 6) %>% #month == 3 | month == 4 |
  #filter(DOY >= 175 & DOY < 198) %>% #DOY >= 175 & DOY < 198
  group_by(year) %>%
  #filter(sub_season == "spring") %>%
  dplyr::summarize(
    spring_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    spring_swc_deep_sd = standard_error(swc_deep_avg),
    spring_precip = sum(precip, na.rm = TRUE),
    spring_swc_all = mean(swc_all_avg, na.rm = TRUE),
    spring_swc_all_sd = standard_error(swc_all_avg),
    spring_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    spring_swc_shallow_sd = standard_error(swc_shallow_avg),
    precip_events = sum(precip > 5),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_modis_comb) %>%
  mutate(GSL = RD - UD)

#Calculate stats

modis.spring.swc_df <- reg_stats_2x(data = modis_phenoMet_spring_water_df, 
                                    x1 = "spring_swc_shallow",
                                    x2 = "spring_swc_deep",
                                    y_vars = c("UD", "SD",
                                               "DD", "RD",
                                               "GSL"))

modis.spring.swc_df <- modis.spring.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_spring_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_spring_swc_deep"), .)
  ))

write.csv(x = modis.spring.swc_df, file = "full_phenometric_stats_modis_spring.csv",
          row.names = F)

test.lm <- lm(modis_phenoMet_spring_water_df$RD ~ modis_phenoMet_spring_water_df$spring_swc_deep)
summary(test.lm)
#########################
#Phenometrics MODIS FOR July and August  USING ANNUAL MODIS METRICS
########################

#GET APRIL AND MARCH RAIN AND MAY AND JUNE
modis_phenoMet_JulAug_water_df <- main_df %>% #flux_df was old one, use if main_df not work
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  select(year, sub_season, month, DOY, swc_130_avg, precip, 
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(month == 7 | month == 8) %>% #month == 3 | month == 4 |
  #filter(DOY >= 175 & DOY < 198) %>% #DOY >= 175 & DOY < 198
  group_by(year) %>%
  #filter(sub_season == "JulAug") %>%
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
    T_max = max(Tair_avg, na.rm = T),
    T_mean = mean(Tair_avg, na.rm = T),
    T_cov = COV_error(Tair_avg)
  ) %>%
  #filter(year != 2017) %>%
  merge(pheno_modis_comb) %>%
  mutate(GSL = RD - UD)

#Calculate stats

modis.JulAug.swc_df <- reg_stats_2x(data = modis_phenoMet_JulAug_water_df, 
                                    x1 = "JulAug_swc_shallow",
                                    x2 = "JulAug_swc_deep",
                                    y_vars = c("UD", "SD",
                                               "DD", "RD",
                                               "GSL"))

modis.JulAug.swc_df <- modis.JulAug.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_JulAug_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_JulAug_swc_deep"), .)
  ))

write.csv(x = modis.JulAug.swc_df, file = "full_phenometric_stats_modis_JulAug.csv",
          row.names = F)

#########################
#Phenometrics MODIS SUMMER   ## NOTE: NOT ALL SUMMER FOR UD SUMMER AND WITHOUT 2019 AND 2020
########################
#GET SUMMER RAIN

modis_phenoMet_summer_water_df <-  main_df %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  select(year, sub_season, month, swc_130_avg, precip, DOY,
         swc_all_avg, swc_shallow_avg, gpp_avg,
         swc_deep_avg, ppfd_in_avg, Tair_avg, Tsoi_2.5_10_avg) %>%
  filter(sub_season == "summer") %>%
  group_by(year) %>%
  dplyr::summarize(
    summer_precip = sum(precip, na.rm = TRUE),
    summer_swc_all = mean(swc_all_avg, na.rm = TRUE),
    summer_swc_deep = mean(swc_deep_avg, na.rm = TRUE),
    summer_swc_shallow = mean(swc_shallow_avg, na.rm = TRUE),
    summer_tair_max = max(Tair_avg, na.rm = TRUE),
    ppfd_avg = mean(ppfd_in_avg, na.rm = T),
    precip_events = sum(precip > 5),
    Tsoi_avg = mean(Tsoi_2.5_10_avg, na.rm = T),
    gpp_sum = sum(gpp_avg, na.rm = T)
  ) %>%
  merge(pheno_modis_comb) %>%
  mutate(GSL = RD - UD)


#Calculate stats
# DD, RD, and GSL
modis.summer.swc_df <- reg_stats_2x(data = modis_phenoMet_summer_water_df, 
                                    x1 = "summer_swc_shallow",
                                    x2 = "summer_swc_deep",
                                    y_vars = c("DD", "RD",
                                               "GSL"))

modis.summer.swc_df <- modis.summer.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_summer_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_summer_swc_deep"), .)
  ))

write.csv(x = modis.summer.swc_df, file = "full_phenometric_stats_modis_summer.csv",
          row.names = F)



# 2019 and 2020 should be removed for UD and SD for summer water 
modis_UD_SD_summer_df <- modis_phenoMet_summer_water_df %>%
  filter(year != 2019 & year != 2020)


modis.UD_SD_summer.swc_df <- reg_stats_2x(data = modis_UD_SD_summer_df, 
                                    x1 = "summer_swc_shallow",
                                    x2 = "summer_swc_deep",
                                    y_vars = c("UD", "SD"))

modis.UD_SD_summer.swc_df <- modis.UD_SD_summer.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_summer_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_summer_swc_deep"), .)
  ))

write.csv(x = modis.UD_SD_summer.swc_df, file = "UD_SD_phenometric_stats_modis_summer.csv",
          row.names = F)

#########################
#Phenometrics MODIS WITH Winter VARIABLES
########################

#GET WINTER RAIN
modis_phenoMet_winter_water_df <-  main_df %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  select(water_year, sub_season, month, DOY, swc_130_avg, precip,
         swc_all_avg, swc_shallow_avg, 
         swc_deep_avg, ppfd_in_avg, Tair_avg) %>%
  #group_by(year, month) %>%
  filter(sub_season == "winter") %>%
  filter(water_year != 2021) %>% # using water year so Nov and Dec water from previous calendar year is used into new year
  group_by(water_year) %>%
  dplyr::summarize(
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
  #filter(year != 2017) %>%
  cbind(., pheno_modis_comb) %>%
  #merge(., pheno_modis_comb) %>%
  mutate(GSL = RD - UD)

#Calculate stats

modis.winter.swc_df <- reg_stats_2x(data = modis_phenoMet_winter_water_df, 
                                    x1 = "winter_swc_shallow",
                                    x2 = "winter_swc_deep",
                                    y_vars = c("UD", "SD",
                                               "DD", "RD",
                                               "GSL"))

modis.winter.swc_df <- modis.winter.swc_df %>%
  rename_all(~ ifelse(grepl("_x1", .), paste0(., "_winter_swc_shallow"),
                      ifelse(grepl("_x2", .), paste0(., "_winter_swc_deep"), .)
  ))

write.csv(x = modis.winter.swc_df, file = "full_phenometric_stats_modis_winter.csv",
          row.names = F)

