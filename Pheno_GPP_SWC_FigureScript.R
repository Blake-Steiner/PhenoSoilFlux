###############################

# Name: Blake Steiner
# Date Updated: April 11th, 2023
# Objective:
#   Script to plot from an R image the figures for the manuscript and supplemental 
#     NOTE: for Table S1, those correlations can be tested with the same image but
#           using scripts: "Phenometric_Analysis_MODIS" and
#           "Phenometrics_Analysis_Short"

##############################

####################
# PACKAGES AND YOU NEED THE R IMAGE
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
                                                size = 11), 
                    axis.title.y = element_text(color = "black", 
                                                size = 11,
                                                hjust = 0.6,
                                                vjust = 1),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    legend.position = "top",
                    plot.title = element_text(color = "black",
                                              size = 12, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Theme for models
plot.theme.model <- theme(axis.title.x = element_text(color = "black", 
                                                size = 12), 
                    axis.title.y = element_text(color = "black", 
                                                size = 12),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    legend.position = c(1,1),
                    legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 14, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))


#theme for phenometrics and GPP
plot.theme_phenoMet_gpp <- theme(axis.title.x = element_text(color = "black", 
                                                size = 12), 
                    axis.title.y = element_text(color = "black", 
                                                size = 12),                                                                       
                    axis.text.x = element_text(size = 10),
                    axis.text.y = element_text(size = 10),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 12, hjust = 0.5),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 10, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 10, color = "black", face = "bold"),
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
                                                size = 12), 
                    axis.title.y = element_text(color = "black", 
                                                size = 12),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 12),
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
                                                size = 12), 
                    axis.title.y = element_text(color = "black", 
                                                size = 12),                                                                       
                    axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    #legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    #legend.position = c(1,1),
                    #legend.justification = c(1,1),
                    plot.title = element_text(color = "black",
                                              size = 114, hjust = 0.5, face = "bold"),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    strip.text.x = element_text(
                      size = 10, color = "black", face = "bold"),
                    strip.text.y = element_text(
                      size = 10, color = "black", face = "bold"),
                    strip.background = element_rect(
                      color="black", fill="white", linetype="solid"
                    )
)



###############
# Figure S1 
###############

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


#clean up
rm(precip.seas.plot, swc.seas.deep.plot, swc.seas.shallow.plot,
   temp.seas.plot)


##################################
# Figure S2
#################################

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
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  xlab("Year") +
  ylim(0, 1.0) +
  ylab("GCC (rescaled)")

#clean up
rm(max_gcc, min_gcc, min_gcc_grass,
   max_gcc_grass, runner, years,
   gcc_names)

################
#-- Figure 1 CLIMATOLOGY PLOTS
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
             size = 2, alpha = 0.7) +
  xlab("") +
  ylab("GCC MODIS") +
  plot.theme.cli +
  theme(legend.position = "none") +
  guides(color = "none")

#TREE and GRASS CLIMATOLOGY
#plant_gcc_cli.plot <- cli_df %>%
#  ggplot() +
#  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
#  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
#  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) + #TREE PHENOCAM 
#  geom_vline(xintercept = 82, color = "purple", size = 1.5, 
#             linetype = "dashed") +  #MODIS mean
#  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
#  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
#  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
#  geom_vline(xintercept = 194, color = "purple", size = 1.5, 
#             linetype = "dashed") +
#  geom_ribbon(alpha = 0.3, aes(x = DOY, y = tree_avg, 
#                               ymin = tree_min, ymax = tree_max), 
#              fill = "#999999") +
#  geom_ribbon(alpha = 0.6, aes(x = DOY, y = grass_avg, 
#                                  ymin = grass_min, ymax = grass_max), 
#                 fill = "#99CCCC") +
#  geom_point(aes(x = DOY, y = tree_avg, color = sub_season), 
#             size = 2, alpha = 1, shape = 24, fill = NA) +
#  geom_point(aes(x = DOY, y = grass_avg, color = sub_season),  
#             size = 2, alpha = 0.8, shape = "circle") +
#  xlab("") +
#  ylab("Relative GCC") +
#  plot.theme.cli +
#  scale_shape_manual(name = "Plant Type", values = c(24, "circle")) +
#  labs(color = "Sub-season", shape = "Plant Type") +
#  theme(legend.position = "bottom", legend.box = "vertical",
#       legend.margin = margin())


plant_gcc_cli.plot <- cli_df %>%
  ggplot() +
  geom_vline(xintercept = 81, color = "darkgreen", size = 1.5) + # TREE PHENOCAM 
  geom_vline(xintercept = 66, color = "darkgreen", size = 1.5) + # TREE PHENOCAM 
  geom_vline(xintercept = 96, color = "darkgreen", size = 1.5) + # TREE PHENOCAM 
  geom_vline(xintercept = 82, color = "purple", size = 1.5, linetype = "dashed") +  # MODIS mean
  geom_vline(xintercept = 187, color = "gold", size = 1.5) +
  geom_vline(xintercept = 176, color = "gold", size = 1.5) +
  geom_vline(xintercept = 199, color = "gold", size = 1.5) +
  geom_vline(xintercept = 194, color = "purple", size = 1.5, linetype = "dashed") +
  geom_ribbon(alpha = 0.3, aes(x = DOY, y = tree_avg, ymin = tree_min, ymax = tree_max), fill = "#999999") +
  geom_ribbon(alpha = 0.6, aes(x = DOY, y = grass_avg, ymin = grass_min, ymax = grass_max), fill = "#99CCCC") +
  geom_point(aes(x = DOY, y = tree_avg, color = sub_season, shape = "Tree"), size = 2, alpha = 1, fill = NA) +
  geom_point(aes(x = DOY, y = grass_avg, color = sub_season, shape = "Grass"), 
             size = 2, alpha = 0.8) +
  xlab("") +
  ylab("Relative GCC") +
  plot.theme.cli +
  scale_shape_manual(name = "Plant Type", values = c("Tree" = 24, "Grass" = 1)) +
  scale_color_manual(labels = c("Spring", "Summer", "Winter"), 
                     values = c("spring"="#F8766D", "summer"="#00BA38", "winter"="#619CFF")) +
  labs(shape = "Plant Type") +
  theme(legend.margin = margin(),
        legend.title = element_text(face = "bold")) +
  guides(color = guide_legend(title = "Sub-Season"))


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
  geom_point(aes(color = sub_season), size = 2, alpha = 0.7) +
  xlab("") +
  ylab("SWC (%) from 2.5 - 30 cm ") +
  plot.theme.cli +
  theme(legend.position = "none") +
  guides(color = "none")

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
  geom_point(aes(color = sub_season), size = 2, alpha = 0.7) +
  ylab("SWC (%) from 45 - 130 cm") +
  plot.theme.cli +
  theme(legend.position = "none") +
  guides(color = "none")

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
  geom_point(aes(color = sub_season), size = 2, alpha = 0.7) +
  xlab("DOY") +
  labs(y = expression(Tower~GPP~(g~CO[2]~m^-2~d^-1))) +
  plot.theme.cli +
  theme(legend.position = "none") +
  guides(color = "none")


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
             size = 2, alpha = 0.7) +
  xlab("") +
  ylab("Precipitation (mm)") +
  plot.theme.cli +
  theme(legend.position = "none") +
  guides(color = "none")


#Create PDF
ggsave(path = getwd(), filename =  "Figure1_Climatology.pdf", width = 18 / 2.54,
       height = 22/ 2.54, device = "pdf", dpi = 600)

#GROUP THE PLOTS TOGETHER
ggpubr::ggarrange(modis_gcc_cli.plot,
                  precip_cli.plot, 
                  plant_gcc_cli.plot,
                  swc_cli_shallow.plot,
                  gpp_cli.plot,
                  swc_cli_deep.plot,
                  align = "hv", #align in both directions / axes
                  ncol=2, nrow=3, common.legend = TRUE, #legend="top", 
                  labels =  c("A", "D", "B", "E", "C", "F"),   #"AUTO",
                  hjust = -0.7,
                  vjust = 1.0)



dev.off()



#############
#-- Table 1 MODEL COMPARISONS FOR RAIN, SWC, DEPTH, AND SUM GPP
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


##################
#-- Figure S6 Supplementary Plot for spring swc and winter rainfall
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





#########################
#-- Figure 2 GPP Phenometrics vs MODIS Phenometrics
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
  annotate(geom = "text", x = 250, y = 500, size = 3,
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
#Create PDF
ggsave(path = getwd(), filename = "Figure2_GPP_Phenometric.pdf",
       width = 18 / 2.54, height = 10 / 2.54, 
       dpi = 600)


dev.off()

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



####################
#-- Figure 3 TIME LAGS BETWEEN SWC AND PHENOCAM/MODIS GCC
###################

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
            color = "black", angle = 0, size = 7,
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
tag_facet(time_lag.plot , size = 4, tag_pool = capletters,
          open = " ", close = " ", vjust = 0.95, hjust = -0.1) 


#Create PDF
ggsave(path = getwd(), filename = "Figure3_TimeLag.pdf",
       width = 18 / 2.54, height = 22 / 2.54, 
       dpi = 600)

dev.off()

rm(time_lag.plot)



######################################
#-- Figure S5 BOX PLOT with each ROI phenometrics 
######################################

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



##########
#-- Table 2 but visualized as a bar chart 
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
#-- Figure 4 Phenometric of RD versus SWC between Trees and Understory
###################

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
  xlab("SWC (%)") +
  xlim(3, 9) +
  ylab("Recession Date (DOY)") +
  facet_wrap(~time_period
             ,labeller = labeller(time_period = depth.labs)
             ) +
  theme(legend.position = "top") +
  scale_color_manual(name = "Medium", 
                     values = c("#666666", "blue", "red"),
                                labels = c("MODIS", "Tree", "Understory")) +
  scale_shape_manual(name = "Medium", labels = c("MODIS", "Tree", "Understory"),
                     values = c(15, 19, 17)) +
  guides(shape = guide_legend(override.aes = list(color = c("#666666", "blue", "red")))) +
  plot.theme.phenometric 


#Create PDF
ggsave(path = getwd(), filename = "Figure4_SWC_PhenoMetric.pdf",
       width = 18 / 2.54, height = 12 / 2.54, 
       dpi = 600)

dev.off()

