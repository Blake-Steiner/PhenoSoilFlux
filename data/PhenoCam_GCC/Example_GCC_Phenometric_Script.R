#---------GCC Analysis--------#
#load package
install.packages("phenopix")
library("phenopix")
#library("data.table")

setwd("E:/2019_Data_MS/MS_Phenology/GCC_Data/Phenocam_2020/Phenocam_Analysis/")
#------------------Draw ROIs---------------

NROI <- 9
ROI_Name <- c('Mes1',"Mes2","Mes3","Mes4", "Mes5",
              'Gra1', 'Gra2', 'Gra3', 'Gra4')

DrawROI(path_img_ref = "REF/srm_2018-08-15-121905.jpg", 
        path_ROIs = "ROI/",
        nroi = NROI, 
        roi.names = ROI_Name,
        file.type = '.JPG')

load('ROI/roi.data.Rdata')
str(roi.data)

#Used to double check out your drawn ROIs based on their names
PrintROI(path_img_ref = 'REF/srm_2018-08-15-121905.jpg',
         path_ROIs = 'ROI/', 
           which = 'all', col = palette())

                  
#---------------Extract vegetation indices------
VI.data <- extractVIs(img.path = "IMG/",
           roi.path = "ROI/",
           vi.path = "VI/", 
           plot = T,
          date.code = 'yyyy_mm_dd_HHMMSS', file.type = '.jpg',
          ncores = 4)


load('VI/VI.data.Rdata')

#--------Filter the data------

#filter with defaults for Mesquite ROIs
autofil.mes1 <- autoFilter(VI.data$Mes1)
autofil.mes2 <- autoFilter(VI.data$Mes2)
autofil.mes3 <- autoFilter(VI.data$Mes3)
autofil.mes4 <- autoFilter(VI.data$Mes4)

#filter with defaults for Grass ROIs
autofil.gra1 <- autoFilter(VI.data$Gra1)
autofil.gra2 <- autoFilter(VI.data$Gra2)
autofil.gra3 <- autoFilter(VI.data$Gra3)
autofil.gra4 <- autoFilter(VI.data$Gra4)


#filter with defaults for Mesquite ROIs

# IF YOU WANT DAILY SUMMARIES 

#autofil.mes <- autoFilter(VI.data$Mes)
#autofil.gra <- autoFilter(VI.data$Gra)

##########################
# FOR SUB-DAILY
##########################
#create function to calculate GCC
calc_gcc <- function(g, r, b){
       #Calculate GCC from raw values
        gcc = g / (g + r + b)
        
        #g = g.av from VI.data
        #r = r.av from VI.data
        #b = b.av from VI.data
return(gcc)        
}


gcc_mes <- calc_gcc(g = VI.data$Mes$g.av,
                    r = VI.data$Mes$r.av,
                    b = VI.data$Mes$b.av)

doy <- VI.data$Mes$date

#create dub daily data frame
sub_daily_df <- data.frame(gcc_mes, doy_mes)

#double check
plot(x = doy, gcc_mes)

#re run for grass data
gcc_gra <- calc_gcc(g = VI.data$Gra$g.av,
                    r = VI.data$Gra$r.av,
                    b = VI.data$Gra$b.av)

#create dub daily data frame
sub_daily_df <- data.frame(gcc_mes, doy,
                           gcc_gra)

#double check
plot(x = doy, gcc_gra)

#Save as CSV
setwd("E:/2019_Data_MS/Final_Processed_Datasets")

write.csv(sub_daily_df,
          file = "GCC_SRM_2019.csv",
          row.names = FALSE)

#clean up
rm(sub_daily_df, doy, gcc_gra, gcc_mes)
##########################

##########################

#create dataframes from filtered data
#Mesquite
#df_mes <- convert(autofil.mes)

#Grass
#df_gra <- convert(autofil.mes)

########### For 2020############
df_mes1 <- convert(autofil.mes1)
df_mes2 <- convert(autofil.mes2)
df_mes3 <- convert(autofil.mes3)
df_mes4 <- convert(autofil.mes4)

#joining the dataframes by rows, then calculating the daily means
df_mesq <- rbind(df_mes1, df_mes2, df_mes3, df_mes4)

#cleanup
rm(df_mes1, df_mes2, df_mes3, df_mes4)

#Grass
df_gra1 <- convert(autofil.gra1)
df_gra2 <- convert(autofil.gra2)
df_gra3 <- convert(autofil.gra3)
df_gra4 <- convert(autofil.gra4)

#joining the dataframes by rows, then calculating the daily means
df_gras <- rbind(df_gra1, df_gra2, df_gra3, df_gra4)

###################################
########### Daily Summaries for 2020 ############
#Get mean and sd of mesquite
library("dplyr")

summary_mesq <- df_mesq %>%
        group_by(doy) %>%
        summarise_all(list(mean = mean, sd = sd))

summary_gra <- df_gras %>%
        group_by(doy) %>%
        summarise_all(list(mean = mean, sd = sd))


###################################
########### CALCULATE GCC FROM VI.DATA ###########
###################################

#GRass
gra_df <- base::data.frame(VI.data$Gra)


gra_gcc <- gra_df$g.av / (gra_df$g.av +
                             gra_df$r.av +
                             gra_df$b.av)

gra_df <- base::cbind(gra_df, gra_gcc)


#Mesquite
mes_df <- base::data.frame(VI.data$Mes)


mes_gcc <- mes_df$g.av / (mes_df$g.av +
                                  mes_df$r.av +
                                  mes_df$b.av)

mes_df <- base::cbind(mes_df, mes_gcc)

#cleanup
rm(mes_gcc, gra_gcc)

###################################
# Write CSVs
###################################

write.csv(summary_mesq, file = "mesq_GCC_2020_rerun.csv", row.names = F)
write.csv(summary_gra, file = "grass_GCC_2020_rerun.csv", row.names = F)

#------------PLot all the things---------------
library("tidyverse")
#GCC
gcc <- ggplot() +
        geom_point(data = summary_mesq, aes(x = doy, y = gcc_mean), color = "darkgreen") +
        geom_errorbar(data = summary_mesq, aes(x = doy,
                                               ymin = gcc_mean - gcc_sd,
                                               ymax = gcc_mean + gcc_sd), color = "darkgreen") + 
        geom_point(data = summary_gra, aes(x = doy, y = gcc_mean), color = "orange") +
        geom_errorbar(data = summary_gra, aes(x = doy,
                                              ymin = gcc_mean - gcc_sd,
                                              ymax = gcc_mean + gcc_sd), color = "orange") +
        ggtitle("GCC of Mesquite and Grass at US-SRM 2020") +
        theme(axis.title.x = element_text(color = "black", 
                                          size = 15), 
              axis.title.y = element_text(color = "black", 
                                          size = 15),                                                                       
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              legend.position = c(1,0),
              legend.justification = c(1,0),
              plot.title = element_text(color = "black",
                                        size = 20, hjust = 0.5),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        scale_color_manual(values = c("orange", "darkgreen")) +
        scale_shape_manual(values = c(19,15)) +
        scale_fill_discrete(labels = c("Grass", "Trees")) +
        xlab("Date") +
        ylab("GCC")

#max filtered
max.filtered.plot <- ggplot() +
        geom_point(data = summary_mesq, aes(x = doy, y = max.filtered_mean), color = "darkgreen") +
        geom_errorbar(data = summary_mesq, aes(x = doy,
                                               ymin = max.filtered_mean - max.filtered_sd,
                                               ymax = max.filtered_mean + max.filtered_sd), color = "darkgreen") + 
        geom_point(data = summary_gra, aes(x = doy, y = max.filtered_mean), color = "orange") +
        geom_errorbar(data = summary_gra, aes(x = doy,
                                              ymin = max.filtered_mean - max.filtered_sd,
                                              ymax = max.filtered_mean + max.filtered_sd), color = "orange") +
        ggtitle("Max-Filtered GCC of Mesquite and Grass at US-SRM 2020") +
        theme(axis.title.x = element_text(color = "black", 
                                          size = 15), 
              axis.title.y = element_text(color = "black", 
                                          size = 15),                                                                       
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              legend.title = element_blank(),
              legend.text = element_text(size = 10),
              legend.position = c(1,0),
              legend.justification = c(1,0),
              plot.title = element_text(color = "black",
                                        size = 20, hjust = 0.5),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        scale_color_manual(values = c("orange", "darkgreen")) +
        scale_shape_manual(values = c(19,15)) +
        scale_fill_discrete(labels = c("Grass", "Trees")) +
        xlab("Date") +
        ylab("GCC")




########### PHENOMETRICS ############
#List zoo objects together
zoo.list <- list(autofil.mes1, autofil.mes2, autofil.mes3, autofil.mes4,
                 autofil.gra1, autofil.gra2,autofil.gra3, autofil.gra4
)

#Function to filter zoo objects
zoo_filt <- function(zoo_object, variable){
        
        #input:
        # a zoo object
        # a variable in a character string from one of the zoo object's columns
        
        #output:
        zoodata = zoo_object[, colnames(zoo_object) == c(variable)]
        
        return(zoodata)
}


#Names for zoo grass
names(zoo.list) <- c("mes1", "mes2", "mes3", "mes4",
                     "gra1", "gra2", "gra3", "gra4")

#Remove unneeded columns

#test
zoo_filt(autofil.gra1, 'max.filtered')

#apply

zoo.list.filt <- lapply(zoo.list, zoo_filt, 
                        'max.filtered')


#Loop around years in annual for variables

fit_2020_annual <- lapply(seq_along(zoo.list.filt), 
                          function(x) greenProcess(zoo.list.filt[[x]],
                                                   fit = 'spline', # 
                                                   threshold =  'gu', # Gu et al. (2009) UD = upturn, SD = stable date, DD = downturn date, RD = recession date
                                                   plot = TRUE,
                                                   uncert = FALSE, #uncertainty calculation
                                                   nrep = 500, #number of replications
                                                   envelope = 'quantiles', #One between quantiles and min-max
                                                   hydro = FALSE, #determines DOY start, being Jan 1st or Oct 1st
                                                   sf=quantile(zoo.list.filt[[x]], 
                                                               na.rm=TRUE, prob=c(0.05, 0.95)),
                                                   ncores = 4 #number of cores for processing
                          )
) 



#Change names of list
names(fit_2020_annual) <- c("mes1", "mes2", "mes3", "mes4",
                            "gra1", "gra2", "gra3", "gra4")

#Set up plot positions

par(mfrow = c(4, # 4 rows
              2 # 2 columns
))
#actual plot
lapply(seq_along(fit_2020_annual), function(x) plot(fit_2020_annual[[x]],
                                                    type = 'p', pch = 20, col = 'grey', 
                                                    main = names(fit_2020_annual[x]), 
                                                    #names(fit_gpp_annual[x]),
                                                    xlab = 'DOYs', ylab = 'GCC'
                                                    #, ylim = c(0.28, 0.43)
)
) 

#main title for the whole plot
mtext("Spline Fit and Gu Thresholds For 2020 Trees and Grass", 
      outer = TRUE, cex = 1.5,
      line = -1.7)

print(fit_2020_annual)


#Extract dates for CSV
list_2020_annual <- lapply(seq_along(fit_2020_annual), function(x) rbind(
        fit_2020_annual[[x]][["metrics"]]
))

ROI_annual_phenometrics_2020 <- do.call("rbind", list_2020_annual)

#Create ROI column
roi <- c("mes1", "mes2", "mes3", "mes4",
         "gra1", "gra2", "gra3", "gra4")

ROI_annual_phenometrics_2020 <- cbind(ROI_annual_phenometrics_2020, roi)

#Create plant type column
type <- c("mes", "mes", "mes", "mes",
          "gra", "gra", "gra", "gra")

ROI_annual_phenometrics_2020 <- cbind(ROI_annual_phenometrics_2020, type)

#Save data
setwd("E:/2019_Data_MS/Final_Processed_Datasets/Phenometrics/")

write.csv(ROI_annual_phenometrics_2020,
          file = "ROI_annual_phenometrics_2020_rerun.csv",
          row.names = FALSE)

