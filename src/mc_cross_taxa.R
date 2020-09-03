#check to make sure required packages are installed
list.of.packages <- c("ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

library(ggplot2)

#tom epa laptop home
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  ams_dir <- "c:/git/amphibian_model_selection/"
}

#subdirectories
ams_dir_input <- paste(ams_dir, "data_in/", sep = "")
ams_dir_output <- paste(ams_dir, "data_out/", sep = "")
ams_dir_graphics <- paste(ams_dir, "graphics/", sep = "")

#import pesticide data from comptox, test and opera predictors
#1835 in total
#cpcat term pesticides
#comptox batch search
#dropped any pesticides missing values for saved columns
comptox_data_filename <- "CompTox_Pesticide_search_subset.csv"
comptox_data <- read.csv(file = paste(ams_dir_input, comptox_data_filename, sep = ""), header = TRUE)
dim(comptox_data)
colnames(comptox_data)

logKow <- comptox_data$OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED
MW <- comptox_data$AVERAGE_MASS

hist(logKow)
min(logKow)
max(logKow)

comptox_data_tall <- rbind(comptox_data,comptox_data,comptox_data,comptox_data,comptox_data,
      comptox_data,comptox_data,comptox_data,comptox_data,comptox_data)
dim(comptox_data_tall)

logKow_tall <- comptox_data_tall$OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED
MW_tall <- comptox_data_tall$AVERAGE_MASS

nsims = 18350

# body weights
bw_amphib <- runif(nsims, min=5, max=400)
bw_reptile <- runif(nsims, min=50, max=3500)
bw_bird <- runif(nsims, min=5, max=400)
bw_mammal <- runif(nsims, min=6, max=1200)
bw_generic <- runif(nsims, min=1, max=5000)
bw_all <- as.data.frame(cbind(bw_amphib,bw_reptile,bw_bird,bw_mammal,bw_generic))
dim(bw_all)
#View(bw_all)
#visual qa check
ggplot(data=bw_all, aes(x=bw_generic, y=bw_amphib))

# field metabolic rates
fmr_amphib_nagy <-  2.25 * bw_amphib^0.808 # generic
fmr_reptile_nagy <-  0.196 * bw_reptile^0.889 # generic
fmr_bird_nagy <-  10.5 * bw_bird^0.681 # generic
fmr_mammal_nagy <-  4.82 * bw_mammal^0.734 # generic
fmr_generic_nagy <- 2.25 * bw_generic^0.808
fmr_all <- cbind(bw_all, fmr_amphib_nagy,fmr_reptile_nagy,fmr_bird_nagy,fmr_mammal_nagy,fmr_generic_nagy)
dim(fmr_all)
#visual qa check
ggplot(data=fmr_all, aes(x=bw_generic, y=fmr_generic_nagy))+
  geom_line()

# body weights
dt_amphib <- runif(nsims, min=0.001, max=0.003)
dt_reptile <- runif(nsims, min=0.025, max=0.075)
dt_bird <- runif(nsims, min=0.025, max=0.075)
dt_mammal <- runif(nsims, min=0.1, max=0.3)


#kp calculation from Walker 2003
#log kp (cm/hr) = -2.72 + 0.71 log Kow - 0.0061 MW
#table 2 formamide
# 10^(-2.72 + (0.71 * -1.51) - (0.0061 * 45.04))
kp <- 10^(-2.72 + (0.71 * logKow_tall) - (0.0061 * MW_tall))

#surface area (exposed)
#dsa_amphib <- 0.4 * 1.131 * bw_amphib^0.579 #frogs
dsa_amphib <- 0.4 * 8.42 * bw_amphib^0.694 #salamanders
dsa_reptile <- 0.3 * 8.42 * bw_reptile^0.694
dsa_bird <- 0.07 * 10 *  bw_bird^0.667 #wildlife exposure factors handbook
dsa_mammal <- 0.22 * 12.3 * bw_mammal^0.65 # generic from Hope 1995

#other variables
dat = 12 #dermal averaging time 12 hours/day
csoil = 11.2 #soil concentration from 1 lb/acre application (112 mg/m2), mixed evenly 1 cm depth, contact top mm
bioavail = 0.1 #organics
cinsect = 45 #kenaganomogram, trex, 1 lb/acre appplication rate
kcal_insect = 1.7 #kcal/g

#dermal
dermal_dose_amphib <- (csoil * kp * dat * (dsa_amphib/dt_amphib) * bioavail)/bw_amphib
dermal_dose_reptile <- (csoil * kp * dat * (dsa_reptile/dt_reptile) * bioavail)/bw_reptile
dermal_dose_bird <- (csoil * kp * dat * (dsa_bird/dt_bird) * bioavail)/bw_bird
dermal_dose_mammal <- (csoil * kp * dat * (dsa_mammal/dt_mammal) * bioavail)/bw_mammal

#diet
diet_dose_amphib <- fmr_amphib_nagy /(bw_amphib * cinsect)/kcal_insect
diet_dose_reptile <- fmr_reptile_nagy /(bw_reptile * cinsect)/kcal_insect
diet_dose_bird <- fmr_bird_nagy /(bw_bird * cinsect)/kcal_insect
diet_dose_mammal <- fmr_mammal_nagy /(bw_mammal * cinsect)/kcal_insect

# inhalation pass
# https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/stir-version-10-users-guide-pesticide-inhalation
#also Hope 1995

low_logKow_tall <- which(logKow_tall<1)
med_logKow_tall <- which(logKow_tall<4&logKow_tall>1)
high_logKow_tall <- which(logKow_tall>4)
head(logKow_tall[low_logKow_tall])
head(logKow_tall[med_logKow_tall])
head(logKow_tall[high_logKow_tall])

sum_dose_amphib <- dermal_dose_amphib + diet_dose_amphib
percent_dermal_amphib <- dermal_dose_amphib/sum_dose_amphib
hist(percent_dermal_amphib[low_logKow_tall])
hist(percent_dermal_amphib[med_logKow_tall])
hist(percent_dermal_amphib[high_logKow_tall])

sum_dose_reptile <- dermal_dose_reptile + diet_dose_reptile
percent_dermal_reptile <- dermal_dose_reptile/sum_dose_reptile
hist(percent_dermal_reptile)
hist(percent_dermal_reptile[low_logKow_tall])
hist(percent_dermal_reptile[med_logKow_tall])
hist(percent_dermal_reptile[high_logKow_tall])

sum_dose_bird <- dermal_dose_bird + diet_dose_bird
percent_dermal_bird <- dermal_dose_bird/sum_dose_bird
hist(percent_dermal_bird)
hist(percent_dermal_bird[low_logKow_tall])
hist(percent_dermal_bird[med_logKow_tall])
hist(percent_dermal_bird[high_logKow_tall])

sum_dose_mammal <- dermal_dose_mammal + diet_dose_mammal
percent_dermal_mammal <- dermal_dose_mammal/sum_dose_mammal
hist(percent_dermal_mammal)
hist(percent_dermal_mammal[low_logKow_tall])
hist(percent_dermal_mammal[med_logKow_tall])
hist(percent_dermal_mammal[high_logKow_tall])

# Amphibia, Reptilia, Aves, Mammalia

n_low_Kow <- length(percent_dermal_amphib[low_logKow_tall])
dermal_percents <- as.numeric(c(percent_dermal_amphib[low_logKow_tall],percent_dermal_reptile[low_logKow_tall],
  percent_dermal_bird[low_logKow_tall],percent_dermal_mammal[low_logKow_tall]))
vertebrate_class <- c(rep("Amphibia",n_low_Kow),rep("Reptilia",n_low_Kow),rep("Aves",n_low_Kow),rep("Mammalia",n_low_Kow))
class(dermal_percents)
class(vertebrate_class)

dermal_low_kow <- data.frame(dermal = dermal_percents,class = vertebrate_class)
class(dermal_low_kow$dermal)
class(dermal_low_kow$class)

mc_dermal_boxplots <- paste(ams_dir_graphics,"mc_dermal_boxplots.jpg",sep="")
  jpeg(mc_dermal_boxplots, width = 6, height = 7, units = "in",res=600)
  ggplot(dermal_low_kow, aes(class, dermal))+
    geom_boxplot(fill="lightblue1",outlier.color="gray",outlier.shape=1,
                 outlier.size=0.7)+
    labs(x="Vertebrate Class", y = "Dermal proportion of total dose")+
    theme_minimal()
dev.off()
