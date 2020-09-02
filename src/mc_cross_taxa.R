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

#import pesticide data from comptox, test and opera predictors
#1835 in total
#cpcat term pesticides
#comptox batch search
#dropped any pesticides missing values for saved columns
comptox_data_filename <- "CompTox_Pesticide_search_subset.csv"
comptox_data <- read.csv(file = paste(ams_dir_input, comptox_data_filename, sep = ""), header = TRUE)
dim(comptox_data)
colnames(comptox_data)

comptox_data_tall <- rbind(comptox_data,comptox_data,comptox_data,comptox_data,comptox_data,
      comptox_data,comptox_data,comptox_data,comptox_data,comptox_data)
dim(comptox_data_tall)


nsims = 18350

# body weights
bw_amphib <- runif(nsims, min=5, max=400)
bw_reptile <- runif(nsims, min=50, max=3500)
bw_bird <- runif(nsims, min=5, max=400)
bw_mammal <- runif(nsims, min=6, max=1200)
bw_generic <- runif(nsims, min=1, max=5000)
bw_all <- cbind(bw_amphib,bw_reptile,bw_bird,bw_mammal,bw_generic)
dim(bw_all)

# field metabolic rates
fmr_amphib_nagy <-  2.25 * bw_amphib^0.808 # generic
fmr_reptile_nagy <-  0.196 * bw_reptile^0.889 # generic
fmr_bird_nagy <-  10.5 * bw_bird^0.681 # generic
fmr_mammal_nagy <-  4.82 * bw_mammal^0.734 # generic
fmr_generic_nagy <- 2.25 * bw_generic^0.808
fmr_all <- cbind(fmr_amphib_nagy,fmr_reptile_nagy,fmr_bird_nagy,fmr_mammal_nagy,fmr_generic_nagy)
dim(fmr_all)
#visual qa check
plot(log10(bw_generic), log10(fmr_generic_nagy))

#kp calculation from Walker 2003
#log kp (cm/hr) = -2.72 + 0.71 log Kow - 0.0061 MW
#table 2 formamide
# 10^(-2.72 + (0.71 * -1.51) - (0.0061 * 45.04))
kp <- 10^(-2.72 + (0.71 * comptox_data_tall$OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED) - (0.0061 * comptox_data_tall$AVERAGE_MASS))

#surface area (exposed)
sa_amphib <- 0.4 * 1.131 * bw_amphib^0.579
sa_reptile <- 0.3 * 8.42 * bw_reptile^0.694
sa_bird <- 0.07 * 10 *  bw_bird^0.667
sa_mammal <- 0.22 * 12.3 * bw_mammal^0.65 # generic from Hope 1995

#dermal

#diet

# inhalation pass
# https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/stir-version-10-users-guide-pesticide-inhalation
#also Hope 1995


