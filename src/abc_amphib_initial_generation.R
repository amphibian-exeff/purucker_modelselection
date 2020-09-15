#check to make sure required packages are installed
list.of.packages <- c("fitdistrplus","ggplot2","GLDEX","truncnorm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) {install.packages(new.packages)}

library(fitdistrplus)
library(ggplot2)
library(GLDEX)
library(truncnorm)


###############################################################
#tom epa laptop home
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  ams_dir <- "c:/git/amphibian_model_selection/"
}

#subdirectories
ams_dir_input <- paste(ams_dir, "data_in/", sep = "")
ams_dir_output <- paste(ams_dir, "data_out/", sep = "")
ams_dir_graphics <- paste(ams_dir, "graphics/", sep = "")

# simulations for each generation
nsims <- 20000
max_generations <- 40

#load in observational body burdens
tr_observations_covars_filename <- "tr_observations_w_covars.csv"
tr_observations_abc <- read.csv(file = paste(ams_dir_output, tr_observations_covars_filename, sep = ""), header = TRUE)
colnames(tr_observations_abc)

# experimental soil concentrations
# use when available, otherwise a function of application rate
soil_concs <- tr_observations_abc$soil_conc_ugg
#change this to equation from Weir which gives 11.2 for 1 lb/ac
#soil concentration from 1 lb/acre application (112 mg/m2), mixed evenly 1 cm depth, contact top mm
#original soil concs are in ug/g, convert to /m2
soil_concs[which.na(soil_concs)] <- 11.2

#calculate permeability coefficients based on logkow and molecular weight, cm/hr
#walker et al 2003 eq 4 for organics except phenols and aliphatic aldohols
logKow <- tr_observations_abc$LogP_pred
mol_weight <- tr_observations_abc$MolWeight
tr_observations_abc$kp <- 10^(-2.72 + (0.71 * logKow) - (0.0061 * mol_weight))
kp <- tr_observations_abc$kp
hl_hours <- exp(tr_observations_abc$BioDeg_LogHalfLife_pred)*24

#calculate dermal surface area based on body weight
bw_amphib <- tr_observations_abc$body_weight_g
namphibs <- length(bw_amphib)

#assign exposure duration
dat <- tr_observations_abc$exp_duration
#hist(dat)

# tissue concentration observations
amphib_concs <- tr_observations_abc$tissue_conc_ugg

##################################################################
### INITIAL Generation
generation = 1

#initialize empty arrays in case we want to increase them later
#create matrix to accept the sums of absolute differences for each simulation in each gneration
# 20 generations by nsims
dermal_sum_abs_differences <- matrix(data=NA, nrow=max_generations, ncol=nsims)

# tracker for proportion of underpredicted concentrations for a generation
prop_underpredict <- vector(mode="numeric", length=nsims)

#parameters that will be uncertain
dt_amphib <- vector(mode="numeric", length=nsims)
movement_mean <- vector(mode="numeric", length=nsims)
movement_rate <- vector(mode="numeric", length=nsims)
bioavail <- vector(mode="numeric", length=nsims)
dermal_sa_slope <- vector(mode="numeric", length=nsims)
dermal_sa_exponent <- vector(mode="numeric", length=nsims)
dermal_fraction <- vector(mode="numeric", length=nsims)

#uncertain parameters
#dermal thickness
dt_amphib <- runif(nsims, min=0.001, max=0.003)

#poisson movement rate
movement_mean <- runif(nsims, min=0.001, max=10)
for(i in 1:nsims){
  movement_rate[i] <- rpois(1,movement_mean[i])+1 #+1 since they occupy a patch when they dont move
}
bioavail <- runif(nsims, min = 0.01, max=0.21)

#create matrix namphib rows and nsim columns
dermal_dose_amphib <- matrix(data=NA, nrow=namphibs, ncol=nsims)

#allometric relationship for surface area
dermal_sa_slope <- runif(nsims, min=1.131, max=11.003)
dermal_sa_exponent <- runif(nsims, min=0.579, max=0.6112)

#fraction of dermal surface area exposed
dermal_fraction <- runif(nsims, min=0.1, max=0.5)

#create blank generation table to remember summary stats and distribution moments
generation_moments <- matrix(data=NA,nrow=20,ncol=15)
colnames(generation_moments) <- c("gen","prop_underpredict_mean","dt_mean","dt_sd","movement_rate_mean","movement_rate_sd","bioavail_mean","bioavail_sd",
                                  "dermal_sa_slope_mean","dermal_sa_slope_sd","dermal_sa_exponent_mean","dermal_sa_exponent_sd",
                                  "dermal_fraction_mean","dermal_fraction_sd","score_median")

#remember uniform limits, after generation 1 everything is poisson or normal
generation_moments[1,3] = 0.001 #update_dt_amphib$estimate[1]/1000
generation_moments[1,4] = 0.003 #update_dt_amphib$estimate[2]/1000
generation_moments[1,5] = 0.001 #update_movement_rate$estimate
generation_moments[1,6] = 10 #update_movement_rate$estimate #kept poisson sd because it was uniform in first gen
generation_moments[1,7] = 0.01 #update_bioavail$estimate[1]
generation_moments[1,8] = 0.21 #update_bioavail$estimate[2]
generation_moments[1,9] = 1.131 #update_dermal_sa_slope$estimate[1]
generation_moments[1,10] = 11.003 #update_dermal_sa_slope$estimate[2]
generation_moments[1,11] = 0.579 #update_dermal_sa_exponent$estimate[1]
generation_moments[1,12] = 0.6112 #update_dermal_sa_exponent$estimate[2]
generation_moments[1,13] = 0.1 #update_dermal_fraction$estimate[1]
generation_moments[1,14] = 0.5 #update_dermal_fraction$estimate[2]

#calculate tissue residues
# movement_rate[i] + 1 is for initial position
#vectorization for speed
for(i in 1:nsims){
  dt_amphib_iteration <- rep(dt_amphib[i],namphibs)
  movement_rate_iteration <- rep((movement_rate[i]),namphibs)
  soil_concs_degradation <- -(log(2)/hl_hours*dat/movement_rate_iteration)
  bioavailability_iteration <- rep(bioavail[i],namphibs)
  dermal_sa_slope_iteration <- rep(dermal_sa_slope[i],namphibs)
  dermal_sa_exponent_iteration <- rep(dermal_sa_exponent[i],namphibs)
  dsa_amphib <- dermal_sa_exponent_iteration * bw_amphib^dermal_sa_exponent_iteration
  dermal_fraction_iteration <- rep(dermal_fraction[i],namphibs)
  dermal_dose_amphib[,i] <- (soil_concs^soil_concs_degradation * kp * (dsa_amphib/dt_amphib_iteration) * dermal_fraction_iteration * 
                               bioavailability_iteration)/bw_amphib
  dermal_sum_abs_differences[generation,i] <- sum(abs(log(dermal_dose_amphib[,i]) - log(amphib_concs)))
  prop_underpredict[i] <- sum(dermal_dose_amphib[,i]<amphib_concs)/namphibs
}
prop_underpredict_mean <- mean(prop_underpredict)

#write amphib_concs and prediction posteriors for each frog
dim(dermal_dose_amphib)
amphib_sim_means <- rowMeans(dermal_dose_amphib)
concs_v_sims <- as.data.frame(cbind(amphib_concs, amphib_sim_means))
concs_v_sims_write_filename <- paste("concs_v_sims_generation_",generation,".csv",sep="")
write.csv(concs_v_sims,paste(ams_dir_output, concs_v_sims_write_filename, sep = ""))

# create the cbind matrix of inputs and fit scores
# this matrix is nsims rows
scores_inputs <- matrix(c(dermal_sum_abs_differences[generation,],prop_underpredict,dt_amphib,movement_rate,bioavail,
                          dermal_sa_slope,dermal_sa_exponent, dermal_fraction),nrow=nsims,ncol=8,byrow = FALSE)
dim(scores_inputs)
colnames(scores_inputs) <- c("score","prop_underpredict","dt_amphib","movement_rate","bioavail",
                             "dermal_sa_slope","dermal_sa_exponent","dermal_fraction")
head(scores_inputs)
#View(scores_inputs)

#write the inputs and fit scores
inputs_fits_write_filename <- paste("inputs_fits_generation_",generation,".csv",sep="")
write.csv(scores_inputs,paste(ams_dir_output, inputs_fits_write_filename, sep = ""))

# sort by score and take the top 25%
winners_10000 <- as.data.frame(scores_inputs[order(scores_inputs[,1], decreasing = FALSE),][1:10000,])
dim(winners_10000)
colnames(winners_10000)

# update input distributions and plot them
#hist(winners_10000$dt_amphib)
update_dt_amphib <- fitdist(winners_10000$dt_amphib*1000, "norm") #*1000 due to precision issues
#hist(winners_10000$movement_rate)
movement_rate_less_1 <- winners_10000$movement_rate-1 #removing the 1 that was added for the initital patch
update_movement_rate <- fitdist(movement_rate_less_1, "pois") 
#hist(winners_10000$bioavail)
update_bioavail <- fitdist(winners_10000$bioavail, "norm")
#hist(winners_10000$dermal_sa_slope)
update_dermal_sa_slope <- fitdist(winners_10000$dermal_sa_slope, "norm")
#hist(winners_10000$dermal_sa_exponent)
update_dermal_sa_exponent <- fitdist(winners_10000$dermal_sa_exponent, "norm")
#hist(winners_10000$dermal_fraction)
update_dermal_fraction <- fitdist(winners_10000$dermal_fraction, "norm")

#find the median of the 2500 as the criterion for the next round
hist(winners_10000$score)
gen_score_median <- median(winners_10000$score)
next_generation <- generation + 1
#store the updated parameters by generation
generation_moments[generation,1] = generation
generation_moments[generation,2] = prop_underpredict_mean
generation_moments[next_generation,3] = update_dt_amphib$estimate[1]/1000
generation_moments[next_generation,4] = update_dt_amphib$estimate[2]/1000
generation_moments[next_generation,5] = update_movement_rate$estimate
generation_moments[next_generation,6] = update_movement_rate$estimate #kept poisson sd because it was uniform in first gen
generation_moments[next_generation,7] = update_bioavail$estimate[1]
generation_moments[next_generation,8] = update_bioavail$estimate[2]
generation_moments[next_generation,9] = update_dermal_sa_slope$estimate[1]
generation_moments[next_generation,10] = update_dermal_sa_slope$estimate[2]
generation_moments[next_generation,11] = update_dermal_sa_exponent$estimate[1]
generation_moments[next_generation,12] = update_dermal_sa_exponent$estimate[2]
generation_moments[next_generation,13] = update_dermal_fraction$estimate[1]
generation_moments[next_generation,14] = update_dermal_fraction$estimate[2]
generation_moments[next_generation,15] = gen_score_median
#gen_score_median  [1] 2537.324
gen_score_median
#View(generation_moments)

moments_write_filename <- paste("moments_generation_",generation,".csv",sep="")
write.csv(generation_moments,paste(ams_dir_output, moments_write_filename, sep = ""))

plot_title=paste("Generation",generation,"; prop underpredict =",round(prop_underpredict_mean,3))
plot(concs_v_sims,main =plot_title)

##################################################################
### qa type visuals
#visual check on predictions and ratios
hist(dermal_dose_amphib[,1]/amphib_concs) #ratios for first frog
head(dermal_dose_amphib[,1]) #predictions for first frog
head(amphib_concs)
head(dermal_dose_amphib[,2]) #predictions for second frog
head(sort(dermal_dose_amphib[,1]/amphib_concs,decreasing=T))
head(sort(dermal_dose_amphib[,1]/amphib_concs,decreasing=F))
head(sort(dermal_dose_amphib[,2]/amphib_concs,decreasing=T))
head(sort(dermal_dose_amphib[,1]/amphib_concs,decreasing=F))
#View(dermal_dose_amphib)

#visual check on absolute differencs
hist(dermal_sum_abs_differences)
head(dermal_sum_abs_differences[generation,])

#visual on underpredictions
hist(prop_underpredict)

#the scores of the best and the worst simulations
head(sort(dermal_sum_abs_differences[generation,],decreasing=F))
head(sort(dermal_sum_abs_differences[generation,],decreasing=T))



