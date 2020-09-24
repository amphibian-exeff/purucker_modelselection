##################################################################
### NEXT Generation
nsims <- 20000
winner_count <- 5000
#while loop to ensure that the number of sims that beat the score stays above the target
generation <- 1
max_generations <- 40

while(winner_count >= 2500){
  
  generation = generation + 1
  generation_moments[generation,1] <- generation
  
  #initialize empty arrays in case we want to increase them later
  #create matrix to accept the sums of absolute differences for each simulation in each gneration
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
  
  #get score to beat
  gen_score_median <- generation_moments[generation,15]
  
  #draw from updated distributions
  dt_amphib <- rtruncnorm(nsims, generation_moments[1,3], generation_moments[1,4], 
                          mean = generation_moments[generation,3], generation_moments[generation,4])
  movement_rate <- rpois(nsims,generation_moments[generation,5])+1
  bioavail <- rtruncnorm(nsims, generation_moments[1,7], generation_moments[1,8], 
                         mean = generation_moments[generation,7], generation_moments[generation,8])
  dermal_sa_slope <- rtruncnorm(nsims, generation_moments[1,9], generation_moments[1,10], 
                                mean = generation_moments[generation,9], generation_moments[generation,10])
  dermal_sa_exponent <- rtruncnorm(nsims, generation_moments[1,11], generation_moments[1,12], 
                                   mean = generation_moments[generation,11], generation_moments[generation,12])
  dermal_fraction <- rtruncnorm(nsims, generation_moments[1,13], generation_moments[1,14], 
                                mean = generation_moments[generation,13], generation_moments[generation,14])
  
  #hist(dt_amphib)
  #hist(movement_rate)
  #hist(bioavail)
  #hist(dermal_sa_slope)
  #hist(dermal_sa_exponent)
  #hist(dermal_fraction)

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
  plot(concs_v_sims)
  concs_v_sims_write_filename <- paste("concs_v_sims_generation_",generation,".csv",sep="")
  write.csv(concs_v_sims,paste(ams_dir_output, concs_v_sims_write_filename, sep = ""))
  
  #subset the ones who beat the last median
  winner_count <- sum(dermal_sum_abs_differences[generation,] < gen_score_median)
  print(winner_count)
  
  # recreate the cbind matrix of inputs and fit scores
  # this matrix is nsims*nrows
  scores_inputs <- matrix(c(dermal_sum_abs_differences[generation,], prop_underpredict, dt_amphib, movement_rate, bioavail,
                          dermal_sa_slope, dermal_sa_exponent, dermal_fraction),
                          nrow=nsims, ncol=8, byrow = FALSE)
  dim(scores_inputs)
  colnames(scores_inputs) <- c("score","prop_underpredict","dt_amphib","movement_rate","bioavail",
                               "dermal_sa_slope","dermal_sa_exponent","dermal_fraction")
  head(scores_inputs)
  
  #write the inputs and fit scores
  inputs_fits_write_filename <- paste("inputs_fits_generation_",generation,".csv",sep="")
  write.csv(scores_inputs,paste(ams_dir_output, inputs_fits_write_filename, sep = ""))
  
  # sort by score and take the top 25%
  which_winners <- which(dermal_sum_abs_differences[generation,] < gen_score_median)
  winners <- as.data.frame(scores_inputs[which_winners,])
  dim(winners)
  winners_2500 <- as.data.frame(winners[1:2500,])
  
  #View(winners_2500)
  dim(winners_2500)
  colnames(winners_2500)
  
  # update input distributions and plot them
  #hist(winners_2500$dt_amphib)
  update_dt_amphib <- fitdist(winners_2500$dt_amphib*1000, "norm") #*1000 due to precision issues
  #hist(winners_2500$movement_rate)
  movement_rate_less_1 <- winners_2500$movement_rate-1 #removing the 1 that was added for the initital patch
  update_movement_rate <- fitdist(movement_rate_less_1, "norm")
  #hist(winners_2500$bioavail)
  update_bioavail <- fitdist(winners_2500$bioavail*1000, "norm")
  #hist(winners_2500$dermal_sa_slope)
  update_dermal_sa_slope <- fitdist(winners_2500$dermal_sa_slope, "norm")
  #hist(winners_2500$dermal_sa_exponent)
  update_dermal_sa_exponent <- fitdist(winners_2500$dermal_sa_exponent, "norm")
  #hist(winners_2500$dermal_fraction)
  update_dermal_fraction <- fitdist(winners_2500$dermal_fraction, "norm")
  
  #find the median of the 2500 as the criterion for the next round
  #hist(winners_2500$score)
  gen_score_median <- median(winners_2500$score)
  next_generation <- generation + 1
  #store the updated parameters by generation
  generation_moments[generation,1] = generation
  generation_moments[generation,2] = prop_underpredict_mean
  generation_moments[next_generation,3] = update_dt_amphib$estimate[1]/1000
  generation_moments[next_generation,4] = update_dt_amphib$estimate[2]/1000
  generation_moments[next_generation,5] = update_movement_rate$estimate[1]
  generation_moments[next_generation,6] = update_movement_rate$estimate[1] #kept poisson sd because it was uniform in first gen
  generation_moments[next_generation,7] = update_bioavail$estimate[1]/1000
  generation_moments[next_generation,8] = update_bioavail$estimate[2]/1000
  generation_moments[next_generation,9] = update_dermal_sa_slope$estimate[1]
  generation_moments[next_generation,10] = update_dermal_sa_slope$estimate[2]
  generation_moments[next_generation,11] = update_dermal_sa_exponent$estimate[1]
  generation_moments[next_generation,12] = update_dermal_sa_exponent$estimate[2]
  generation_moments[next_generation,13] = update_dermal_fraction$estimate[1]
  generation_moments[next_generation,14] = update_dermal_fraction$estimate[2]
  generation_moments[next_generation,15] = gen_score_median
  
  score_improvement <- abs(generation_moments[next_generation,15]-generation_moments[generation,15])/generation_moments[generation,15]
  score_improvement
  print(sum(dermal_sum_abs_differences[generation,] < gen_score_median))
  View(generation_moments)
  
  moments_write_filename <- paste("moments_generation_",generation,".csv",sep="")
  write.csv(generation_moments,paste(ams_dir_output, moments_write_filename, sep = ""))
  
  plot_title=paste("Gen",generation,"; p_under =",round(prop_underpredict_mean,3), "; good_sims =", winner_count, "; improve = ", round(score_improvement, 3))
  plot(concs_v_sims,main =plot_title)
}



