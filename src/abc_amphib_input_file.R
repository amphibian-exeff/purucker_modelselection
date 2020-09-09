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

#load in observational body burdens
tr_observations_filename <- "amphib_dermal_collated.csv"
tr_observations_data <- read.csv(file = paste(ams_dir_input, tr_observations_filename, sep = ""), header = TRUE)
dim(tr_observations_data)
colnames(tr_observations_data)

#list unique chemicals in opera predictions by dtxsid
unique(tr_observations_data$chemical)

#load in opera predictions
opera_predictions_filename <- "dsstox_ids_pesticides_opera.csv"
opera_predictions <- read.csv(file = paste(ams_dir_input, opera_predictions_filename, sep = ""), header = TRUE)
dim(opera_predictions)
colnames(opera_predictions)
opera_predictions <- subset(opera_predictions,select=c(MoleculeID,LogP_pred))

#list dtxsids of opera predictors
opera_predictions$MoleculeID

#load mapping of pesticides to dsstox_ids
dsstox_pesticides_filename <- "dsstox_ids_pesticides.csv"
dsstox_pesticides <- read.csv(file = paste(ams_dir_input, dsstox_pesticides_filename, sep = ""), header = TRUE)
dim(dsstox_pesticides)
colnames(dsstox_pesticides)

#add dsstox and preferred_name to tr_observations_data
colnames(tr_observations_data)
colnames(dsstox_pesticides)
tr_observations_temp <- merge(tr_observations_data,dsstox_pesticides)
colnames(tr_observations_temp)
tr_observations_temp <- subset(tr_observations_temp,select=-c(standard_InChI,standard_InChIKey))

# add logkow
# tr_observations_temp$dsstox_substance_id and opera_predictions$MoleculeID are the same
colnames(opera_predictions)[1] <- "dsstox_substance_id"
colnames(opera_predictions)
tr_observations_write <- merge(tr_observations_temp,opera_predictions)
View(tr_observations_write)

tr_observations_write_filename <- "tr_observations_w_covars.csv"
tr_observations_data <- write.csv(tr_observations_write,paste(ams_dir_output, tr_observations_write_filename, sep = ""))


#You have already done 50k simulations from the uniform priors. We are going to take the first 10k (not the best 10k, do not sort) of these as the first generation of simulations. We are simply throwing away the last 40k and not using them for the official simulations for the manuscript. This is necessary to honor the assumptions behind ABC-MC.
#Calculate the average of the 3 nses for each of the 10k simulations: nse_average = mean(nse_conc, nse_flow, nse_ flux)
#Take the top 2.5k simulations based on the nse_average, disregarding the other 7.5K simulations.
#Use the 2.5k set of inputs associated with these top 2.5k simulations. Calculate the unweighted kernel densities using the kde package and fit to a normal distribution, truncate at the range limits for each parameter.
#Find the first_quartile_average_nse of the original 10k nse_averages, this will be the average of the 2500 and 2501st highest average_nse of the first 10k of generation zero.

#These steps are only for generation 1 of the simulation sets, this approach is slightly different than above for generation zero.
#6.	Use the truncated normal distributions from 4) to set up the next round of simulations. Simulate with these inputs. For each simulation, calculate the average_nse. We will only keep individual simulations if the average_nse is higher that the first_quartile_average_nse (calculated in step 5) from the last set of simulations. 
#7.	Keep simulating until we get 10k new simulations with an average_nse greater than the first_quartile_average_nse from step 5 calculated for the previous set of 10k simulation results. You can make this decision of which ones to keep on the hpc server or afterwards after doing a bunch of simulations.
#8.	Calculate the updated unweighted kernel densities based on these new 10k simulations and fit to the normal distribution, truncate at the original range limits for each parameter.
#9.	Now use these new 10k simulations to calculate the updated first_quartile_average_nse, this will be the average of the 2500 and 2501st highest average_nse.
#These steps are for generations 2 . N of the simulation sets, this approach is slightly different than for generations zero and 1.
#10.	Use the truncated normal distributions from the previous generation of simulations to set up the next round of simulations. Simulate with these inputs. For each simulation, calculate the average_nse. We will only keep individual simulations if the average_nse is higher that the first_quartile_average_nse from the last set of simulations. 
#11.	Keep simulating until we get 10k new simulations with an average_nse greater than the first_quartile_average_nse calculated for the previous set of 10k simulation results.  You can make this decision of which ones to keep on the hpc server or afterwards after doing a bunch of simulations. Note that this may require more than 50k simulations to get the needed 10k good simulations.
#12.	Calculate the updated unweighted kernel densities based on the latest set of 10k simulations and fit to the normal distribution, truncating at the original range limit.
#13.	Calculate the updated first_quartile_average_nse for the new set of 10k simulations, this will be the average of the 2500 and 2501st highest average_nse.
#14.	Repeat steps 10)-13) over and over until the median_average_nse fails to improve by X% versus the median_average_nse from the last generation. We have not explicitly defined what this percentage is just yet. It will probably be something like 1% or less.
