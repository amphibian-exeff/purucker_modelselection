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
opera_predictions <- subset(opera_predictions,select=c(MoleculeID,MolWeight,LogP_pred,BioDeg_LogHalfLife_pred))

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
#View(tr_observations_write)

tr_observations_write_filename <- "tr_observations_w_covars.csv"
write.csv(tr_observations_write,paste(ams_dir_output, tr_observations_write_filename, sep = ""))
