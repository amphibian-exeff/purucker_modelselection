library(dplyr)
library(tidyr)

##script to combine and edit glinksi 2013 data
setwd("C:\\Users\\eliza\\Documents\\EPA\\ORISE\\Amphibian_Paper\\amphib_code\\labvfield_cleaned\\field_data\\glinski2018")
ml<-read.csv('masterlist.csv')
tf<-read.csv('tiftonfrogs.csv')
jf<-read.csv('jjercfrogs.csv')
##remove any empty columns
names(jf)
names(tf)
tf<-tf[,c(1:154)]

##remove library match score
tf_out<-select(tf,-matches('Library.Match.Score'))
jf_out<-select(jf,-matches('Library.Match.Score'))

##remove all empty columns from both datasets for easier management
jf_out<- Filter(function(x)!all(is.na(x)), jf_out)
tf_out<- Filter(function(x)!all(is.na(x)), tf_out)


##create columns of tissue by sample, pesticide
names(tf_out)
tf_c<-gather(tf_out, "sample","tissue",5:42)
names(jf_out)
jf_c<-gather(jf_out,'sample',"tissue",5:95)

##remove extraneous strings to match unique names
#namesjf<-unique(jf_c$sample) #for examining strings
jf_n<-substring(jf_c$sample, 4)
jf_n<-substring(jf_n,1,3)
jf_n<-gsub("[.]","",jf_n) 
unique(jf_n) #check that all additional strings are gone

#namestf<-unique(tf_c$sample) #for examining strings
tf_n<-substring(tf_c$sample, 3)
tf_n<-substring(tf_n,1,2)
tf_n<-gsub("[.]","",tf_n) 
unique(tf_n) #check that all additional strings are gone

##attach back to main sheet
jf_c$name<-jf_n
tf_c$name<-tf_n

##fix same species with different names
jf_c$name<- gsub("BU", "BT", jf_c$name)
tf_c$name<- gsub("BU", "BT", tf_c$name)

##pull out unique names, add in scientific names
namesjf<-unique(tf_c$name)
namestf<-unique(jf_c$name)
speciesjf<-c("Bufo Terrestris","Tadpole","Hyla Cinerea", "Rana sphenocephala","Hyla versicolor")
speciestf<-c("Bufo Terrestris","Hyla versicolor","Hyla Cinerea","Tadpole", "Rana sphenocephala","Hyla gratiosa","Bufo fowlers")

##combine into a code sheet for matching names
code_jf<-as.data.frame(cbind(namesjf,speciesjf))
colnames(code_jf)[1]<-'name'
colnames(code_jf)[2]<-'species'
code_tf<-as.data.frame(cbind(namestf,speciestf))
colnames(code_tf)[1]<-'name'
colnames(code_tf)[2]<-'species'
#
#join to original datasheet
jf_c_f<-full_join(jf_c,code_jf,by = "name")
tf_c_f<-full_join(tf_c,code_tf,by = "name")

##check that name match
head(tf_c_f)
head(jf_c_f)

finalout<-rbind(tf_c_f,jf_c_f)
write.csv(finalout,'glinski13_fin.csv')
