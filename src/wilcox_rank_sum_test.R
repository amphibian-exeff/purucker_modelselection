library(tools)
library(dplyr)
#wilcox rank sum test (nonparametric version of t-test between means of two indepedent samples)
field <- read.csv("data_in/field_bodyburdens9_4.csv")
lab <- read.csv("data_in/updated_amphib_dermal_collated.csv")

names(field)
names(lab)

#compare body burdens of chemicals in both datasets using wrst
unique(lab$chemical)
unique(field$chemical)
field<-na.omit(field) #remove all cases of NA
lab$chemical<-toTitleCase(as.character(lab$chemical)) # match structure of chemicals to field
field$chemical<-toTitleCase(as.character(field$chemical)) 
field$chemical<- gsub("DDT, O,p'-", "Dt", field$chemical) #change ppdt name in field

both <- intersect(unique(lab$chemical), unique(field$chemical)) #look at intersecting chemicals, pull out

lab_c<-lab[lab$chemical %in% both,]
field_c<-field[field$chemical %in% both,]

lab_c<-lab_c[,c(5,13)]
field_c<-field_c[,c(2:4)]
colnames(lab_c)[2]<-'tissue'
lab_c$Type<-'lab'
dat<-rbind(field_c,lab_c)



#do wilcox rank sum test for individual chemicals
#is there a significant difference between field and lab data for individual chemicals?
pest <- map(.x = both, 
                .f = ~ wilcox.test(formula = tissue ~ Type, 
                                   data    = subset(dat, chemical %in% .x)))
wilcox_pvals <- as.data.frame(do.call(cbind,list(t(data.frame(map(.x = pest, .f  = "p.value"))))))
row.names(wilcox_pvals) <- unlist(map(.x =both, .f = ~ paste0(.x, collapse = "")))
colnames(wilcox_pvals) <- 'pval'
#sig difference between all but atrazine - see note below
write.csv(wilcox_pvals,'data_out/wilcox_pvals.csv')

                        
#quick check to make sure it's working as intended
test1<-filter(dat, chemical == 'Atrazine')    
wilcox.test(tissue ~ Type,test1)
boxplot(tissue ~ Type, test1, frame = FALSE)   

#note that we have a single value for field here, so it's hard to tell
test2<-filter(dat, chemical == 'Bifenthrin')    
wilcox.test(tissue ~ Type,test2)
boxplot(tissue ~ Type, test2, frame = FALSE)   
