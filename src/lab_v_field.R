library(ggplot2)
library(dplyr)
library(tidyr)
library(wesanderson)
library(cowplot)

if(Sys.info()[4]=="LZ26EPAULUKO"){
  ams_dir <- "c:/git/amphibian_model_selection/"
}

#subdirectories
ams_dir_input <- paste(ams_dir, "data_in/", sep = "")
ams_dir_output <- paste(ams_dir, "data_out/", sep = "")
ams_dir_graphics <- paste(ams_dir, "graphics/", sep = "")

#note; if compiled already, skip and instead read compiled field_bodyburdens9_4.csv from data_in
sw<-'swanson.csv'
bg<-'battaglin.csv'
sm15<-'smalling15.csv'
sm13<-'smalling13.csv'
sm18<-'smalling18.csv'
g13<-'glinski13_fin.csv'
lab<-'amphibian_dermal_collated.csv'

sw <- read.csv(file = paste(ams_dir_input, sw, sep = ""), header = TRUE)
bg <- read.csv(file = paste(ams_dir_input, bg, sep = ""), header = TRUE)
sm15 <- read.csv(file = paste(ams_dir_input, sm15, sep = ""), header = TRUE)
sm13 <- read.csv(file = paste(ams_dir_input, sm13, sep = ""), header = TRUE)
sm18 <- read.csv(file = paste(ams_dir_input, sm18, sep = ""), header = TRUE)
g13 <- read.csv(file = paste(ams_dir_input, g13, sep = ""), header = TRUE)
lab <- read.csv(file = paste(ams_dir_input, lab, sep = ""), header = TRUE)

###group plots by type, study, pesticide
##Prep field data - each dataset should be carefully examined individually
#Battaglin16 (Var.)
head(bg)
bg_f<-gather(bg, "chemical","tissue",6:21)
names(bg_f)
bg_f<-bg_f[,c(3,5:7)]
bg_f$Type<-'Field'
bg_f$Source<-'Battaglin et al. 2016'
bg_f$unit<-'ug/kg'
colnames(bg_f)[1]<-'ID'
names(bg_f)
bg_f<-bg_f[,c(1,3:7,2)]

#smalling13 (CA)
head(sm13)
sm_f<-gather(sm13,'chemical','tissue',3:9)
sm_f<-sm_f[,c(1,3:4)]
colnames(sm_f)[1]<-'ID'
sm_f$Type<-'Field'
sm_f$Source<-'Smalling et al. 2013'
sm_f$unit<-'ug/kg'
sm_f$Species<-'Pseudacris regilla'
names(sm_f)

#smalling 2015 (IA)
head(sm15)
sm2_f<-gather(sm15,'chemical','tissue',2:4)
colnames(sm2_f)[1]<-'ID'
sm2_f$Type<-'Field'
sm2_f$Source<-'Smalling et al. 2015'
sm2_f$unit<-'ug/kg'
sm2_f$Species<-'Pseudacris maculata'
names(sm2_f)

#swanson 18 (IA)
head(sw)
sw_f<-gather(sw,'chemical','tissue',3:6)
sw_f<-sw_f[,c(1,3:4)]
sw_f$Type<-'Field'
sw_f$Source<-'Swanson et al. 2018'
sw_f$unit<-'ng/g'
sw_f$Species<-'Rana sphenocephala'
names(sw_f)

#USGS 18 (NJ)
names(sm18)
colnames(sm18)[1]<-'ID'
colnames(sm18)[2]<-'tissue'
sm18$Type<-'Field'
sm18$Source<-'USGS 2018'
sm18$unit<-'ug/kg' 
sm18$Species<-'Various'
sm18$chemical<-'various'
head(sm18)
sm18f<-sm18[,c(1,7,2:6)] #reorder

#Glinski 13 (GA)
names(g13)
g13<-g13[,c(4,6:7,9)]
colnames(g13)[1]<-'chemical'
colnames(g13)[2]<-'ID'
colnames(g13)[4]<-'Species'
g13$Type<-'Field'
g13$Source<-'Glinski 2020'
g13$unit<-'ug/g' 
names(g13)
names(bg_f)
g13f<-g13[,c(2,1,3,5,6,7,4)] #reorder


#check structure and names 
df_list<-list(bg_f,sw_f,sm_f,sm2_f,sm18f,g13f)
unlist(lapply(df_list,str)) #for easier comparison
lapply(df_list,names)

field<-rbind(bg_f,sw_f, sm_f, sm2_f,sm18f) #all except glinski have alternative units
field$tissue<-as.numeric(as.character(field$tissue))
field$tissue<-field$tissue/1000 #put all in same units; both ug/kg and ng/g to ug/g 
str(field)
field<-rbind(field,g13f) #add in glinski, which is already in ug/g
field$applicationrate<-'unknown'
write.csv(field,'field_bodyburdens9_4.csv')

#fd<-'field_bodyburdens9_4.csv'
#field<-read.csv(file = paste(ams_dir_input, fd, sep = ""), header = TRUE)

##Lab
#match names/structure of lab data to field data
names(field)
names(lab)
lab_f<-lab[,c(1,4,5,8,11:13)]
names(lab_f)
colnames(lab_f)[1]<-'applicationrate'
colnames(lab_f)[6]<-'Species'
colnames(lab_f)[7]<-'tissue'
colnames(lab_f)[4]<-'ID'
colnames(lab_f)[5]<-'Source'
lab_f$Type<-'Lab'
lab_f$unit<-'ug/g'

names(lab_f)
names(field)
lab_fin<-lab_f %>% select(ID,chemical, tissue, Type, Source,unit,Species, applicationrate)    
names(lab_fin) #check that names align

#note that new output datasets should be generated prior to this step to retain correct units
lab_fin$Source<-'Lab'
all<-rbind(lab_fin, field)
all$tissue<-log(all$tissue) #do log normalized version for visuals


###plot by density, source (KDES, figure 1)
px <- ggplot(all, aes(x=tissue, group=Type)) + 
  geom_density(aes(fill=Type), alpha=0.5)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"))+ 
  xlab('Log normalized tissue concentrations across all pesticides and species (ug/g)')+
  ylab('Density')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
px

# build the data displayed on the plot.
px.data <- ggplot_build(px)$data[[1]]
# Note that column 'scaled' is used for plotting
# so we extract the max density row for each group
px.text <- lapply(split(px.data, f = px.data$group), function(df){
  df[which.max(df$scaled), ]
})
px.text <- do.call(rbind, px.text)
#px.text <- do.call(rbind, px.text)  # we can also get p.text with dplyr.
# now add the text layer to the plot
px + annotate('text', x = px.text$x, y = px.text$y,
              label = sprintf('n = %d', px.text$n), vjust = -0.4, hjust =1.3, size=4)


###plot by points, and individual pesticides (point plot), figure 3
#let's change all chemical names to lowercase
test<-all %>% mutate(chemical = tolower(chemical))
unique(g13f$chemical)#n34 at beginning, now 30 same number of records, we haven't dropped anything by accident
#fix ppddt 
all<-test
#fix naming
all$chemical<- gsub("dt", "ppddt", all$chemical)
all$chemical<- gsub("ppdppddt", "ppddt", all$chemical)
all$chemical<- gsub("x35.dca", "35'dca", all$chemical)
all$chemical<- gsub("x3.5.dca", "35'dca", all$chemical)
all$chemical<- gsub("x34.dca", "34'dca", all$chemical)
#for this one we will remove rows from the USGS, as specific chemicals weren't reported
alln<-subset(all, Source!="USGS 2018"& Source!="Glinski 2013")

unique(alln$Source) #double check
pp <- ggplot(alln, aes(x=chemical, y=tissue, fill=Type)) + 
  # geom_boxplot()+
  geom_point(aes(color=Type))+
  #scale_color_manual(values=wes_palette(n=7, name="Darjeeling2", type='continuous'))+
  scale_color_manual(values=wes_palette(n=2, name="Darjeeling2"))+ 
  #scale_fill_brewer(palette="Dark2")+
  xlab('Pesticide')+
  ylab('Log Tissue Conc.(ug/g) Reported in Published Studies')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(face = "bold", size = 10, angle = 90, hjust= 0.2, vjust=0.2))
#EnvStats:: stat_n_text() 

pp




###plot by density, source (KDES, figure 2) - make some alterations to names and structure to align plots
#stack figures on top of each other; one with all lab studies, and one with all field studies
#drop lab data for figure 2a

allf<-subset(all, Type!="Lab")
allf$Paper<-allf$Source
ps <- ggplot(allf, aes(x=tissue, group=Paper)) + 
  geom_density(aes(fill=Paper), alpha=0.4)+
  scale_fill_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7","#F0E442", "#000000"))+
  xlab('Log normalized tissue concentrations by study across all pesticides and species (ug/g), Field')+
  ylab('Density')+
  scale_x_continuous(limits = c(-10, 5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
ps

# # build the data displayed on the plot.
# ps.data <- ggplot_build(ps)$data[[1]]
# # Note that column 'scaled' is used for plotting
# # so we extract the max density row for each group
# ps.text <- lapply(split(ps.data, f = ps.data$group), function(df){
#   df[which.max(df$scaled), ]
# })
# ps.text <- do.call(rbind, ps.text)
# # now add the text layer to the plot
# ps + annotate('text', x = ps.text$x, y = ps.text$y,
#               label = sprintf('n = %d', ps.text$n), vjust = -0.6, hjust =1.0, angle=0, size=5)


#use original lab data
#add in names for each source

#Van Meter 2016
#Van Meter 2015
#Van Meter 2017
#Glinski 2018a (hydration)
#Glinski 2019 (biomarker)
#Henson-Ramsey 2008
#Glinski 201 (metabolites)
#Glinski 2018b (dermal)
lab_f$Paper<-lab_f$Source
lab_f$Paper<- gsub("rvm2015", "Van Meter et al. 2015", lab_f$Paper)
lab_f$Paper<- gsub("rvm2016", "Van Meter et al. 2016", lab_f$Paper)
lab_f$Paper<- gsub("rvm2017", "Van Meter et al. 2017", lab_f$Paper)
lab_f$Paper<- gsub("dag_dehydration", "Glinski et al. 2018a", lab_f$Paper)
lab_f$Paper<- gsub("dag_metabolites", "Glinski et al. 2018b", lab_f$Paper)
lab_f$Paper<- gsub("dag_biomarker", "Glinski et al. 2019", lab_f$Paper)
lab_f$Paper<- gsub("dag_dermal_routes", "Glinski et al. 2020", lab_f$Paper)
lab_f$Paper<- gsub("hr2008", "Henson-Ramsey et al. 2008", lab_f$Paper)
unique(lab_f$Paper)

lab_f$tissue<-log(lab_f$tissue) 
psl <- ggplot(lab_f, aes(x=tissue, group=Paper)) + 
  geom_density(aes(fill=Paper), alpha=0.4)+
  scale_fill_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7","#F0E442", "#003300","#CC3300"))+
  xlab('Log normalized tissue concentrations by study across all pesticides and species (ug/g), Lab')+
  ylab('Density')+
  scale_x_continuous(limits = c(-10, 5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
psl

# # build the data displayed on the plot.
# psl.data <- ggplot_build(psl)$data[[1]]
# # Note that column 'scaled' is used for plotting
# # so we extract the max density row for each group
# psl.text <- lapply(split(psl.data, f = psl.data$group), function(df){
#   df[which.max(df$scaled), ]
# })
# psl.text <- do.call(rbind, psl.text)
# # now add the text layer to the plot
# psl + annotate('text', x = psl.text$x, y = psl.text$y,
#               label = sprintf('n = %d', psl.text$n), vjust = -0.6, hjust =1.0, angle=0, size=5)


plot_grid(ps, psl,
          labels = c('Fig A','Fig B'),
          label_x = 0.2,
          nrow = 2)


