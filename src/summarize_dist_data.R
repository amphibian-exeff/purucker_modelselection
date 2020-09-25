
#code that summarizes the distribution of empirical data based on all combinations of 
#species x chemical x publication
#assume normal distribution, return the mean and sd

lab <- read.csv("data_in/updated_amphib_dermal_collated.csv")
lab<-lab[,c(2:13)]
levels(lab$source) #10 sources
#species x chemical x publication
#split by source

by_s<-split(lab, lab$source) 

  out<-do.call(rbind.data.frame, lapply(by_s,function(x){
    with(x, aggregate(tissue_conc_ugg ~ species + chemical, 
                      FUN =  function(y) c( MN= mean(y), SD = sd(y) ))) 

  }))
  
out #check structure
out$source<-rownames(out)
#fix nested dimensions within output, final output
fin_list<-do.call(data.frame, out)
