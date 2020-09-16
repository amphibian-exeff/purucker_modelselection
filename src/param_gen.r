library(ggplot2)
library(ggfortify)
library(cowplot)

#tom epa laptop home
if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  ams_dir <- "c:/git/amphibian_model_selection/"
}

#subdirectories
ams_dir_input <- paste(ams_dir, "data_in/", sep = "")
ams_dir_output <- paste(ams_dir, "data_out/", sep = "")
ams_dir_graphics <- paste(ams_dir, "graphics/", sep = "")

generations_file <- paste(ams_dir_output,"moments_generation_6_old.csv", sep="")
par <- read.csv(generations_file)
names(par)

#go ahead remove 7th row; priors for the 7th gen, but stopping at 6
#movement = movement term
#bioavail = bioavailabiliy
#dermal sa slope = dermal allometric slope
#dermal sa exponent = dermal allometric exponent
#dermal fraction = dermal fraction exposed

#2 rows x 3 columns
#gen 1 is uniform for all 6 params
#gen 2-6 is poisson for movement, uniform for all others
#consistent change of colors through gens

par_n<-par[c(1:6),c(2,6:15,4:5)]
head(par_n)

num=16
#poisson...check with Tom
p1 <- ggplot(data = data.frame(x = c(0:15)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[1,2], sd = par_n[1,3]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[2,2]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[3,2]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[4,2]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[5,2]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[6,2]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Movement Rate (hr)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))

p1


num=101
p2 <- ggplot(data = data.frame(x = c(-0.03, 0.2)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,4], sd = par_n[1,5]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,4], sd = par_n[2,5]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,4], sd = par_n[3,5]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,4], sd = par_n[4,5]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,4], sd = par_n[5,5]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,4], sd = par_n[6,5]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Bioavailability")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))
  
p2


p3 <- ggplot(data = data.frame(x = c(-5, 15)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,6], sd = par_n[1,7]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,6], sd = par_n[2,7]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,6], sd = par_n[3,7]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,6], sd = par_n[4,7]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,6], sd = par_n[5,7]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,6], sd = par_n[6,7]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Dermal Allometric Slope")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))

p3


p4 <- ggplot(data = data.frame(x = c(0.55,0.65)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,8], sd = par_n[1,9]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,8], sd = par_n[2,9]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,8], sd = par_n[3,9]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,8], sd = par_n[4,9]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,8], sd = par_n[5,9]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,8], sd = par_n[6,9]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Dermal Allometric Exponent")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))

p4


p5 <- ggplot(data = data.frame(x = c(-0.2,0.8)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,10], sd = par_n[1,11]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,10], sd = par_n[2,11]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,10], sd = par_n[3,11]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,10], sd = par_n[4,11]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,10], sd = par_n[5,11]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,10], sd = par_n[6,11]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Dermal Fraction Exposed")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))

p5


p6 <- ggplot(data = data.frame(x = c(0,0.005)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,12], sd = par_n[1,13]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,12], sd = par_n[2,13]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,12], sd = par_n[3,13]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,12], sd = par_n[4,13]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,12], sd = par_n[5,13]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,12], sd = par_n[6,13]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Density") +
  xlab("Dermal Thickness (cm)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12),
        axis.title.x = element_text(face='bold'))

p6


prow <- plot_grid( p1 + theme(legend.position="none"),
                   p2 + theme(legend.position="none"),
                   p3 + theme(legend.position="none"),
                   p4 + theme(legend.position="none"),
                   p5 + theme(legend.position="none"),
                   p6 + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2,
                   ncol=3
)

legend_b <- get_legend(p6 + theme(legend.position="top"))
p <- plot_grid(prow, legend,ncol = 2, rel_widths = c(1, .1))
p


