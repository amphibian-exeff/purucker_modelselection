library(ggplot2)
library(ggfortify)
library(cowplot)

par <- read.csv("data_out/moments_generation_6.csv")
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

#I want to clean up the code/automate this process later; this will have to do for now

par_n<-par[c(1:6),c(2,6:15,4:5)]
head(par_n)
num=11 #the internet says do this

#ggplot(data.frame(x=c(0:10)), aes(x)) +
  #stat_function(geom="point", n=11, fun=dpois, args=list(1))
#poisson...check with Tom
p1 <- ggplot(data = data.frame(x = c(0,10)), aes(x)) +
  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[1,2], sd = par_n[1,3]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[2,2]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[3,2]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[4,2]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[5,2]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dpois, n = num, args = list(lambda = par_n[6,2]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Prior Density - Movement Rate") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), legend.position="none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))

p1


#new n for normal distributions
num=100

p2 <- ggplot(data = data.frame(x = c(-0.03, 0.2)), aes(x)) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[1,4], sd = par_n[1,5]), aes(col='Generation 1'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[2,4], sd = par_n[2,5]), aes(col='Generation 2'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[3,4], sd = par_n[3,5]), aes(col='Generation 3'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[4,4], sd = par_n[4,5]), aes(col='Generation 4'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[5,4], sd = par_n[5,5]), aes(col='Generation 5'),size=1.2) +
  stat_function(fun = dnorm, n = num, args = list(mean = par_n[6,4], sd = par_n[6,5]), aes(col='Generation 6'),size=1.2) + 
  scale_color_manual(values=c("#E69F00", "#CC0000", "#56B4E9", "#009E73", "#CC79A7", "#000000"))+
  labs(color='Prior') +
  ylab("Prior Density - Bioavailability") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), legend.position="none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
        #legend.title = element_text( size = 14, face = "bold"))
  
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
  ylab("Prior Density - Dermal Allometric Slope") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), legend.position="none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))

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
  ylab("Prior Density - Dermal Allometric Exponent") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), legend.position="none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))

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
  ylab("Prior Density - Dermal Fraction Exposed") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), legend.position="none", panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))

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
  ylab("Prior Density - Dermal Thickness") +
  xlab("Tissue Concentration (ug/g)")+
  scale_y_continuous()+
  scale_x_continuous()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.key=element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))

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

?plot_grid

###  scrap code ----
#dnorm- test
# pmean<-0.0010
# psd<-10
# x<-seq(-10,10,length=100)
# prior = dnorm(x,pmean,psd)
# plot(prior)

#testing; rnorm generates random numbers, dnorm generates  a sequence of n of your choosing
# N <- seq(-0.03, 0.5, length=100)
# gen1 <- dnorm(N, mean = par_n[1,4], par_n[1,5])
# gen2 <- dnorm(N, mean = par_n[2,4], par_n[2,5])
# gen3 <- dnorm(N, mean = par_n[3,4], par_n[3,5])
# gen4 <- dnorm(N, mean = par_n[4,4], par_n[4,5])
# gen5 <- dnorm(N, mean = par_n[5,4], par_n[5,5])
# gen6 <- dnorm(N, mean = par_n[6,4], par_n[6,5])


#color - bright colors no line capture prior 

# set.seed(13579)
# N<-100
# gen1 <- rnorm(N, mean = par_n[1,4], par_n[1,5])
# gen2 <- rnorm(N, mean = par_n[2,4], par_n[2,5])
# gen3 <- rnorm(N, mean = par_n[3,4], par_n[3,5])
# gen4 <- rnorm(N, mean = par_n[4,4], par_n[4,5])
# gen5 <- rnorm(N, mean = par_n[5,4], par_n[5,5])
# gen6 <- rnorm(N, mean = par_n[6,4], par_n[6,5])
# 
# 
# plot(density(gen1),
# ylim=c(0,60),
# xlim=c(-0.3,0.5),
# main = "Norm of Priors, Bioavailability")
# lines(density(gen2), col = "coral2")
# lines(density(gen3), col = "green3")
# lines(density(gen4), col = "thistle3")
# lines(density(gen5), col = "orange")
# lines(density(gen6), col = "cyan")
# legend("topright",                                    # Add legend to density
#        legend = c("Gen 1","Gen 2", "Gen 3","Gen 4","Gen 5","Gen 6"),
#        col = c("black", "coral2", "green3", "thistle3",'orange','cyan'),
#        lty = 7)
# plot(density(y_rnorm),                               # Plot default density
#      xlim = c(- 10, 10),
#      main = "Normal Distribution in R")
# lines(density(y_rnorm2), col = "coral2")             # Plot density with higher mean
# lines(density(y_rnorm3), col = "green3")             # Plot density with higher sd
# legend("topleft",                                    # Add legend to density
#        legend = c("Mean = 0; SD = 1",
#                   "Mean = 2; SD = 1",
#                   "Mean = 2; SD = 3"),
#        col = c("black", "coral2", "green3"),
#        lty = 1)


# p1 <- ggplot(data = data.frame(x = c(-0.5, 0.5)), aes(x)) +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[1,4], sd = par_n[1,5])) + ylab("") +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[2,4], sd = par_n[2,5])) + ylab("") +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[3,4], sd = par_n[3,5])) + ylab("") +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[4,4], sd = par_n[4,5])) + ylab("") +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[5,4], sd = par_n[5,5])) + ylab("") +
#  stat_function(fun = dnorm, n = 100, args = list(mean = par_n[6,4], sd = par_n[6,5])) + ylab("prior density") +
#  scale_y_continuous(breaks = NULL)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
#  
# p1

#this definitely doesn't work, but ideas for future for loops in ggplot2 structure
# 
# for(i in 1:ncol(test)) { 
#   for (j in 1:nrow(test)){
#  print(ggplot(data=data.frame(x = c(-0.5, 0.5)), aes(x)) +
#     stat_function(fun = dnorm, n = 100, args = list(mean = par_n[[i,j]], sd = par_n[[i,j]])) + ylab(""))+
#       scale_y_continuous(breaks = NULL)+
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
#     
#   }
# }
# 




