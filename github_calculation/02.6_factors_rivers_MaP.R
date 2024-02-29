#derive factors for losses

# take three approaches: low retention, mid retention and high retention
main.path <- "PhD/mennekes2.0/"
#

library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)
library(sf)
library(reshape2)


#make data frame as simple river

d <- data.frame(L_steps = rep(1000, 100), #in m -> each step is 1000m = 1km
								flow_velocity = 1, #in m/s
								high = 0,
								mid = 0,
								low = 0)



#Factors for Landuse: use low, middle and high factors which will be attributed to different landuses

#high
v_high <- 0.00012
(1- exp(1)^(-v_high*(25000))) #should be after 25000 steps about 5% == 0.95
fun_high <- function(x){(1- exp(1)^(-v_high*x))}
fac_high <- fun_high(1)



#mid
v_mid <- 0.00006
(1- exp(1)^(-v_mid*(50000))) #should be after 50000 steps about 5%
fun_mid <- function(x){(1- exp(1)^(-v_mid*x))}
fac_mid <- fun_mid(1)

#low
v_low <- 0.00003
(1- exp(1)^(-v_low*(100000)))#should be after 100000 steps about 5%
fun_low <- function(x){(1- exp(1)^(-v_low*x))}
fac_low <- fun_low(1)


#list with all factors
fac_list <- list(fac_low, fac_mid, fac_high)
polymers <- c("low", "mid", "high")


#demonstrate the values
#loop per polymer
for (i in 1:length(polymers)) {
	polymer <- polymers[i]
	d[1, polymer] <- 1
	d[ , paste0(polymer, "_actual_pol")] <- 0
	d$length_s = (d$L_steps)/d$flow_velocity
	

	
	d$factor <- 1*(1-fac_list[[i]])^d$length_s #einbauen in negative Zinsfunktion 1(1-factor(1. Zeitschrit))^lengths in s
	#for loop as calculation
	act <- paste0(polymer, "_actual_pol")
	for (j in 1:nrow(d)) {
		d[2:nrow(d), act] <- ((d[1:I(nrow(d)-1), act] + d[1:I(nrow(d)-1), polymer]) * d$factor[1:I(nrow(d)-1)])
	}
	d[1, act]<-1

}

d
d_p <- d[ , paste0(polymers, "_actual_pol")]
names(d_p) <- polymers
d_p$L <- c(0, cumsum(d$L_steps[1:I(nrow(d)-1)]))

d_p2 <- melt(d_p, id.vars = "L")
names(d_p2) <- c("L", "category", "value")

nice <- theme_bw()+
	theme(legend.background = element_rect(fill = "transparent", color = NA),
				plot.background = element_rect(fill = "transparent",colour = NA),
				text = element_text(color = "black"),
				panel.grid = element_blank(),
				axis.text.x = element_text(color = "black", face = "plain", size = 10),
				axis.text.y = element_text(color = "black", face = "plain", size = 11),
				panel.background = element_rect(fill = "transparent")) #trbl


p1 <- ggplot(d_p2, aes(x = L/1000, y = value*100, color = category, linetype = category))+
	nice+
	labs(x = "river lengths in time steps (v = 1m/s)",
			y = "MaP in suspension\nin relation to start value [%]")+
	geom_vline(xintercept = 25)+
	geom_vline(xintercept = 50)+
	geom_vline(xintercept = 100)+
	geom_hline(yintercept = 5, linetype = "dashed")+
	scale_y_continuous(expand = c(0,0))+
	scale_x_continuous(expand = c(0,0))+
	geom_line()
	

p1
ggsave(paste0(main.path, "output_files/plots/plot_factor_river_MaP.png"), plot = p1, width = 12, height = 8, units = "cm", bg = "transparent", dpi = 500)

fac_rivers <- c(fac_low, fac_mid, fac_high)
names(fac_rivers) <- polymers
save(fac_rivers, file = paste0(main.path, "temp_data/factor_rivers_MaP.Rdata"))






# rm(list = ls())
