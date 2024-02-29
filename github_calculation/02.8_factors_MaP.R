#######
# add accumulation and removal factors to the data set. Partly factors are calculated here
# calculation of scenarios described in the manuscript. the names of the scenario are written as header

# author: david mennekes,  david.mennekes@empa.ch
# march 2022
#################




###############################


#packages and path
#packages and path
###############################

	library(tidyverse)
	library(sf)
	
###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
###############################
	# scenario base ####
###############################

###############################
# load data
###############################

	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low

	fac_rivers["zero"] <- 0 #add a value for no change
	
	


#############
# define factors
#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.75000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])

	
		
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)

	rivers.all6[r.lakes, c.sed] <- 0.95000000000000001 #lakes = 95%
	
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	##### cleaning####
	c.clean <- grep("clean.fac_", c.names)
	rivers.all6[ , c.clean] <- 0.20000000000000001
	
	
	#### resus ##
	c.resus <- grep("resus.fac_", c.names)
	rivers.all6[r.rivers, c.resus] <- 0.20000000000000001 #for all rivers 20%
	rivers.all6[r.lakes, c.resus] <- 0 #for all lakes 0%; is included in sedimentation
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	#safe file####
	extra_name <- "base"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
			
			

	rm(list = ls())	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario null		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "null"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario weir05			####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.05000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	
	#safe file####
	extra_name <- "weir05"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario weir 25			####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.25000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	
	#safe file####
	extra_name <- "weir25"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario weir 50			####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.50000000000000001 #for all dams assume a removal rate of 50% accross all polymers
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "weir50"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario weir 75			####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.75000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "weir75"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario weir 95			####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.95000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "weir95"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario baseLakesLinear		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	a <- 0.95/600000000 #incline of linear function
	rivers.all6[r.lakes, c.sed] <- rivers.all6$area[r.lakes]*a
	
	#5% min water contamination (max sed = 0.95)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	

	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "baseLakesLinear"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario baseLakes05		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	rivers.all6[r.lakes, c.sed] <- 0.05
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "baseLakes05"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario baseLakes50		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	rivers.all6[r.lakes, c.sed] <- 0.5
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "baseLakes50"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario baseLakes95		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	rivers.all6[r.lakes, c.sed] <- 0.95000000000000001
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "baseLakes95"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	##############################################################
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario baseRivers		####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.75000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	#use df01 for rivers in river.all6 data
	df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	rivers.all6[r.lakes, c.sed] <- 0 #sed for lakes = 0
	
	##### cleaning####
	c.clean <- grep("clean.fac_", c.names)
	rivers.all6[ , c.clean] <- 0.20000000000000001
	rivers.all6[r.lakes, c.clean] <- 0
	
	
	#### resus ##
	c.resus <- grep("resus.fac_", c.names)
	rivers.all6[r.rivers, c.resus] <- 0.20000000000000001 #for all rivers 20%
	rivers.all6[r.lakes, c.resus] <- 0 #for all lakes 0%; is included in sedimentation
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	#safe file####
	extra_name <- "baseRivers"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	##############################################################
	
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	##### scenario baseRiversnoResus   ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.75000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	#linear equations
	
	rivers.all6[r.lakes, c.sed] <- 0 #sed for lakes = 0
	
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	##### cleaning####
	c.clean <- grep("clean.fac_", c.names)
	rivers.all6[ , c.clean] <- 0.20000000000000001
	rivers.all6[r.lakes, c.clean] <- 0
	
	
	#### resus ##
	c.resus <- grep("resus.fac_", c.names)
	rivers.all6[r.rivers, c.resus] <- 0 #for all rivers 20%
	rivers.all6[r.lakes, c.resus] <- 0 #for all lakes 0%; is included in sedimentation
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	
	#safe file####
	extra_name <- "baseRiversnoResus"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario LUlow           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	

	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- df01$fac_sed_landuse
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- (1-(1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*0.5
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	#safe file####
	extra_name <- "LUlow"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario LUmid           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_landuse)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "LUmid"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario LUhigh           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_landuse)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*2
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "LUhigh"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Qlow           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- (1-exp(1)^(-df01$x*1)) #for low discharge
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	

	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_discharge)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*0.5
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Qlow"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Qmid           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	

	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_discharge)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Qmid"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Qhigh           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- (1-exp(1)^(-df01$x*1))
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_discharge)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*2
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Qhigh"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Slow           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	

	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*0.5
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Slow"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Smid           ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	

	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Smid"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario Shigh          ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	#use df01 for rivers in river.all6 data
	# df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	df01$sum <- 1-(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	
	#manipulate:
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed])*2
	Sys.sleep(3)
	
	#5% min water contamination
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	
	
	
	
	#safe file####
	extra_name <- "Shigh"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario low_all ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.05000000000000001 #for all dams assume a removal rate of 75% accross all polymers
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	#manipulate
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed]*0.5)
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	
	rivers.all6[r.lakes, c.sed] <- 0.05000000000000001 #lakes = 5%
	
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	##### cleaning####
	c.clean <- grep("clean.fac_", c.names)
	rivers.all6[ , c.clean] <- 0.0
	
	
	#### resus ##
	c.resus <- grep("resus.fac_", c.names)
	rivers.all6[r.rivers, c.resus] <- 0.20000000000000001 #for all rivers 20%
	rivers.all6[r.lakes, c.resus] <- 0 #for all lakes 0%; is included in sedimentation
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	#safe file####
	extra_name <- "low_all"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
	
	
	
	
	
	
	###################################################################
	#path to sub-folders
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	# 
	# 
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	###############################
	# scenario high_all ####
	###############################
	
	###############################
	# load data
	###############################
	
	# # load river data
	load(paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rivers.all6 <- rivers.all5
	rm(rivers.all5)
	
	#load factors for river
	load(paste0(main.path, "temp_data/factor_rivers_MaP.Rdata")) #high middle low
	
	fac_rivers["zero"] <- 0 #add a value for no change
	
	
	
	
	#############
	# define factors
	#############
	
	
	# add all data to rivers.calc
	
	for (i in polymers) {
		a <- as.data.frame(matrix(0, nrow = nrow(rivers.all6), ncol = 5))
		names(a) <- c(paste0("sed.fac_", i, "_MaP"), 
									paste0("removal.fac_", i, "_MaP"),
									paste0("acc.fac_", i, "_MaP"),
									paste0("clean.fac_", i, "_MaP"),
									paste0("resus.fac_", i, "_MaP"))
		
		rivers.all6 <- cbind(rivers.all6, a)
		rm(a)
	}
	
	c.names <- names(rivers.all6)
	r.rivers <- which(rivers.all6$isLake==F & rivers.all6$outflow == 0)	
	
	##### removal #####
	c.removal <- grep("removal.fac_", c.names)
	rivers.all6[rivers.all6$isDam==T, c.removal] <- 0.95000000000000001 #for all dams assume a 
	
	
	
	##### sedimentation######
	#------------------#
	
	c.sed <- grep("sed.fac_", c.names) #columns with name sedimentation
	
	
	### rivers ###
	#create dataframe to calculate factor
	df01 <- st_drop_geometry(rivers.all6 %>% select(id_all,discharge, isLake, isDam, forest, rocks, unknown, urban, agriculture, glacier, water, meandering_factor))
	
	
	# for rivers ###
	#create factors for each category for sedimentation: landuse, sinuosity, discharge
	
	df01$fac_sed_landuse <- 0
	df01$fac_sed_sinuosity <- 0
	df01$fac_sed_discharge <- 0
	
	# for landuse:
	#apply the landuse factors high, middle, low according to the groupping of landuse
	# forest, agriculture -> high
	# rocks, unknown (grassland) -> mid
	# urban -> low
	# water, glacier -> zero (=0)
	
	df01$fac_sed_landuse <- df01$forest*fac_rivers["high"]+df01$agriculture*fac_rivers["high"]+
		df01$rocks*fac_rivers["mid"]+df01$unknown*fac_rivers["mid"]+
		df01$urban*fac_rivers["low"]+
		df01$water*fac_rivers["zero"]+df01$glacier*fac_rivers["zero"]
	df01$fac_sed_landuse[is.na(df01$fac_sed_landuse)] <- fac_rivers["zero"] #replace NA with zero value
	
	# for discharge
	# 1.) find theoretical max distance dmax
	# case different equation for discharge below = 0.3 and over 0.3
	df01$dmax[df01$discharge<= 0.3] <- 3333*df01$discharge[df01$discharge<= 0.3] #for discharge <= 0.3
	df01$dmax[df01$discharge> 0.3] <- 330*df01$discharge[df01$discharge> 0.3]+901 #for discharge >0.3
	
	#2. use dmax to calculate factor x in the for one step (x = log(0.05)/ dmax)
	df01$x <- -(log(0.05) / df01$dmax)
	
	#3.) find fac for 1 time step -> f(x) = 1-exp(1)^(-x * 1)
	df01$fac_sed_discharge <- 1-exp(1)^(-df01$x*1)
	
	
	# for sinuosity
	# apply equation based on Newbould 2021
	df01$fac_sed_sinuosity <- (1-(1/(df01$meandering_factor)^0.3))*0.1
	
	
	
	#use df01 for rivers in river.all6 data
	df01$sum <- 1-(1-df01$fac_sed_landuse)*(1-df01$fac_sed_discharge)*(1-df01$fac_sed_sinuosity)
	# summary(df01)
	
	# bring numbers to rivers.all6 and apply neg. compound interest eq.
	#for rivers only!
	rivers.all6[r.rivers, c.sed] <- ((1 - df01$sum[r.rivers])^rivers.all6$length_m[r.rivers])
	
	#manipulate
	rivers.all6[r.rivers, c.sed] <- st_drop_geometry(rivers.all6[r.rivers, c.sed]*2)#double all values
	
	
	
	### lakes ###
	r.lakes <- which(rivers.all6$isLake == T)
	
	rivers.all6[r.lakes, c.sed] <- 0.95000000000000001 #lakes = 95%
	
	
	#5% min water contamination (max sed = 0.95000000000000001)
	for (i in c.sed) {
		over95 <- which(st_drop_geometry(rivers.all6[ , i]) > 0.95000000000000001)
		if(length(over95) > 0){
			rivers.all6[over95, i] <- 0.95000000000000001
		}
		
	}
	
	
	##### cleaning####
	c.clean <- grep("clean.fac_", c.names)
	rivers.all6[ , c.clean] <- 0.0
	
	
	
	#### resus ##
	c.resus <- grep("resus.fac_", c.names)
	rivers.all6[r.rivers, c.resus] <- 0.20000000000000001 #for all rivers 20%
	rivers.all6[r.lakes, c.resus] <- 0 #for all lakes 0%; is included in sedimentation
	
	
	
	#### accumulation ###
	c.acc <- grep("acc.fac_", c.names)
	# rivers.all6[rivers.all6$outflow==0,c.acc] <- 1- st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.clean]) - st_drop_geometry(rivers.all6[rivers.all6$outflow==0,c.resus]) # the rest will be accumulation, causes problems of rounding
	rivers.all6[rivers.all6$outflow==0,c.acc] <- 1 #can be as high as 1 means that the entire rest will be accumulated.
	
	#warning if sed. factors are 1 or higher
	if(length(which(rivers.all6$sed.fac_PET_MaP >= 1)) >=1){
		warning("sedimentation factors are 1 or higher!")
		break
	}
	
	
	#safe file####
	extra_name <- "high_all"
	c.numeric <- names(select_if(st_drop_geometry(rivers.all6), is.numeric))
	rivers.all6[ ,c.numeric] <- round(st_drop_geometry(rivers.all6[ , c.numeric]), digits = 10) #round numbers to avoid problems
	save(rivers.all6,extra_name, file = paste0(main.path, "temp_data/rivers_all6_", extra_name, ".Rdata"))
	
	
	
	rm(list = ls())	
	
