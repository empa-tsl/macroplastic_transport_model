######################
# add data of river / lake contamination and run model
# unit: g/ s
# author: david mennekes, PhD Student at Empa St. Gallen / ETH Zürich, Switzerland, david.mennekes@empa.ch
# march 2023, last edit:
######################


	###############################
	#packages and path
	###############################

	library(tidyverse)
	library(sf)
	#path to sub-folders
	# change this path to the directory of the model
	setwd("~/")
	main.path <- "PhD/mennekes2.0/" #change!
	#
	#
	#polymers of interest
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	#important non polymer specific columns that should be saved:
	#adjust according to your desires
	save_c <- c("id_all", "flow_to", "outflow", "height_last", "name_river", "slope_percent", "length_m", "length_seconds", "flow_velocity", "isLake", "country", "area", "name_2", "FID_poly_s", "OBJEKTART", "NAME", "VERLAUF", "GEWISS_NR", "LAUF_NR")
	
	#scenarios defined in 2.7_factors. Each factor represents a different parameter set-up
	# extra_names <- c( "_base", "_null", "_weir25", "_weir50", "_weir75")
	extra_names <- c("_null", "_weir05", "_weir25", "_weir50", "_weir75", "_weir95", "_base", "_baseLakesLinear","_baseLakes05","_baseLakes50","_baseLakes95", "_baseRivers", "_baseRiversnoResus", "_LUlow", "_LUmid", "_LUhigh", "_Qlow", "_Qmid", "_Qhigh", "_Slow", "_Smid", "_Shigh", "_low_all", "_high_all")
	
	# extra_names <- c("_base")
	
	N <- 801 # number of runs. should be high enough to establish a constant, constant with about 600 for Rhine
	print_round <- c(801) #rounds which will be saved, should be at least the last round

	###############################
	# functions
	###############################


	#loop for extra_names
	# run the model for each scenario / parameter set-up
	for (xxx in extra_names) {
		

	###############################
	# load data
	###############################

	# # load river data saved in 2.7_factors
	load(paste0(main.path, "temp_data/rivers_all6", xxx, ".Rdata"))

	rivers.calc <- st_drop_geometry(rivers.all6) #drop geometry for faster calculation / change this if you have a file without geometry in the first time
	
	#save geometry in a different file
	geo <- rivers.all6
	geo <- rivers.all6[ , "id_all"]
	names(geo)[1] <- "id_all_geo"

	save(geo, file = paste0(main.path, "temp_data/flow_files/0000_geo.Rdata")) #geometry file without calculations

	rm(geo) #remove file
	rm(rivers.all6) #remove file




	##############
	#add data for actual contamination: sum data is yearly flow in to the environment calculated by Delphine in contamination per s and river m for rivers and total for lakes. Mass in g
	##############

	for(mat in polymers){
		rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")] <- 0
	}

	for(mat in polymers){
		rivers.calc[ , paste0("actualinflow_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	for(mat in polymers){
		rivers.calc[ , paste0("actualinflow_sed_", mat, "_WaterMaP_concMSV")] <- 0
	}
	

	# for actual accumulation
	for(mat in polymers){
		rivers.calc[ , paste0("actualacc_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	#for actual cleaning
	for(mat in polymers){
		rivers.calc[ , paste0("actualclean_", mat, "_WaterMaP_concMSV")] <- 0
	}

	#for actual removal
	for(mat in polymers){
		rivers.calc[ , paste0("actualremoval_", mat, "_WaterMaP_concMSV")] <- 0
	}

	#actual concentration in sedimentation
	for(mat in polymers){
		rivers.calc[ , paste0("actualsed_", mat, "_WaterMaP_concMSV")] <- 0
	}
	
	#for actual resus
	for(mat in polymers){
		rivers.calc[ , paste0("actualresus", mat, "_WaterMaP_concMSV")] <- 0
	}


	# find rows which are no outflow and lakes or rivers

	r.rivers <- which(rivers.calc$isLake == F & rivers.calc$outflow == 0)
	# r.rivers.l <- rivers.calc$isLake == F & rivers.calc$outflow == 0 #for lakes

	r.lakes <- which(rivers.calc$isLake == T)




	#find duplicated numbers, like this the calculation will be faster
	dup <- unique(rivers.calc$flow_to[duplicated(rivers.calc$flow_to)])
	multi_flow_to <- rivers.calc$flow_to %in% dup
	dup_id <- rivers.calc$id_al[multi_flow_to]
	single_flow_to <- !(multi_flow_to)

	
	rivers.calc.all <- rivers.calc #transfer data to new data with selected columns

	for (mat in polymers) { #loop for each polymer. 
		#create name vectors for each polymer
		#create names sum (yearly outflowin MSV) for macroplastics -> without the ending "MP"
		MaPw.sum <- paste0("sum_", mat, "_Water_concMSV")

		#inflow from previous section
		MaPw.in <- paste0("actualinflow_", mat, "_WaterMaP_concMSV")
		
		
		# sediment inflow from previous section
		MaPw.in_sed <- paste0("actualinflow_sed_", mat, "_WaterMaP_concMSV")



		#create names accumulation
		MaPw.acc <- paste0("actualacc_", mat, "_WaterMaP_concMSV")

		#create names removal
		MaPw.removal <- paste0("actualremoval_", mat, "_WaterMaP_concMSV")


		MaPw.clean <- paste0("actualclean_", mat, "_WaterMaP_concMSV")
		
		#create names sedimentation
		MaPw.sed <- paste0("actualsed_", mat, "_WaterMaP_concMSV")
		
		MaPw.resus <- paste0("actualresus_", mat, "_WaterMaP_concMSV")
		
		#select only columns that are needed
		c.mat <- grep(paste0(mat, "_"), names(rivers.calc.all))
		rivers.calc <- rivers.calc.all[, c(save_c, names(rivers.calc.all)[c.mat])]


		for (i in 1:N) { #loop for each calculation step
			# print(i)
			# create empty container for data
			temp_MaP <- rep(0, nrow(rivers.calc)) #MaP in suspension
			
			temp_MaP_sed <- rep(0, nrow(rivers.calc)) #MaP in sediment
			
			
			# important: the order of calculation is important!
			# first removal (inflow*removal + input * removal)
			MaP <- tibble(removal = ((rivers.calc[ , MaPw.sum])  * (rivers.calc[ , paste0("removal.fac_", mat, "_MaP")]) + (rivers.calc[ , MaPw.in]) * (rivers.calc[ , paste0("removal.fac_", mat, "_MaP")]))*-1)
			
			# second sedimentation (inflow + input in segment - removal) (removal is neg. number, thus +)
			MaP$sed_wo_sedinflow <- (((rivers.calc[ , MaPw.sum] + rivers.calc[ , MaPw.in] + MaP$removal) * rivers.calc[ , paste0("sed.fac_", mat, "_MaP")]) ) *-1
			MaP$sed <- MaP$sed_wo_sedinflow - rivers.calc[ ,MaPw.in_sed]# add (substract because of neg. number) inflowing sediments
			
			
			# third cleaning resuspension and burial / accumulation (all together must be smaller than 1)
			# calculated per segment
			#based on sedimented portion plus actual sediments (from inflow)
			# MaP$sed * -1 because sed is negative number but we need positive numbers.
			MaP$sed_pos <- MaP$sed*-1
			
			
			MaP$clean <- (MaP$sed_pos) * rivers.calc[ , paste0("clean.fac_", mat, "_MaP")]*-1
			
			#  resuspension
			MaP$resus <- (MaP$sed_pos) * rivers.calc[ , paste0("resus.fac_", mat, "_MaP")]
			
			#accumulation
			MaP$burial <- (MaP$sed_pos) * rivers.calc[ , paste0("acc.fac_", mat, "_MaP")]*-1

			MaP <- MaP*1e15 #to avoid rounding errors
			#cut of numbers at position 15 -> to avoid rounding errors.
			# for neg. numbers use ceiling for pos. numbers use floor
			MaP[, c("removal", "sed_wo_sedinflow", "sed", "clean", "burial")] <- ceiling(MaP[, c("removal", "sed_wo_sedinflow", "sed", "clean", "burial")])
			MaP[ , c("sed_pos", "resus")] <- floor(MaP[ , c("sed_pos", "resus")])
			
			# accumulation cant be higher than the rest after clean and resuspension
			w1 <- MaP$clean*-1 + MaP$resus + MaP$burial*-1
			w2 <- which(w1 > MaP$sed_pos)
			#sum w2
			w2_sum <- MaP$clean[w2]*-1 + MaP$resus[w2]
			if(length(w2)>0){
				MaP$burial[w2]<- (MaP$sed_pos[w2] - w2_sum)*-1 #burial is mass in sed - clean - resus (! clean is a negative number!), floor and ceiling avoid rounding problems. number might leave very small rest. can be passed on via sed transport...
			}
			
			#futher information:
			MaP$sum <- rivers.calc[ , MaPw.sum]*1e15
			MaP$inflow <- rivers.calc[ , MaPw.in]*1e15
			


			
			
			
			
			# summary(MaP)
			########## ################
			#sum the MaP dataframes per row ->negative numbers are subtracted (sedimentation, burial) while positive number are added (input emission, resuspension, inflow from upstream)
			#assumptions: inflow is per second and all other data is for the entire segment. therefore all "changes" along the segment are included. Thus the result is per second at the end of the segment
			
			# to reduce computation time we separate multiflow vs. single flow
			#if only one section flows to another section:
			# just sum the rows of removal, burial, sum and act. conc. -> because of the negative numbers of sed_wo_sedinflow and removal this is a easy solution to find the new act.conc. in the water
			temp_MaP[rivers.calc$flow_to[single_flow_to]] <- rowSums(MaP[single_flow_to, c("inflow", "sum", "removal", "sed_wo_sedinflow", "resus")], na.rm = T) # because sed and clean up are stored as neg. numbers this amount will be substrected.resuspension, sum (data by Kawecki) and inflow are positive numbers
			
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MaP[sort(dup)] <- rowSums(rowsum(MaP[multi_flow_to, c("inflow", "sum", "removal", "sed_wo_sedinflow", "resus")], rivers.calc$flow_to[multi_flow_to])) #sorts always according to second argument
			
			
			#same for sediments
			#pass on sedimentation..  sed_pos (including sed_inflow) are positive numbers. burial, resus_neg and cleaning are negative numbers
			MaP$resus_neg <- MaP$resus*-1
			temp_MaP_sed[rivers.calc$flow_to[single_flow_to]] <- rowSums(MaP[single_flow_to, c("burial", "sed_pos", "resus_neg", "clean")], na.rm = T)
			
			
			# for summing in groups / meaning multiple flows go into one, rowsum takes a grouping argument
			temp_MaP_sed[sort(dup)] <- rowSums(rowsum(MaP[multi_flow_to, c("burial","sed_pos", "resus_neg", "clean")], rivers.calc$flow_to[multi_flow_to], na.rm = T))
			
			#back transfer temp_MaP
			temp_MaP <- temp_MaP*1e-15
			temp_MaP_sed <- temp_MaP_sed*1e-15
			
			

			#eliminate too small numbers
			temp_MaP[temp_MaP > -1e-18 & temp_MaP < 1e-18] <- 0
			temp_MaP_sed[temp_MaP_sed > -1e-18 & temp_MaP_sed < 1e-18] <- 0
			
			
			
			# use the temp_data as new inflow concentration data.
			rivers.calc[ ,MaPw.in] <- temp_MaP 
			
			#use the temp_data of sed. for sed inflow
			rivers.calc[ , MaPw.in_sed] <- temp_MaP_sed
			
			#controll that no negative accumulation. possible when cleaning + accumulation is higher than "inflow"
			if(length(which((rivers.calc[ ,MaPw.in]) < 0)) >=1 ){
				warning(paste0("negative contamination in water (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				
				break
			}
			
			
			if(length(which(MaP$burial > 0)) >=1 ){
				warning(paste0("negative contamination in accumulation (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				
				break
			}
			if(length(which((rivers.calc[ ,MaPw.in_sed]) < 0)) >=1 ){
				warning(paste0("negative contamination in sediments (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				break
			}
			
			# actual concentration of plastic in the water
			rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")] <- rowSums(MaP[ , c("inflow", "sum", "removal", "sed_wo_sedinflow")], na.rm = T) #inflow contains resus from previous segment
			rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")] <- rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")]*1e-15 #back transfer
			
			
			
			
			if(length(which((rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")]) < 0)) >=1 ){
				warning(paste0("negative contamination in water (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				
				break
			}
			
			#back transfer MaP
			MaP <- MaP*1e-15
			
			#transfer data to rivers.calc dataframe
			rivers.calc[ , MaPw.acc] <- MaP[ , "burial"]*-1 #neg numbers were needed before
			
			
			rivers.calc[ , MaPw.removal] <- MaP[ , "removal"]*-1 #neg numbers were needed before
			
			rivers.calc[ , MaPw.clean] <- MaP[ , "clean"]*-1 #neg numbers were needed before
			
			#for sed: what you will find / is present in the entire river segment!!! in one second
			rivers.calc[ , MaPw.sed] <- MaP[ , "sed_pos"] #sed_pos is a positive numbers
			
			
			if(length(which((rivers.calc[ , MaPw.removal]) < 0)) >=1 ){
				warning(paste0("negative removal in water (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				break
			}
			
			if(length(which((rivers.calc[ , MaPw.acc]) < 0)) >=1 ){
				warning(paste0("negative accumulation in water (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				break
			}
			
			if(length(which((rivers.calc[ , MaPw.clean]) < 0)) >=1 ){
				warning(paste0("negative cleaning from water (MaP)! polymer: ", mat, "; round: ", i, "; scenario: ", xxx))
				break
			}
			
			
			
			

			if(i %in% print_round){
				Sys.sleep(10)
				save(rivers.calc, file = paste0(main.path, "temp_data/flow_files/round_", i, "_", mat,"_",  extra_name, ".Rdata"))
				print(paste(Sys.time(), ":", mat, extra_name, "done!"))
				Sys.sleep(5)
			}
		}
	}



	} #end loop extra_names

	save(print_round,extra_names, file = paste0(main.path, "temp_data/flow_files/print_round.Rdata"))
	rm(list = ls())

