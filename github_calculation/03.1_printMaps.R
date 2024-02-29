############################
# script to read saved Rdata file and produce gpkg file which can be viewed in GIS software, also this scripts generates Rdata files which can be used for graphs
# please, adjust file accordingly to your input files -> see L44
# to save gpkg file: un-comment L158! saving a gpkg file take much more time than saving only Rdata files!
# make graphs
# unit: general in g / s
# rivers per m in g/ m (river section length)
# lakes per m in g / m2 
# author: David Mennekes, PhD Student at Empa, Switzerland, david.mennekes@empa.ch
# May 2021, last edited: May 2021
############################

	
	# packages
	library(sf)
	library(dplyr)

	# main path
	
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"
	
	#subfolder (change if needed)
	sub.path <- "temp_data/flow_files/"

	
	# define output folder
	output.gpkg <- paste0(main.path, "output_files/GIS/")
	
	#load geo
	load(paste0(main.path, sub.path, "0000_geo.Rdata"))
	
	# polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	

	# important: runs you want to save, default you will save all rounds you printed in the script 3.0
	load(paste0(main.path, "temp_data/flow_files/print_round.Rdata"))
	rounds <- print_round
	
	#extra names are the scenarios. will be loaded from Rdata. If you want only few scenarios, plase change extra_name
	extra_names <- extra_names
	# rounds <- c(801)
	
	#columns that should be included in final report beside  contamination columns
	nselection <- c("NAME", "isLake", "id_all", "flow_velocity", "flow_to", "outflow", "name_river")
	
	
	

	#files directionary
	rfiles_short <- list.files(path = paste0(main.path, sub.path))
	rfiles_long <- list.files(path = paste0(main.path, sub.path), full.names = T)


	
	# loop for extra names and all files: 
	for (extra_name in extra_names) {
			
		for(mat in polymers){
				load(paste0(main.path, sub.path, "round_801_", mat, extra_name, ".Rdata"))
				r.rivers <- which(rivers.calc$isLake == F & rivers.calc$outflow == 0) #find rivers
				r.lakes <- which(rivers.calc$isLake == T) #find lakes
				r.outflow <- which(rivers.calc$outflow != 2)
				lake_id <- id_lakes <- unique(rivers.calc$FID_poly_s)[-is.na(unique(rivers.calc$FID_poly_s))]
				
				n_old <- names(rivers.calc)
				#generate final columns for the data
				rivers.calc[ , paste0("water_perM_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("sediment_tot_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("sediment_perM_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("clean_tot_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("clean_perM_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("removal_tot_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("removal_perM_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("accumulation_tot_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("accumulation_perM_", mat, "_MaP")] <- 0
				rivers.calc[ , paste0("inputKawecki_perS_", mat, "_MaP")] <- rivers.calc[ , paste0("sum_",mat, "_Water_concMSV")]
				rivers.calc[ , paste0("acutalinflow_water_", mat, "_MaP")] <- rivers.calc[ , paste0("actualinflow_", mat, "_WaterMaP_concMSV")]
				rivers.calc[ , paste0("acutalinflow_sediment_", mat, "_MaP")] <- rivers.calc[ , paste0("actualinflow_sed_", mat, "_WaterMaP_concMSV")]
				n_new <- names(rivers.calc)
				n1 <- n_new[!(n_new %in% n_old)]
				
				
				#calculate new numbers for sections
				#weight in gramms
				#for rivers (perM in per m of the river section length)
				# lake section all have the length 1m therefore the numbers will not be changed and water_tot, accumulation.... etc are per second!!!
				rivers.calc[ , paste0("water_tot_", mat, "_MaP")] <- rivers.calc[ , paste0("actualcon_", mat, "_WaterMaP_concMSV")] *round(rivers.calc$length_m, 2)
				
				rivers.calc[ , paste0("water_perM_", mat, "_MaP")] <- rivers.calc[ , paste0("water_tot_", mat, "_MaP")] / rivers.calc$length_m
				
				#for sediments
				
				rivers.calc[ r.rivers, paste0("sediment_tot_", mat, "_MaP")] <- rivers.calc[ r.rivers, paste0("actualsed_", mat, "_WaterMaP_concMSV")]
				
				
				rivers.calc[ r.rivers, paste0("sediment_perM_", mat, "_MaP")] <- rivers.calc[ r.rivers, paste0("sediment_tot_", mat, "_MaP")] / rivers.calc$length_m[r.rivers]
				
				#accumulation
				rivers.calc[ , paste0("accumulation_tot_", mat, "_MaP")] <- rivers.calc[ , paste0("actualacc_", mat, "_WaterMaP_concMSV")]
				
				rivers.calc[ , paste0("accumulation_perM_", mat, "_MaP")] <- rivers.calc[ , paste0("accumulation_tot_", mat, "_MaP")] / rivers.calc$length_m
				
				#cleaning
				rivers.calc[ , paste0("clean_tot_", mat, "_MaP")] <- rivers.calc[ , paste0("actualclean_", mat, "_WaterMaP_concMSV")]
				
				rivers.calc[ , paste0("clean_perM_", mat, "_MaP")] <- rivers.calc[ , paste0("clean_tot_", mat, "_MaP")] / rivers.calc$length_m
				
				#removal
				rivers.calc[ , paste0("removal_tot_", mat, "_MaP")] <- rivers.calc[ , paste0("actualremoval_", mat, "_WaterMaP_concMSV")]
				
				#removal doesn´t exist perM -> is point value
				
				
				
				
				rivers.calc[r.lakes , paste0("inputKawecki_perS_", mat, "_MaP")] <- rivers.calc[r.lakes, paste0("sum_", mat, "_Water_concMSV")] / rivers.calc$area[r.lakes]
				
				
				## for all lakes... per lake ID if needed
				# all sections per lake will have the same value. this is better for interpretation
				
				
				lakes_single <- rivers.calc #jeder abschnitt bleibt unabhängig voneinander...
				for (id in lake_id) {
					x = which(rivers.calc$FID_poly_s == id)
					rivers.calc[x , paste0("water_tot_", mat, "_MaP")] <- sum(rivers.calc[x, paste0("actualcon_", mat, "_WaterMaP_concMSV")], na.rm = T) # nothing in sediments, because sediments = accumulation
					
					rivers.calc[x , paste0("water_perS_", mat, "_MaP")] <- sum(rivers.calc[x, paste0("actualcon_", mat, "_WaterMaP_concMSV")], na.rm = T)
					
					rivers.calc[x , paste0("water_perM_", mat, "_MaP")] <- rivers.calc[x , paste0("water_tot_", mat, "_MaP")] / rivers.calc$area[x]
					
					rivers.calc[x , paste0("sediment_tot_", mat, "_MaP")] <- 0
					rivers.calc[x , paste0("sediment_perM_", mat, "_MaP")] <- 0
					
					rivers.calc[x , paste0("accumulation_tot_", mat, "_MaP")] <- sum(rivers.calc[x , paste0("actualacc_", mat, "_WaterMaP_concMSV")])
					rivers.calc[x , paste0("accumulation_perM_", mat, "_MaP")] <- rivers.calc[x , paste0("accumulation_tot_", mat, "_MaP")]/rivers.calc$area[x]
					
					rivers.calc[x , paste0("clean_tot_", mat, "_MaP")] <- sum(rivers.calc[x , paste0("actualclean_", mat, "_WaterMaP_concMSV")])
					rivers.calc[x , paste0("clean_perM_", mat, "_MaP")] <- rivers.calc[x , paste0("clean_tot_", mat, "_MaP")]/rivers.calc$area[x]
					#removal effects river not lakes!
				}
				
				
				n2 <- c(nselection, n1) #make vector with all col names which should be selected
				geo_x <- geo
				geo_x[ , n2] <- NA
				geo_x[ , n2] <- rivers.calc[ , n2]
				s_r <- rivers.calc[ , n2]
				s_r2 <- lakes_single[ , n2]
				Sys.sleep(8)
				# st_write(geo_x, paste0(output.gpkg, "cont_", mat, "_round_801", extra_name, ".gpkg" ), append = F)
				save(s_r, file = paste0(main.path, "output_files/rdata/cont_",mat,"_801", extra_name, ".Rdata"))
				Sys.sleep(1)
				save(s_r2, file = paste0(main.path, "output_files/rdata/lakes_single/cont_",mat,"_801", extra_name, ".Rdata"))
				Sys.sleep(8)
				print(paste0(extra_name, " ", mat, " done! (", Sys.time(), ")"))
		}
	}
	

	rm(list = ls())
	