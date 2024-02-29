######################
# collect data of river and lake contamination, prepare data for simulation
# main goal: find the connection of the river segments. which is the following river segment downstream? This informatin will stored as "flow_to" and corresponds to the "id_all" which is equal to the row number. This script is not needed if you have a frame with all data connected..

# author: david mennekes, PhD Student at Empa ST. Galen / ETH Zürich, Switzerland, david.mennekes@empa.ch
# november 2020, last edit: march 2021
######################

# IMPORTANT # IMPORTANT #
# this script uses a package from github. likely this package will change in future or will be available on CRAN. look for further information on github!
# for installation:
# remotes::install_github("paleolimbot/qgisprocess")
# works with QGIS 3.16. and newer, however QGIS needs to be installed as well
# packages: View(qgis_algorithms())
# help: qgis_show_help("native:FUNKTION_YOU_NEED_HELP")



	###############################
	#packages and path
	###############################
	
	#path to sub-folders
	#add your own path
	setwd("~/")
	main.path <- "PhD/mennekes/"
	
	
	#polymers
	polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")
	
	
	library(tidyverse)
	library(sf)
	library(qgisprocess)
	library(tmap)
	library(raster)

	###############################
	# functions
	###############################
	
	# change output from qgis to simple features

	qgis_to_sf <- function(input){
		return(sf::st_as_sf(sf::read_sf(qgis_output(input, "OUTPUT"))))
	}
	
	
	# rename geometry column. as standard use "geometry"
	rename_geometry <- function(g, name = "geometry"){
		current = attr(g, "sf_column")
		names(g)[names(g)==current] = name
		st_geometry(g)=name
		g
	}
	

	###############################
	# load data
	###############################

	# load river and lake data produced by Kawecki
	#unit: for rivers: emission kg / km river length
	#			 for lakes: emission kg / ha lake

	load(paste0(main.path, "data_input_kawecki/Data_Summed/Water_summed_dm.Rdata"))
	# emission for rivers in kg / km
	# emission for lakes in kg / ha
	

	
	#load river	polylines (data by toposwiss)
	load(paste0(main.path, "data_raw/maps/rivers/rivers_updated.Rdata")) # CRS: CH1903+ /LV95

	#load lakes (generated from tlm_fliesgewaesser.shp)
	load(paste0(main.path, "data_raw/maps/lakes/lakes_poly.Rdata"))
	
	#check for CRS
	if(length(unique(c(st_crs(tlm.river), st_crs(lakes_emi),  st_crs(lakes_poly), st_crs(rivers_emi)))[[1]]) != 1){warning("CRS are different! Please change!")}else{"all CRS are equal."} #CRS need to be the same!
	
	# the dataset for switzerland contains different river types stored in OBJEKTART. not all of them were used.
	# s Bisse Suone (0), pressure tubes of powerstations (1, 2 & 3), temporal rivers (7), rivers (4)
	# lake connections (6) are important for further calculations!
	# use only rivers enbeded in to the river systems (enabled == 1)
	
	tlm.river <- tlm.river %>% filter(OBJEKTART %in% c(4,6), ENABLED == 1) 
	
	
	# analyse river and lake specific columns for later selection
	
	cr <- names(rivers_emi) #all col names river
	cl <- names(lakes_emi) # all col. names lakes
	col.lakes <- cl[!(cl %in% cr)] #unique col names lakes
	col.river <- cr[!(cr %in% cl)] # unique col names rivers
	
	
	j <- NULL #collect positions in cl
	for (i in polymers) {
		k <- which(grepl(i, cl))
		j <- c(j, k)
	}
	col.emi.lakes <- cl[unique(j)] #col names with emission for lakes
	
	j <- NULL #collect positions in cr
	for (i in polymers) {
		k <- which(grepl(i, cr))
		j <- c(j, k)
	}
	col.emi.rivers <- cr[unique(j)] #col names with emission for rivers
	rm(j)
	
	
	#########################
	#  calculation
	#########################
	
	rivers.in.lakes <- rivers46_cast[rivers46_cast$OBJEKTART == 6, ] #lakes are where objektar == 6
	rivers.in.lakes$Lake_id <- 1:nrow(rivers.in.lakes)
	underground.rivers <- rivers46_cast[rivers46_cast$OBJEKTART == 4 & rivers46_cast$VERLAUF %in% c(200, 300), ] #rivers which are missing in the rivers_emi because they are underground
	
	
	###########################	
	# 1.) get emission information from lake polygon via join
	###########################
	# the output emissions by Kawecki are given in polygons for the lakes. therefore emission informations from lake polygons are transferred to the polylines. 
	
	#join by FID_poly_s
	lakes_con <- as.data.frame(st_drop_geometry(lakes_emi[c(col.emi.lakes, "FID_poly_s")])) #write relevant data in dataframe, especially contamination
	rivers6 <- merge(rivers.in.lakes, lakes_con, by = "FID_poly_s") #merge contamination data with rivers.in.lakes by FID_poly_s an ID given by the polygones in the first time.

	
	
	
	
	
	
	##################
	# 2.) bring joined rivers and rivers emi together
	##########################
	
	
	# b) make dataframe for all
	###### 

	names.all <- unique(c(names(rivers_emi), names(rivers6), names(underground.rivers))) #all names existing
	length(names.all)

	setdiff(names.all, names(rivers_emi))

	
	# find columns that are missing in other df and fill these columns with NA
	##########
	rivers_emi[, setdiff(names.all, names(rivers_emi))] <- NA #make rows with NA when missing compared to other sf
	rivers6[ ,setdiff(names.all, names(rivers6))] <- NA
	underground.rivers[ , setdiff(names.all, names(underground.rivers))] <- NA
	
	
	
	# bind all data together
	rivers6 <- st_transform(rivers6, st_crs(rivers_emi))
	underground.rivers <- st_transform(underground.rivers, st_crs(rivers_emi))
	
	rivers.all <- rbind(rivers_emi, rivers6[ , names(rivers_emi)],underground.rivers[ , names(rivers_emi)]) # rbind, while using order of rivers_emi
	
	
	save(rivers.all, file = paste0(main.path, "temp_data/rivers_all00.Rdata"))
	
	# st_write(rivers.all, paste0(main.path, "temp_data/rivers_all.gpkg"), append = F)
	
	
	
	
	###########
	# 4.) make river connections
	###########
	# connection are the most important. to see where the river flows to
	# the principle is, that each last vertex of a linefeature is associated with the closest first vertex of another linefeature. 
	
	load(paste0(main.path, "temp_data/rivers_all00.Rdata"))
	
	
	# drop multilinestring to linestring for further process
	rivers.all <- st_cast(rivers.all, "LINESTRING") #only non important rivers section contain actually "mulilinestrings" which cause an error
	
	nuller <- which(as.numeric(st_length(rivers.all)) < 0.1) #in meter, many section have length = 0
	rivers.all <- rivers.all[-nuller, ]
	
	#### get the ID of the river section which follows downstream.. first make ID
	# make new cols: 1) id_all; 2) flow_to: in which id_all flows id_all n-1 
	rivers.all$id_all <- 1:nrow(rivers.all) #important: id_all = row number
	
	
	# find first and last point of river linestring. Water flows from first to last point (s. data description)
	first <- data.frame(f = st_line_sample(rivers.all, sample = 0), #sample = 0 for first point
											n = rivers.all$id_all)# first
	first <- st_as_sf(first)
	
	last <- data.frame(l = st_line_sample(rivers.all, sample = 1),
										 n = rivers.all$id_all)#last
	last <- st_as_sf(last)
	
	#should be same length. for each element one number
	if(nrow(first) != nrow(last) | nrow(first) != nrow(rivers.all)){
		warning("something went wrong!")
		break
	}
	
	#######
	#a ) find nearest first to last. Do this muliple times in case more than one option would be possible
	
	
	
	######
	#find nearest feature
	nrst_a <- st_nearest_feature(last, first) #find nearest point from last to a first point which is a river or lake
	dist_a <- as.numeric(st_distance(first[nrst_a, ], last, by_element = T)) #distances between both points
	first_s_a <- first[nrst_a, ]
	f_a <- first_s_a$n[which(dist_a < 0.1)]# get all id of features that were closer than 0.1cm
	l_a <- last$n[which(dist_a < 0.1)]
	# test <- st_distance(first[f_a, ], last[l_a, ], by_element = T)
	
	
	#do it again... to find a second possible option
	first_b <- first[-unique(f_a), ]
	nrst_b <- st_nearest_feature(last, first_b) #find nearest point when nrst01_a is not allowed
	dist_b <- as.numeric(st_distance(first_b[nrst_b, ], last, by_element = T))
	first_s_b <- first_b[nrst_b, ]
	f_b <- first_s_b$n[which(dist_b < 0.1)]
	l_b <- last$n[which(dist_b < 0.1)]
	
	
	#do it again... to find a second possible option
	first_c <- first[-unique(c(f_b, f_a)), ]
	nrst_c <- st_nearest_feature(last, first_c) #find nearest point when nrst01_a is not allowed
	dist_c <- as.numeric(st_distance(first_c[nrst_c, ], last, by_element = T))
	first_s_c <- first_c[nrst_c, ]
	f_c <- first_s_c$n[which(dist_c < 0.1)]
	l_c <- last$n[which(dist_c < 0.1)]
	
	#do it again... to find a second possible option
	first_d <- first[-unique(c(f_b, f_a, f_c)), ]
	nrst_d <- st_nearest_feature(last, first_d) #find nearest point when nrst01_a is not allowed
	dist_d <- as.numeric(st_distance(first_d[nrst_d, ], last, by_element = T))
	first_s_d <- first_c[nrst_d, ]
	f_d <- first_s_d$n[which(dist_d < 0.1)]
	l_d <- last$n[which(dist_d < 0.1)]
	
	
	df_flow_to <- data.frame(id = last$n,
													 option_a = NA,
													 option_b = NA,
													 option_c = NA,
													 option2_a = NA,
													 option2_b = NA,
													 option2_c = NA)
	
	#check for same GEWISS_NR as first creteria
	gew_eq1_a <- which(rivers.all$GEWISS_NR[f_a] == rivers.all$GEWISS_NR[l_a] & rivers.all$LAUF_NR[f_a] %in% c(0, 999))
	gew_eq2_a <- which(rivers.all$GEWISS_NR[l_a] == rivers.all$GEWISS_NR[f_a] & rivers.all$LAUF_NR[f_a] %in% c(0, 999))
	df_flow_to$option_a[l_a[gew_eq2_a]] <- rivers.all$id_all[f_a[gew_eq1_a]] #write flow to ID when GEWissnr is equal
	
	# for b
	gew_eq1_b <- which(rivers.all$GEWISS_NR[f_b] == rivers.all$GEWISS_NR[l_b] & rivers.all$LAUF_NR[f_b] %in% c(0, 999))
	gew_eq2_b <- which(rivers.all$GEWISS_NR[l_b] == rivers.all$GEWISS_NR[f_b] & rivers.all$LAUF_NR[f_b] %in% c(0, 999))
	df_flow_to$option_b[l_b[gew_eq2_b]] <- rivers.all$id_all[f_b[gew_eq1_b]] #write flow to ID when GEWissnr is equal
	
	# for c
	gew_eq1_c <- which(rivers.all$GEWISS_NR[f_c] == rivers.all$GEWISS_NR[l_c] & rivers.all$LAUF_NR[f_c] %in% c(0, 999))
	gew_eq2_c <- which(rivers.all$GEWISS_NR[l_c] == rivers.all$GEWISS_NR[f_c] & rivers.all$LAUF_NR[f_c] %in% c(0, 999))
	df_flow_to$option_c[l_c[gew_eq2_c]] <- rivers.all$id_all[f_c[gew_eq1_c]] 
	
	#testing:
	df_flow_to[which(!(rowSums(df_flow_to[, 2:4], na.rm = T) == rowMeans(df_flow_to[, 2:4], na.rm = T))), ] #shoud be empty
	#found two conflicts.. use first option here
	# View(df_flow_to)
	
	#make final solution
	df_flow_to$final <- NA
	df_flow_to$final[which((rowSums(df_flow_to[, 2:4], na.rm = T) == rowMeans(df_flow_to[, 2:4], na.rm = T)))] <- rowSums(df_flow_to[which((rowSums(df_flow_to[, 2:4], na.rm = T) == rowMeans(df_flow_to[, 2:4], na.rm = T))), 2:4], na.rm = T) #when no conflict, use the only number
	
	#in conflict use first row
	df_flow_to$final[which(!(rowSums(df_flow_to[, 2:4], na.rm = T) == rowMeans(df_flow_to[, 2:4], na.rm = T)))] <- df_flow_to$option_a[which(!(rowSums(df_flow_to[, 2:4], na.rm = T) == rowMeans(df_flow_to[, 2:4], na.rm = T)))]
	
	
	
	
	### use other connections with distance 0
	missing_a <- which(l_a %in% df_flow_to$id[is.na(df_flow_to$final)])
	g_a <- rivers.all$LAUF_NR[f_a[missing_a]] %in% c(0, 999)
	df_flow_to$option2_a[l_a[missing_a[g_a]]] <- f_a[missing_a[g_a]]
	
	#for b
	missing_b <- which(l_b %in% df_flow_to$id[is.na(df_flow_to$final)])
	g_b <- rivers.all$LAUF_NR[f_b[missing_b]] %in% c(0, 999)
	df_flow_to$option2_b[l_b[missing_b[g_b]]] <- f_b[missing_b[g_b]]
	
	#for c
	missing_c <- which(l_c %in% df_flow_to$id[is.na(df_flow_to$final)])
	g_c <- rivers.all$LAUF_NR[f_c[missing_c]] %in% c(0, 999)
	df_flow_to$option2_c[l_c[missing_c[g_c]]] <- f_c[missing_c[g_c]]
	
	#testing
	df_flow_to[which(!(rowSums(df_flow_to[, 5:7], na.rm = T) == rowMeans(df_flow_to[, 5:7], na.rm = T))), ]
	rivers.all$OBJEKTART[which(!(rowSums(df_flow_to[, 5:7], na.rm = T) == rowMeans(df_flow_to[, 5:7], na.rm = T)))]
	# many sections are lakes. -> doesn´t matter
	# other sections are rural rivers. just choose first option
	
	
	# final 
	na_rows <- which(is.na(df_flow_to$final))
	df_flow_to$final[na_rows] <- rowMeans(df_flow_to[na_rows, 5:7], na.rm = T)
	
	#conflict handling
	df_flow_to$final[which(!(rowSums(df_flow_to[, 5:7], na.rm = T) == rowMeans(df_flow_to[, 5:7], na.rm = T)))] <- df_flow_to$option2_a[which(!(rowSums(df_flow_to[, 5:7], na.rm = T) == rowMeans(df_flow_to[, 5:7], na.rm = T)))] #use option 2a in case of conflict
	
	
	
	####next: for all still empty spots use the element with distance 0
	# shows all options nrst_a
	dist_0m <- which(as.numeric(st_distance(last, first[nrst_a, ], by_element = T)) < 0.1) #find all with distance 0
	dist_0m_NA <- dist_0m[dist_0m %in% which(is.na(df_flow_to$final))] #find all with distance 0 which are still missing in df_flow_to$final
	
	df_flow_to$final[dist_0m_NA] <- nrst_a[dist_0m_NA]
	
	
	df_flow_to$final[is.na(df_flow_to$final)] <- NA
	
	#done: rest reminds NA
	
	
	
	
	
	
	
	#add flow to for rivers to river network
	rivers.all$flow_to <- df_flow_to$final
	save(rivers.all, file = paste0(main.path, "temp_data/forPlot.Rdata")) #with all connection also through lakes
	

	# b) find outflow for lakes
	###############
	# outflow for lakes are the first linefeature which is not a lake any more. all linefeatures in the lake are directed to "outflow" linefeature regardless their location.
	
	#id of lakes
	id_lakes <- unique(rivers.all$FID_poly_s)[-is.na(unique(rivers.all$FID_poly_s))] # numbers without "river" in lake are missing, without NA
	possible_options <- NULL
	
	rivers_conflict <- NULL
	
	for (i in 1 : length(id_lakes)){
		rivers.i <- which(rivers.all$FID_poly_s == id_lakes[i]) # which rivers have lake id = i
		rivers.i.flow_to <- unique(rivers.all$flow_to[rivers.i])
		if(length(rivers.i.flow_to) > 1){
			rivers.i.flow_to <- rivers.i.flow_to[!(is.na(rivers.i.flow_to))] # if more than one outflow option omit na option
			if(length(rivers.i.flow_to) < 1){ #if all options were NA, new length would be < 1. thus fill rivers.i.flow_to with NA
				rivers.i.flow_to <- NA
				next
			}
		}
		# if lake section has only one outflow is available choose this one. in case of more lake section or more than one option select river (objektar = 4) connected to lake if possible or other options..
		if(length(rivers.i.flow_to) == 1){ # if length == 1
			rivers.all$flow_to[rivers.i] <- rivers.i.flow_to # if outflow is NA, put rivers.i.flow_to for all river sections within lake rivers.i.flow_to (will be NA), otherwise it will be the only option possible (case one outflow)
		}else{
			#more than one option
			outflow.river4 <- which(rivers.all$OBJEKTART[rivers.i.flow_to] == 4) #check which is outflow with objektart = 4
			outflow.all <- which(rivers.all$FID_poly_s[rivers.i.flow_to] != id_lakes[i]) # check for flows to other section but not the same lake (id_lakes[i])
			
			if(length(outflow.river4) == 1){ # ideal case. only one outflow to one river. prefer river before pressure tubes etc.
				rivers.all$flow_to[rivers.i] <- rivers.i.flow_to[outflow.river4] # select all flow_to of river. fill with outflow.river4 river section
				next #go to next lake id
			}
			
			# same case as above. only one option possible
			if(length(outflow.all) == 1 & length(outflow.river4) < 1){ 
				rivers.all$flow_to[rivers.i] <- rivers.i.flow_to[outflow.all]
				next
			}
			
			# more than one option was found
			if(length(outflow.all) > 1 | length(outflow.river4) > 1){
				if(length(outflow.river4) > 1){ #if more than one river possibility
					id_possible <- rivers.i.flow_to[outflow.river4]
				}else{
					id_possible <- rivers.i.flow_to[outflow.all]
				}
				a <- which(rivers.all$LAUF_NR[id_possible] == 0)# take outflows which are main river
				if(length(a) == 1){
					rivers.all$flow_to[rivers.i] <- id_possible[a]
				}	else{
					low_gewissNR <- which(rivers.all$GEWISS_NR[id_possible] == min(rivers.all$GEWISS_NR[id_possible])) #get river with lowest GEWISS_NR because this is the biggest river
					rivers.all$flow_to[rivers.i] <- id_possible[low_gewissNR[1]] # position 1 in case of multiple possibilities
				}
				next
			}
			
			
			if(length(outflow.river4)<1 & length(outflow.all)<1){ #worst case. for both option no result. means no outflow of lake.
				rivers.all$flow_to[rivers.i] <- NA
				}
			}
		}
	
	
	
	# warnings()
	print(sort(rivers_conflict))
	# st_write(rivers.all[rivers_conflict, ], "PhD/spielereien/conflicts.gpkg", append = F)
	
	### all data that flow to each self. make NA
	rivers.all$flow_to[which(rivers.all$id_all == rivers.all$flow_to)] <- NA
	
	
	########
	# save data
	########
	
	# write first and last point to file 
	st_write(first, paste0(main.path, "temp_data/first_points.shp"), append = F) #writing first points for control
	
	st_write(last, paste0(main.path, "temp_data/last_points.shp"), append = F)
	
	#save data without connections
	st_write(rivers.all[is.na(rivers.all$flow_to), ], paste0(main.path, "temp_data/not_connected.gpkg"), append = F)
	
	save(rivers.all, last, first, file = paste0(main.path, "data_modified/river_network/rivers_all01.Rdata"))
	st_write(rivers.all, paste0(main.path, "data_modified/river_network/rivers_all01_gis.gpkg"), append = F)
	
	rm(list = ls())
	