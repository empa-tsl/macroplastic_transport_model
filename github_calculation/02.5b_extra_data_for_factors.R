#########################
# Information
#
#This script is used to gether further information for the macroplastic modelling. This includes:
# dam possition on rivers
# calculating the sinocity (be calculating distances of two points and the lengths of the river segments)
# landuse (using data by swisstopo)
# discharge was added beforehand. but will be needed for calculation too
# August 2023, david mennekes
#########

#packages
	library(sf)
	library(dplyr)
	library(readr)
	library(tidyverse)


	#path
	main.path <- "PhD/mennekes2.0/"
	
	
	
	#load the data with all the input emission (check modelling of Microplastics )
	load(paste0(main.path, "temp_data/rivers_all5.Rdata"))#data with rivers

	#load data with further information about weirs/dams
	dam_position <- st_read("PhD/data/karten/stauanlagen-bundesaufsicht/Dam.shp") #not freely available, data of dams by swisstopo
	dam_type <- st_drop_geometry(st_read("PhD/data/karten/stauanlagen-bundesaufsicht/DamTypeCatalogue.shp")) #contains information about dam type
	
	dam_facility <- st_drop_geometry(st_read("PhD/data/karten/stauanlagen-bundesaufsicht/Facility.shp")) #futher information of aim of the dam
	dam_facility_type <- st_drop_geometry(st_read("PhD/data/karten/stauanlagen-bundesaufsicht/FacilityAimCatalogue.shp")) #catalogue data for dam facilities

	
	
	
	
	
	#step 01 -> join information of dams
	dam2 <- merge(dam_position, dam_type, by.x = "DamType", by.y = "ID", all.x = T) #get dam type information
	fac2 <- merge(dam_facility, dam_facility_type, by.x = "Aim", by.y = "ID", all.x = T) %>% select("xtf_id", "DE", "EN") #merge information of usage
	dam3 <- merge(dam2, fac2, by.x = "facilityR2", by.y = "xtf_id", all.x = T) %>% select("ConstYear", "DamName", "DamHeight", "DE.x", "EN.x", "DE.y", "EN.y")

	rm(fac2, dam2, dam_facility, dam_facility_type, dam_position, dam_type)
	
	#step 02 -> join information of dams to river network
	rivers.nrst <- rivers.all5[rivers.all5$isLake==F, ]
	nf <- st_nearest_feature(dam3, rivers.nrst) #find nearst river segment
	nrst <- rivers.nrst$id_all[nf] #get id all of this segment
	rm(nf, rivers.nrst)
	
	dst <- as.numeric(st_distance(dam3, rivers.all5[nrst, ], by_element = T)) #measure distance between segment and dam
	dst
	nrst_100m <- nrst[dst<100] #select only dams that are in 100m distance to a river segment. other dams might not be related to a river segment
	dam4 <- dam3[dst<100, ]
	
	# create new rows for rivers.all5
	
	rivers.all5$isDam <- F
	rivers.all5$isDam[nrst_100m] <- T

	rivers.all5$DamType <- NA	
	rivers.all5$DamType[nrst_100m] <- dam4$EN.y
	
	
	
	#step 02 -> meandering factor (direct length between first and last point divided by segment length)
	mf <- rivers.all5 %>% select(id_all, geometry)
	mf$length <- as.numeric(st_length(rivers.all5))
	mf$first <- st_line_sample(rivers.all5, sample = 0) #get first point of each line feature
	mf$last <- st_line_sample(rivers.all5, sample = 1)	#get last point of each line feature
	mf$dist_firstlast <- as.numeric(st_distance(mf$first, mf$last, by_element = T)) #distance between first and last point
	mf$meander_factor <- mf$length/mf$dist_firstlast #the meandering factor is represented by (real length of river segment) / (theoretical shortest possible lengths)

	hist(mf$meander_factor)	
	sum(mf$meander_factor>1.5)
	mf$meander_factor[rivers.all5$isLake] <- NA #no meandering factors for lakes
	mf$meander_factor[rivers.all5$outflow != 0] <- NA #no meandering factor for outflow containers
	
	# plot(1:nrow(mf), log10(sort(mf$meander_factor)))
	# plot(mf$length, mf$meander_factor, xlim = c(0,10000))
	
	rivers.all5$meandering_factor <- mf$meander_factor

	rm(mf)	
	rm(nrst, nrst_100m, dst, dam3, dam4)
	
	
	
	#step 03 -> land use / cover next to the rivers
	#load data

	landuse_buffer_10m <- read_delim(paste0(main.path,"data_raw/land use/landuse_buffer_10m.txt"), 
			delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = "."))
	
	landuse_buffer_100m <- read_delim(paste0(main.path,"data_raw/land use/landuse_buffer_100m.txt"), 
																		delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = "."))
	
	landuse_buffer_200m <- read_delim(paste0(main.path,"data_raw/land use/landuse_buffer_200m.txt"), 
																	 delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = "."))
		
	#change format to wide format
	landuse10 <- spread(landuse_buffer_10m[ , c("id_all_geo", "land_cover", "geom_Area")], land_cover, geom_Area) %>% select(-"<NA>")
	landuse10$agriculture[is.na(landuse10$agriculture)] <- 0 #fill NA with 0
	landuse10$forest[is.na(landuse10$forest)] <- 0
	landuse10$glacier[is.na(landuse10$glacier)] <- 0
	landuse10$rocks[is.na(landuse10$rocks)] <- 0
	landuse10$unknown[is.na(landuse10$unknown)] <- 0
	landuse10$urban[is.na(landuse10$urban)] <- 0
	landuse10$water[is.na(landuse10$water)] <- 0
	
	sum(duplicated(landuse10$id_all_geo)) #control -> should be 0
	
	landuse100 <- spread(landuse_buffer_100m[ , c("id_all_geo", "land_cover", "geom_Area")], land_cover, geom_Area) %>% select(-"<NA>")
	
	landuse100$agriculture[is.na(landuse100$agriculture)] <- 0 #fill NA with 0
	landuse100$forest[is.na(landuse100$forest)] <- 0
	landuse100$glacier[is.na(landuse100$glacier)] <- 0
	landuse100$rocks[is.na(landuse100$rocks)] <- 0
	landuse100$unknown[is.na(landuse100$unknown)] <- 0
	landuse100$urban[is.na(landuse100$urban)] <- 0
	landuse100$water[is.na(landuse100$water)] <- 0

	landuse200 <- spread(landuse_buffer_200m[ , c("id_all_geo", "land_cover", "geom_Area")], land_cover, geom_Area) %>% select(-"<NA>")	
	
	landuse200$agriculture[is.na(landuse200$agriculture)] <- 0 #fill NA with 0
	landuse200$forest[is.na(landuse200$forest)] <- 0
	landuse200$glacier[is.na(landuse200$glacier)] <- 0
	landuse200$rocks[is.na(landuse200$rocks)] <- 0
	landuse200$unknown[is.na(landuse200$unknown)] <- 0
	landuse200$urban[is.na(landuse200$urban)] <- 0
	landuse200$water[is.na(landuse200$water)] <- 0

	rm(landuse_buffer_10m, landuse_buffer_100m, landuse_buffer_200m)	

	
	#add column with total area and find distribution among land cover -> 0:1
	names(landuse10)
	landuse10$total_area <- rowSums(landuse10[ , 2:8], na.rm = T)
	landuse10[ ,2:8] <- landuse10[2:8]/landuse10$total_area
	
	landuse100$total_area <- rowSums(landuse100[ , 2:8], na.rm = T)
	landuse100[ ,2:8] <- landuse100[2:8]/landuse100$total_area
	
	landuse200$total_area <- rowSums(landuse200[ , 2:8], na.rm = T)
	landuse200[ ,2:8] <- landuse200[2:8]/landuse200$total_area
	
	
	
	#if more than 50% of landuse is water change
	 #final landuse (for rivers) / attention, some rivers are missing information because they are located outside of Switzerland, here "unknown" = 1 was assumed
	
	use_10m <- which(landuse10$water <= 0.5) #use only rows where water is less than 50%
	ids_10 <- landuse10$id_all_geo[use_10m]
	use_100m <- which(landuse100$water <= 0.5) #which landuse is smaler/ equal 50% water where 10m buffer was more than 50
	use_100m <- use_100m[!(landuse100$id_all_geo[use_100m] %in% ids_10)] #filter ids that are not in ids_10
	
	
	
	#make final dataset
	
	landuse_final <- landuse10[use_10m, ] #final version first 10m
	landuse_final <- rbind(landuse_final, landuse100[use_100m, ])
	w <- which(!(landuse200$id_all_geo %in% landuse_final$id_all_geo)) #which ids from the buffer 200 are not in the final version, yet
	landuse_final <- rbind(landuse_final, landuse200[w, ])
	
	#check for duplicated id
	sum(duplicated(landuse_final$id_all_geo))
	
	
	
	
	#add information to rivers.all5
	rivers.all5[, names(landuse_final[2:8])] <- 0
	rivers.all5[landuse_final$id_all_geo, names(landuse_final[2:8])] <- landuse_final[ , names(landuse_final[2:8])]

	
	####################
	#step04 -> landuse for lakes	
	landuse_lake <- read_delim(paste0(main.path, "data_raw/land use/landuse_lakes_100m.txt"), 
																	 delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",", grouping_mark = "."))
	
	landuse_lake <- spread(landuse_lake[ , c("FID_poly_s", "land_cover", "Shape_Area")], land_cover, Shape_Area) %>% select(-water) #take out water because no further information...

	#change NA to 0
	landuse_lake$agriculture[is.na(landuse_lake$agriculture)]<-0
	landuse_lake$forest[is.na(landuse_lake$forest)]<-0
	landuse_lake$glacier[is.na(landuse_lake$glacier)]<-0
	landuse_lake$rocks[is.na(landuse_lake$rocks)]<-0
	landuse_lake$unknown[is.na(landuse_lake$unknown)]<-0
	landuse_lake$urban[is.na(landuse_lake$urban)]<-0

	landuse_lake$total_area <- rowSums(landuse_lake[ , 2:7])	#total area
	landuse_lake[ , 2:7] <- landuse_lake[ , 2:7] / landuse_lake$total_area

	sum(duplicated(landuse_lake$FID_poly_s)) #check for duplicated ids
	
	#join data to rivers.all5
	m <- merge(rivers.all5[ ,c("id_all", "FID_poly_s")], landuse_lake, by = "FID_poly_s") #merge by FID_poly_s to find id_all
	
	rivers.all5[m$id_all, names(landuse_lake[, 2:7])] <- st_drop_geometry(m[ ,names(landuse_lake[, 2:7])])
	
	
	
	#check for NA in factor data
	rivers.all5$forest[is.na(rivers.all5$forest)] <- 0
	rivers.all5$agriculture[is.na(rivers.all5$agriculture)] <- 0
	rivers.all5$rocks[is.na(rivers.all5$rocks)] <- 0
	rivers.all5$unknown[is.na(rivers.all5$unknown)] <- 0
	rivers.all5$urban[is.na(rivers.all5$urban)] <- 0
	rivers.all5$water[is.na(rivers.all5$water)] <- 0
	rivers.all5$glacier[is.na(rivers.all5$glacier)] <- 0
	
	#find rows which are not summing up to 1
	rSums <- rowSums(st_drop_geometry(rivers.all5[ , c("forest", "agriculture", "rocks", "unknown", "urban", "water", "glacier")]))
	falls.rsums <- which(rSums <0.99 | rSums > 1.01) #small tolorance
	rivers.all5[ falls.rsums,c("forest", "agriculture", "rocks", "unknown", "urban", "water", "glacier")] <- NA
	
	rivers.all5$meandering_factor[is.na(rivers.all5$meandering_factor)] <- 0
	
	rivers.all5$discharge[is.na(rivers.all5$discharge)] <- 0.00001
	

	#save final data
	save(rivers.all5, file = paste0(main.path, "temp_data/rivers_all5_2.Rdata"))
	rm(list = ls())
	