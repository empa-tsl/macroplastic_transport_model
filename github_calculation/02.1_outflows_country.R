	###############
	# only to make file with endpoints of river sections which contain a name.
	# this specific points need to be reviewed in a GIS software and manuelly selected
	# output containers are linefeatures drown in a GIS software which are next to the outflowing river. we did this only for the bigger outflowing countries. Additionally, added two outflow container after each other. the first outflow container/linefeature passes the contamination on without any interaction while the second one is a final container. inflow always flows to the same container again....
	# we marked the outflow container with the attribute outflow == 1 for rivers and the first type of outflow and outflow == 2 for the final upsumming container.
	# outflow == 3 and outflow == 4 correspond to the first and second outflow type which recieve all pollution of dead ends or small border crossing rivers
	# author: david mennekes, PhD Student at Empa ST. Galen / ETH Zürich, Switzerland, david.mennekes@empa.ch
	# march 2021
	######################
	setwd("~/")
	
	# packages
	library(sf)
	library(dplyr)
	library(tidyverse)
	library(sp)
	library(raster)
	
	main.path <- "Phd/mennekes/"
	
	
	#load data
	load(paste0(main.path, "data_modified/river_network/rivers_all01.Rdata")) #dataset with river segments connected according to the direction of flow.
	
	#use rivers.all2 for modification in this script
	
	rivers.all2 <- rivers.all
	rm(rivers.all)
	
	
	outflows_container <- st_read(paste0(main.path, "data_modified/river_network/outflow_lines.shp")) #this data was obtained manually in a GIS Software, country = "none" is container for NAs -> for data collection of outflowing rivers / cross boundary rivers. Or rivers that are dead ends.

	#check for names. column Id is not needed
	outflows_container <- outflows_container[ , -1]
	
	
	#bring dataframe together with rivers.all2
	#make empty rows again
	names.all <- c(names(outflows_container), names(rivers.all2))
	
	outflows_container[ , setdiff(names.all, names(outflows_container))] <- NA #make NAs
	rivers.all2[ , setdiff(names.all, names(rivers.all2))] <- NA
	
	#bring togehter
	rivers.all2a <- rbind(rivers.all2, outflows_container[ , names(rivers.all2)])
	tail(rivers.all2a)
	
	rm(rivers.all2)
	
	#####
	#get id_all for outflows. Id_all equals to rownumber
	rivers.all2a$id_all[which(!(is.na(rivers.all2a$outflow)))] <- which(!(is.na(rivers.all2a$outflow)))
	#check for duplicated
	sum(duplicated(rivers.all2a$id_all))#no duplicated! perfecto
	
	
	#find first and last points again for flow to connections. distances should be 0
	#find outflows to the "container" points
	#outflow =1 for first section after outflow, =2 for second section, follows outflow = 1, =3 for outflow unknown, =4 follows =3
	
	#find first point for outflow of =1
	outflows_container_first <- st_line_sample(rivers.all2a[which(rivers.all2a$outflow == 1), ], sample = 0)
	id_first1 <- 	which(rivers.all2a$outflow == 1)
	
	#find last points
	outflows_container_last <- st_line_sample(rivers.all2a[which(rivers.all2a$outflow == 1), ], sample = 1)
	
	#find first points for outflow =2
	outflows_container_first2 <- st_line_sample(rivers.all2a[which(rivers.all2a$outflow == 2), ], sample = 0)
	id_first2 <- 	which(rivers.all2a$outflow == 2)
	
	#find last points for rivers.all2
	last_rivers <- st_line_sample(rivers.all2a, sample = 1)
	
	

	
	### find nearest feature between last points rivers.all2a und first piont outflows=1
	
	nrst.last <- st_nearest_feature(outflows_container_first, last_rivers)
	
	#check distances: 
	dist.d <- st_distance(outflows_container_first, rivers.all2a[nrst.last, ], by_element = T) #passt
	
	if(sum(as.numeric(dist.d)) != 0){warning("some endpoints are not connected to rivernetwork!")}
	
	#write flow to into rivers.all
	rivers.all2a$flow_to[nrst.last] <- id_first1
	
	#find nearst. feature for outflow =1 to outflow =2
	
	nrst.outflow <- st_nearest_feature(outflows_container_last, outflows_container_first2) #caution. the row numbers are only in relation to outflows_countainer_last
	
	#write to rivers.all
	rivers.all2a$flow_to[id_first1] <- id_first2[nrst.outflow]
	
	
	########## correct some rivers which are not really connected but have contamination in flow
	#correct lago maggiore, da läuft etwas falsch und die Flüsse laufen nicht raus. deshalb händische zuordnung zum richtigen ausfluss
	w <- rivers.all2a$id_all[grepl("Maggiore", rivers.all2a$name_2) & !(is.na(rivers.all2a$type_lake))]
	rivers.all2a$flow_to[w] <- 441452
	#exception: tresa (outflow lugano lake) flows to lago maggior (thats a fact)
	rivers.all2a$flow_to[245951] <- 322929
	rivers.all2a[245951, ] #check name
	
	# more corrections, specific to certain rows / river segments. only in our model because of the data we used.
	rivers.all2a$flow_to[145582] <- 65252
	rivers.all2a$flow_to[161662] <- 151698
	rivers.all2a$flow_to[137386] <- 161435
	rivers.all2a$flow_to[119416] <- 64103
	rivers.all2a$flow_to[330682] <- 107754
	rivers.all2a$flow_to[330731] <- 82724
	rivers.all2a$flow_to[229408] <- 2262
	rivers.all2a$flow_to[56105] <- 297158
	rivers.all2a$flow_to[169845] <- 118141
	rivers.all2a$flow_to[158965] <- 258322
	
	
	##########
	# handle all rivers that flow to NA
	
	rivers.all2a$flow_to[is.na(rivers.all2a$flow_to)] <- rivers.all2a$id_all[which(rivers.all2a$outflow == 3)] #all flow to with NA flowing to the container of NAs which is the outflow =3
	
	#outflow=3 flows to outflow=4
	rivers.all2a$flow_to[which(rivers.all2a$outflow == 3)] <- rivers.all2a$id_all[which(rivers.all2a$outflow == 4)]
	
	
	
	#####
	#outflows =2 and outflow=4 flow to each self.. for control
	rivers.all2a$flow_to[which(rivers.all2a$outflow %in% c(2,4))] <- rivers.all2a$id_all[which(rivers.all2a$outflow %in% c(2,4))]
	
	
	rivers.all2 <- rivers.all2a
	rm(rivers.all2a)
	save(rivers.all2, file = paste0(main.path, "data_modified/river_network/rivers_all3.Rdata")) #save in temp_data folder. should be created..
	
	rm(list = ls())
	