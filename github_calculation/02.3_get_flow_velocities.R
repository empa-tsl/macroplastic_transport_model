	######################
	# connect discharge data from old maps (old crs) with data used here
	# author: david mennekes; david.mennekes@empa.ch
	
	#packages
	library("sf")
	library("dplyr")
	
	#load data
	
	# wd
	setwd("~/")
	
	#main path
	main.path <- "PhD/mennekes/"
	
	#new data
	load(paste0(main.path, "temp_data/rivers_all3.Rdata"))
	rivers_LV05 <- st_transform(rivers.all3, 21781) #transform to old CRS for connecting with simulated data by swisstopo
	
	st_crs(rivers_LV05)
	
	#old data with discharge information
	# for Switzerland discharge data is available but only for older maps. 
	MQ_GWN_CH <- read.csv("~/PhD/data/karten/mittlerer Q/MQ-GWN-CH/Datensatz/MQ_GWN_CH.txt", header=T)

	#load old gwn25 (CRS95)
	gwn25 <- st_read("PhD/data/karten/swisstopo/200901_mennekes/200901_mennekes/Vec25_LV03_2008/gwn_25_l.shp") #available upon request
	st_crs(gwn25) = 21781 #assign crs CH1903 to CH1903
	

	#test: showing the same seciton?
	plot(st_geometry(gwn25[gwn25$GWLNR == "CH0006420000", ]))
	plot(st_geometry(rivers_LV05[rivers_LV05$GWL_NR == "CH0006420000", ]), col = "red", add = T)
	
	gwn_Q <- merge(gwn25, MQ_GWN_CH, by.y = "OBJECTID_GWN25", all.x = T, by.x = "OBJECTID")
	gwn_Q <- gwn_Q[ , c("MQN_JAHR", "GEWISSNR", "geometry")] #select only needed data
	
	
	#######
	# 1.) get Data from discharge stations in Switzerland
	# read data from stations
	f_long <- list.files("PhD/data/karten/mittleren Abflüsse für Stationen/Q/",full.names = T) # available upon request
	
	Q_station <- data.frame(Station_NR = rep(NA, length(f_long)),
													river = NA,
													discharge = NA,
													Parameter = NA,
													Parametereinheit = NA)
	
	#read all values from all files
	for (i in 1:length(f_long)) {
		t <- read.csv(f_long[i], sep=";", skip = 8)
		Q_station[i, "Station_NR"] <- t$Stationsnummer
		Q_station[i, "river"] <- t$Gewässer
		Q_station[i, "discharge"] <- t$Wert
		Q_station[i, "Parameter"] <- t$Parameter
		Q_station[i, "Parametereinheit"] <- t$Parametereinheit
	}
	rm(t)
	

	unique(Q_station$Parametereinheit) # change l/s to m3/s -> *0.001
	unique(Q_station$Parameter) #nur Abfluss: keine Seewasserstände
	
	#change all discharge to same unit (m3/s)
	Q_station$discharge[Q_station$Parametereinheit == "l/s"] <- Q_station$discharge[Q_station$Parametereinheit == "l/s"]*0.001
	Q_station$Parametereinheit <- "m3/s"

	
	# change discharge to flow velocity based on paper with IOWA, USA, Dataset
	Q_station$flow_velocity <- 0.3*Q_station$discharge^0.228

	plot(Q_station$flow_velocity, Q_station$discharge)
	
	
	##### connect discharge with station location. station location than will be connected to river location
	#read locations of station according to shapefile
	location_station <- st_read("PhD/data/karten/mittleren Abflüsse für Stationen/hydrometrische_Stationen_2019/lhg_UBST.shp")
	st_crs(location_station) <- 21781 #assign coordinate system to shapefile
	unique(location_station$lhg_code)
	location_station <- location_station[location_station$lhg_code == "lhg_fluss", ]#filter only river discharge
	location_station
	
	
	sum(duplicated(gwn25$OBJECTID)) #test for duplicates
	
	rivers_LV05_2 <- rivers_LV05[as.numeric(st_length(rivers_LV05))>0, ] #use only existing rivers
	
	#assign discharge of station to river section based on 
	close_river <- st_nearest_feature(location_station, rivers_LV05_2) #row from close river
	dis_close <- st_distance(location_station, rivers_LV05_2[close_river, ], by_element = T)
	hist(dis_close)
	location_station$loc_ID <- 1:nrow(location_station)
	location_station[which(as.numeric(dis_close) >100), c("lhg_name", "loc_ID")]
	rivers_LV05_2[close_river[which(as.numeric(dis_close) >100)], "NAME"] #we deleted the following lines:  189 & 216, 217
	duplicated(close_river)
	unique(close_river) #delete data which was wrongly associated
	

	location_station$river_idall <- rivers_LV05$id_all[close_river] #  GWSNR is an ID for each river given by the data. 
	location_station <- location_station[-c(189,216, 217), ] #delete
	location_station <- location_station[!(location_station$river_idall %in% close_river[duplicated(close_river)]), ] #duplicated -> delete
		
	Q_station_compl <- merge(location_station, Q_station, by.y = "Station_NR", all.x = T, by.x = "EDV_NR4")
	is.na(Q_station_compl$discharge)
	Q_station_compl <- Q_station_compl[!(is.na(Q_station_compl$discharge)), ] #delete all rows with no discharge
	
	
	
	
	##### 
	# 2.) use data from discharge modelling by swisstopo
	# assign data to the new data set
	gwn_Q_woNA <- na.omit(gwn_Q) #entries with NA value in gwn_Q
	gwn_Q_woNA$flow_velocity <- 0.3*gwn_Q_woNA$MQN_JAHR^0.228 #calculate flow velocity based on paper
	
	rivers_LV05$discharge <- NA
	rivers_LV05$flow_velocity <- NA
	rivers_LV05$flow_velocity_type <- NA
	
	#Search for same GewissNR (ID by the data set) and a distance less than 10m
	for (i in 1:nrow(gwn_Q_woNA)) {
		z <- which(gwn_Q_woNA$GEWISSNR[i] == rivers_LV05$GEWISS_NR) #find all sections with same GWN_NR
		if(length(z) < 1){
			next #if not found, skip
		}
		z2 <- which(as.numeric(st_distance(gwn_Q_woNA[i, ], rivers_LV05[z, ], by_element = T)) < 10) #one segment in the old data set gwn_Q_woNA might be represented by muliple segments in the newer data set rivers_LV05
		rivers_LV05$flow_velocity[z[z2]] <- gwn_Q_woNA$flow_velocity[i] #use the flow velocity
		rivers_LV05$discharge[z[z2]] <- gwn_Q_woNA$MQN_JAHR[i]
		rivers_LV05$flow_velocity_type[z[z2]] <- "model BAFU" #set type to model bafu
		print(i)
	}
	
	rivers_LV05[which(rivers_LV05$OBJEKTART == 6), c("flow_velocity", "flow_velocity_type", "discharge")] <- NA #overwrite values for lakes with NA in case something did go wrong
	save(rivers_LV05,
			 file = paste0(main.path, "temp_data/get_flow_v_rivers_LV05.Rdata")) #save
	
	########## load data
	load(paste0(main.path, "temp_data/get_flow_v_rivers_LV05.Rdata"))
	
	
	
	
	
	#### connect flow velocity with rivers.all3
	rivers.all4 <- rivers.all3 #create new data frame
	rivers.all4$isLake <- F
	rivers.all4$isLake[!(is.na(rivers.all4$volume))] <- T #make T / F for lake
	rivers.all4$flow_velocity <- rivers_LV05$flow_velocity
	rivers.all4$flow_velocity_type <-rivers_LV05$flow_velocity_type
	rivers.all4$discharge <- rivers_LV05$discharge
	# rm(rivers.all3)
	# rm(rivers_LV05)
	
	####
	#add information from station measurements Q_stations_comp
	# use mean values between measured and modelled
	rivers.all4$flow_velocity[Q_station_compl$river_idall] <- rowMeans(
		data.frame(a = rivers.all4$flow_velocity[Q_station_compl$river_idall], 
							 b = Q_station_compl$flow_velocity), na.rm = T)
	
	rivers.all4$flow_velocity_type[Q_station_compl$river_idall] <- "measurement"
	rivers.all4$discharge[Q_station_compl$river_idall] <- Q_station_compl$discharge
	save(rivers.all4, file = paste0(main.path, "temp_data/rivers_all4.Rdata"))
	
	
	
	### pass on known velocities
	# if velocity is known in a section i but unknown in the section downstream of i (i+1) then section i+1 get velocity from section i. If two velocities could be passed on the higher one will be used. Assuming this would be the main river
	
	# set marker for rivers that have unknown flow velocity. These data can´t be overwritten
	rivers.all4$is_known_v <- F
	rivers.all4$is_known_v[!(is.na(rivers.all4$flow_velocity))] <- T
	known_v_id <- which(rivers.all4$is_known_v == T) #here flow velocity is known
	
	
	u <- 0
	v <- 0
	for (i in 1:500) { #set high number to also take over higher flow velocities in larger rivers
		print(paste(u-sum(is.na(rivers.all4$flow_velocity), na.rm = T), "more explained"))
		print(paste0("change sum velocity", v -sum(rivers.all4$flow_velocity, na.rm = T)))
		v <- sum(rivers.all4$flow_velocity, na.rm = T)
		u <- sum(is.na(rivers.all4$flow_velocity), na.rm = T)
		
		
		
		df <- data.frame(id = rivers.all4$flow_to, #get ids wo es hinfließt
										 fv = rivers.all4$flow_velocity) #get flow velocity von vorherigen Flusssection
		df2 <- df %>% filter(!(is.na(fv))) %>% group_by(id) %>% summarise(max_v = max(fv, na.rm = T)) # group by ID to determine maximum value when multiple rivers flow into one single river. Use the maximum flow velocity based on the assumption that velocity becomes rather higher than lower in average
		
		df2 <- df2[!(df2$id %in% known_v_id), ] #filter all ids which are known by measurement or models
		# write the found velocity inrivers.all 4
		rivers.all4$flow_velocity[df2$id] <- df2$max_v
		rivers.all4$flow_velocity_type[df2$id] <- "uebernommen"
		print(paste("round:",i))
		
	}
	
	summary(rivers.all4$flow_velocity)
	rivers.all4$flow_velocity
	
	
	##### find solutions for all river sections that do not have a flow velocity yet
	# a) river sections above 1800m are mountaineous rivers -> flow velocity == 0.5
	
	#select all rivers where velocity is NA and height of last river part ist above 1800m and not lake
	i1 <- rivers.all4$id_all[is.na(rivers.all4$flow_velocity) & rivers.all4$height_last>1800 & !(rivers.all4$isLake)] 
	rivers.all4$flow_velocity[i1] <- 0.5
	rivers.all4$flow_velocity_type[i1] <- "above1800"
	
	#below 1800m 
	i2 <- rivers.all4$id_all[is.na(rivers.all4$flow_velocity) & rivers.all4$height_last<1800 & !(rivers.all4$isLake)] 
	rivers.all4$flow_velocity[i2] <- 0.25
	rivers.all4$flow_velocity_type[i2] <- "below1800"
	
	# rest
	i3 <- rivers.all4$id_all[is.na(rivers.all4$flow_velocity) & !(rivers.all4$isLake)]
	rivers.all4$flow_velocity[i3] <- 0.25
	rivers.all4$flow_velocity_type[i3] <- "rest"
	
	rivers.all4$discharge[is.na(rivers.all4$discharge)] <- 0.05
	
	#collect total inflow into one lake
	
	
	#inflow to lakes to determine residence times for lakes. This is used for rough estimation of amount of plastics in the lake
	lake_id <- as.numeric(na.omit(unique(rivers.all4$FID_poly_s)))
	rivers.all4$inflow_discharge <- NA
	for (i in lake_id) {
		x <- which(rivers.all4$FID_poly_s == i)
		ids.x <- unique(which(rivers.all4$flow_to %in% x))
		rivers.all4$inflow_discharge[x] <- sum(rivers.all4$discharge[ids.x], na.rm = T)
		}
	rivers.all4$verweilzeiten <- rivers.all4$volume/rivers.all4$inflow_discharge #in seconds
	
	rivers.all4$verweilzeiten[which(is.infinite(rivers.all4$verweilzeiten))] <- 100 #lakes without inflow. but also have not input of plastics...
	
	summary(rivers.all4$verweilzeiten)
	save(rivers.all4, file = paste0(main.path, "temp_data/rivers_all4.Rdata"))
	rm(list = ls())
	