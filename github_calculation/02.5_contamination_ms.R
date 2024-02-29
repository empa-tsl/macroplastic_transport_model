############
# add flow velocity and change concentration to m/s
# author: david mennekes, david.mennekes@empa.ch; 
# Januar 2022
############



# library
library(sf)

#path to sub-folders
setwd("~/")
main.path <- "PhD/mennekes/"
load(paste0(main.path, "temp_data/rivers_all4.Rdata"))
polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")


# 
rivers.all5 <- rivers.all4

rm(rivers.all4)

rivers.all5$outflow[is.na(rivers.all5$outflow)] <- 0

# length
rivers.all5$length_m	<- as.numeric(st_length(rivers.all5))

# set everything == 1 for lakes and outflows
rivers.all5$length_m[rivers.all5$isLake == T] <- 1
rivers.all5$length_m[rivers.all5$outflow != 0] <- 1

rivers.all5$flow_velocity[rivers.all5$isLake == T] <- 1
rivers.all5$flow_velocity[rivers.all5$outflow != 0] <- 1

rivers.all5$flow_velocity_type[rivers.all5$isLake == T] <- "other"
rivers.all5$flow_velocity_type[rivers.all5$outflow != 0] <- "other"


# transform data
cols <- which(grepl("sum_", names(rivers.all5))) #find important columns for transformation
rows <- which(rivers.all5$isLake == F & rivers.all5$outflow == 0)

rivers.all5$length_seconds <- rivers.all5$length_m/rivers.all5$flow_velocity # this factor can be used for multiply the concMSV (see below) and get masses per section

# contamination is in kg/km per year for rivers using Shape_length
# contamination is in kg/ha per year for lakes using Shape_area


#for rivers. change to contamination in g/m per second and multiplied with the velocity. Means it shows how much plastics will be passed on per second
for (i in 1:length(cols)) {
	a <- st_drop_geometry(rivers.all5[ , cols[i]])
	n <- paste0(names(rivers.all5)[cols[i]], "_concMSV") #is per velocity
	rivers.all5[ , n] <-  a # make new column
	rivers.all5[rows, n] <- a[ rows, 1]/(365*24*60*60)*rivers.all5$length_m[rows] #change from year to seconds /(365*24*60*60). The input is per km. here we assume that the all input will enter the segment just at the most upstream point at of the segment (a simplification). Thus, the input is multiplied by the river lengths. one should use the river lengths in km! because the input is per km. However, from per km to per m  = (/1000) but change from kg to g (*1000) -> no change; ange to total load per second assuming that the input and output per second is an equilibrium for each segment
	print(i)
}


#####
#for lakes, caution! here no flow velocity change!!!!

#rows with lakes
rows_lake <- which(rivers.all5$isLake == T)

for (i in 1:length(cols)) {
	a <- st_drop_geometry(rivers.all5[ , cols[i]])
	n <- paste0(names(rivers.all5)[cols[i]], "_concMSV")
	area_ha <- rivers.all5$area[rows_lake] / 10000 #change area of lakes in ha (contamination is in kg/ha)
	c_total <- a[ rows_lake, 1]* area_ha * 1000 # from kg / ha to g...  *1000, *area in ha... gives total contamination
	rivers.all5[rows_lake, n] <- (c_total / (365*24*60*60)) #change to input per second
	print(i)
}

#fill NAs with 0 concentration, all rivers that are underground...
for (i in 1:length(cols)) {
	na <- which(is.na(rivers.all5[ , cols[i]]))
	n <- paste0(names(rivers.all5)[cols[i]], "_concMSV")
	rivers.all5[na, n] <- 0
}


# for lakes change concentration to 0 only not for the first element per lake. otherwise lake concentration will overestimated when considering each section.

id_lakes <- unique(rivers.all5$FID_poly_s)[-is.na(unique(rivers.all5$FID_poly_s))] # numbers without "river" in lake are missing, without NA

cols_msv <- grep("_concMSV", names(rivers.all5))# find all columns with MSV contamination. For lakes this is total load in g per s

for (i in 1 : length(id_lakes)){
	rivers.i <- which(rivers.all5$FID_poly_s == id_lakes[i]) #per lake ID. which "river" section.
	if(length(rivers.i) == 1){ #if only one "river" section exists, go to next lake ID
		next
	}	
	rivers.all5[rivers.i[2:length(rivers.i)], cols_msv] <- 0 #if more than one section exists that make all
}


#mark the 15 biggest lakes
rivers.all5$lake15 <- F
lakes_areas <- sort(unique(rivers.all5$area), decreasing = T)
rivers.all5$lake15[which(rivers.all5$area>lakes_areas[16])] <- T

#create concentrations

save(rivers.all5, file = paste0(main.path, "temp_data/rivers_all5.Rdata"))
# st_write(rivers.all5, paste0(main.path, "temp_data/rivers_all5.gpkg"), append = F)
rm(list = ls())	
