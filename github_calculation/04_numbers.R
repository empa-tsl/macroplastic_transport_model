#numbers for the paper
#### plot 02

#total masses outflows:

####
# plotting figures of the model; the plots might give you an idea for plotting; however, the actual results depend highly on your input data set.
# author: david mennekes, david.mennekes@empa.ch,
# march 2022
##################



main.path <- "PhD/mennekes2.0/"# library packages

library(reshape)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(patchwork) #for making all images the same dimensions.
library(cowplot)
library(readxl)


#load data

polymers <- c("EPS", "PP", "PS", "LDPE", "HDPE", "PVC", "PET")

base <- "_base"
rounds <- "801"

load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, base, ".Rdata"))
compartments <- c("outflow", "sediment_tot", "accumulation_tot", "removal_tot")

# load for each variable a one data frame

# overall contamination switzerland, figure with overall burial, accumulation, sedimentation and plastics in water
#create dataframe


### load data in water
df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")


#load data in water
for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_base.Rdata"))
	df01[, mat] <- s_r2[ , paste0("water_perM_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_water_perM <- df01
df_water_perM$sum <- rowSums(df_water_perM[ , polymers])
tail(df_water_perM)
df_water_perM[c(441454,441462), "sum"]*60*60*24*365/1000
s_r[s_r$outflow!=0, ]


### load data removal tot
df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_base.Rdata"))
	df01[, mat] <- s_r2[ , paste0("removal_tot_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_removal_tot <- df01
df_removal_tot$sum <- rowSums(df_removal_tot[ , polymers])

### load data clean tot
df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_base.Rdata"))
	df01[, mat] <- s_r2[ , paste0("clean_tot_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_clean_tot <- df01
df_clean_tot$sum <- rowSums(df_clean_tot[ , polymers])


### load data accumulation tot
df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_base.Rdata"))
	df01[, mat] <- s_r2[ , paste0("accumulation_tot_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_accumulation_tot <- df01
df_accumulation_tot$sum <- rowSums(df_accumulation_tot[ , polymers])


### load data temp storage

df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_base.Rdata"))
	df01[, mat] <- s_r2[ , paste0("sediment_tot_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_sediment_tot <- df01
df_sediment_tot$sum <- rowSums(df_sediment_tot[ , polymers])


### load data for control (scenario Null)
df01 <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 4))) #*2 for scenarios
names(df01) <- c(polymers, "flow_to", "isLake", "id_all", "outflow")

for (mat in polymers) {
	load(paste0(main.path, "output_files/rdata/lakes_single/cont_", mat,"_", rounds, "_null.Rdata"))
	df01[, mat] <- s_r2[ , paste0("water_perM_", mat, "_MaP")]
	df01[ , c("flow_to", "isLake", "id_all", "outflow")] <- s_r2[ , c("flow_to", "isLake", "id_all", "outflow")]
}
rm(s_r2)
df_null <- df01
df_null$sum <- rowSums(df_null[ , polymers])




# get network of one river
#######################################
outflow_1_Rhine <- 441464
outflow_1_Rhone <- 441470

######### results for Rhine ##########
#find all rivers connected to the stream rhine
trib <- data.frame(flow_to <- s_r$flow_to,
									 id_all <- s_r$id_all)
trib$partof <- 0 #part of the tributaries
trib$partof[outflow_1_Rhine] <- 1 #all sections of the main river are selected
for (i in 1:700) {
	ids <- trib$id_all[trib$partof == 1] #get geo IDs with one
	trib$partof[trib$flow_to %in% ids] <- 1 #write 1 when the flow to refers to a an id with 1
}

ids_trib <- trib$id_all[trib$partof==1]

water_rhine <- melt(df_water_perM[outflow_1_Rhine, polymers])
water_rhine$value <- water_rhine$value*60*60*24*365/1000 #in kg / year
water_rhine$river <- "Rhine"


ids_trib_river <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == F])]
ids_trib_lakes <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == T])]


ezg_rhine <- data.frame(ausfluss = df_water_perM[outflow_1_Rhine, "sum"],
												acc_river = sum(df_accumulation_tot$sum[ids_trib_river], na.rm = T),
												acc_lake = sum(df_accumulation_tot$sum[ids_trib_lakes], na.rm = T),
												clean_river = sum(df_clean_tot$sum[ids_trib_river], na.rm = T),
												clean_lake = sum(df_clean_tot$sum[ids_trib_lakes], na.rm = T),
												rem = sum(df_removal_tot$sum[ids_trib], na.rm = T))
temp_storage_rhine = sum(df_sediment_tot$sum[ids_trib], na.rm = T) #is per second
ezg_rhine_melt <- melt(ezg_rhine)
ezg_rhine_melt$value <- ezg_rhine_melt$value*60*60*24*365/1000 #in kg per year


#control shows me: the null retention is the same value assuming up different values. all perfect!
tot_input_rhine <- df_null[outflow_1_Rhine, "sum"]
tot_input_rhine

ezg_rhine_melt$value_percent <- ezg_rhine_melt$value/ (tot_input_rhine*60*60*24*365/1000)*100
##

ezg_rhine_melt$color <- c("a", "b", "b", "c", "c", "d") #add factors for colors
ezg_rhine_melt$pattern <- c("x", "x", "y", "x", "y", "x")

ids_trib_rhine <- ids_trib

######### results for rhone ##########
#find all rivers connected to the stream rhone
trib <- data.frame(flow_to <- s_r$flow_to,
									 id_all <- s_r$id_all)
trib$partof <- 0 #part of the tributaries
trib$partof[outflow_1_Rhone] <- 1 #all sections of the main river are selected
for (i in 1:700) {
	ids <- trib$id_all[trib$partof == 1] #get geo IDs with one
	trib$partof[trib$flow_to %in% ids] <- 1 #write 1 when the flow to refers to a an id with 1
}

ids_trib <- trib$id_all[trib$partof==1]

water_rhone <- melt(df_water_perM[outflow_1_Rhone, polymers])
water_rhone$river <- "Rhône"
water_rhone$value <- water_rhone$value*60*60*24*365/1000


ids_trib_river <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == F])]
ids_trib_lakes <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == T])]



ezg_rhone <- data.frame(ausfluss = df_water_perM[outflow_1_Rhone, "sum"],
												acc_river = sum(df_accumulation_tot$sum[ids_trib_river], na.rm = T),
												acc_lake = sum(df_accumulation_tot$sum[ids_trib_lakes], na.rm = T),
												clean_river = sum(df_clean_tot$sum[ids_trib_river], na.rm = T),
												clean_lake = sum(df_clean_tot$sum[ids_trib_lakes], na.rm = T),
												rem = sum(df_removal_tot$sum[ids_trib], na.rm = T))
ezg_rhone_melt <- melt(ezg_rhone)
ezg_rhone_melt$value <- ezg_rhone_melt$value*60*60*24*365/1000 #in kg per year

#control shows me: the null retention is the same value assuming up different values. all perfect!
tot_input_rhone <- df_null[outflow_1_Rhone, "sum"]
tot_input_rhone

ezg_rhone_melt$value_percent <- ezg_rhone_melt$value/ (tot_input_rhone*60*60*24*365/1000)*100
##

ezg_rhone_melt$color <- c("a", "b", "b", "c", "c", "d") #add factors for colors
ezg_rhone_melt$pattern <- c("x", "x", "y", "x", "y", "x")


#### rbind both tables
ezg_rhine_melt$river <- "Rhine"
ezg_rhone_melt$river <- "Rhône"

ezg_rr <- rbind(ezg_rhine_melt, ezg_rhone_melt)
ezg_rr$color <- factor(ezg_rr$color, levels = c("a", "b", "c", "d"))

# ezg_rr$pattern <- fct_reorder(ezg_rr$pattern)






#### data for total masses
###################

######### results for all ##########
#find all rivers connected to the stream rhone

water_all <- melt(colSums(df_water_perM[df_water_perM$outflow %in% c(1,3), polymers]))
water_all <- cbind(variable = rownames(water_all), water_all)
water_all$value <- water_all$value*60*60*24*365/1000
water_all$river <- "all"




ezg_all <- data.frame(ausfluss = sum(df_water_perM[df_water_perM$outflow %in% c(1,3) , "sum"], na.rm = T),
											acc_river = sum(df_accumulation_tot$sum[df_accumulation_tot$isLake == F], na.rm = T),
											acc_lake = sum(df_accumulation_tot$sum[df_accumulation_tot$isLake == T], na.rm = T),
											clean_river = sum(df_clean_tot$sum[df_clean_tot$isLake == F], na.rm = T),
											clean_lake = sum(df_clean_tot$sum[df_clean_tot$isLake == T], na.rm = T),
											rem = sum(df_removal_tot$sum, na.rm = T))
ezg_all_melt <- melt(ezg_all)
ezg_all_melt$value <- ezg_all_melt$value*60*60*24*365/1000 #in kg per year

#control shows me: the null retention is the same value assuming up different values. all perfect!
tot_input_all <- sum(ezg_all, na.rm = T)
tot_input_all

ezg_all_melt$value_percent <- ezg_all_melt$value/ (tot_input_all*60*60*24*365/1000)*100
##

ezg_all_melt$color <- c("a", "b", "b", "c", "c", "d") #add factors for colors
ezg_all_melt$pattern <- c("x", "x", "y", "x", "y", "x")


#### rbind all tables
ezg_rhine_melt$river <- "Rhine *"
ezg_rhone_melt$river <- "Rhône *"
ezg_all_melt$river <- "all Swiss water bodies"

ezg_rr <- rbind(ezg_rhine_melt, ezg_rhone_melt, ezg_all_melt)
ezg_rr$color <- factor(ezg_rr$color, levels = c("a", "b", "c", "d"))



########################## calculation##########################
#total mass
total_mass <- colSums(df_null[df_null$outflow %in% c(1,3), c(polymers, "sum")])*60*60*24*365/1000000 #in tons /year
total_mass

rivers_outflow <- data.frame(mass_total = df_water_perM[df_water_perM$outflow %in% c(1,3), "sum"],
														 names = s_r$name_river[s_r$outflow %in% c(1,3)])
rivers_outflow$mass_kg_year <- rivers_outflow$mass_total*60*60*24*365/1000
rivers_outflow

rivers_outflow$mass_total[11]/ sum(rivers_outflow$mass_total)

rivers_outflow_null <- data.frame(mass_total = df_null[df_water_perM$outflow %in% c(1,3), "sum"]*60*60*24*365/1000000, #in tons /year
																	names = s_r$name_river[s_r$outflow %in% c(1,3)])
rivers_outflow_null


#percentage
rivers_outflow$mass_total / rivers_outflow_null$mass_total*100

library(sf)
breggia <- st_read("PhD/data/processed maps/breggia.gpkg")

plot(st_geometry(breggia))
br_km2 <- as.numeric(st_area(breggia)/1000000)

rhone <- st_read("PhD/data/processed maps/rhone.gpkg")
plot(st_geometry(rhone))
rh <- as.numeric(st_area(rhone)/100000)
rh / br_km2


####
load("PhD/mennekes2.0/temp_data/flow_files/round_801_PET_base.Rdata")
rivers.calc[93992, ]

sum(df_sediment_tot[ids_trib_rhine, "sum"])
length(p)
p <- (rivers.calc$actualacc_PET_WaterMaP_concMSV / rivers.calc$actualsed_PET_WaterMaP_concMSV*100)
length(which(p == 100))
rivers.calc[which(p == 100), "actualacc_PET_WaterMaP_concMSV"]

rivers.calc[ids_trib_river[823], ]

ezg_rr
ezg_rr %>% group_by(river, color) %>% summarise(sum(value))


#peak concentratin per km
df_acc_per_m <- df_accumulation_tot$sum /  rivers.calc$length_m
max(df_acc_per_m[df_sediment_tot$outflow == 0 & df_acc_per_m != 0])*60*60*24*365
summary(df_acc_per_m[df_sediment_tot$outflow == 0 & df_acc_per_m != 0])*60*60*24*365

#number of rivers that are contaminated
r.river <- !(rivers.calc$isLake)
r.river[rivers.calc$outflow!=0] <- F
r.lake <- rivers.calc$isLake

cont_river <- df_water_perM$sum[r.river] > 0
sum(cont_river)/sum(r.river)


#potential outflow null scenario
df_null[outflow_1_Rhine, "sum"] *60*60*24*365/1000
df_null[outflow_1_Rhone, "sum"] *60*60*24*365/1000


###############################
#find reason for change in Aare river at km a bit over 200

main.path <- "PhD/mennekes2.0/"
load(paste0(main.path, "temp_data/forPlot.Rdata"))

load(paste0(main.path, "temp_data/flow_files/print_round.Rdata"))
# extra_names <- c("_weir05", "_weir95")
rounds <- "801"
polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")

rhein <- 65062
doubs <- 329205
aare <- 79433
rhone <- 325768


d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()

d$x <- 0
d$x[aare] <- 1
ft <- d$flow_to[which(d$x == 1)]
ft_long <- aare
for (i in 1:800) {
	d$x[ft] <- 1
	ft_long <- c(ft_long, ft)
	ft <- d$flow_to[ft]
}
sum(d$x)

ft_rhine <- ft_long[1:sum(d$x)]

len <- as.numeric(st_length(rivers.all[ft_rhine, ]))
len_cum <- cumsum(len)



df01 <- as_tibble(matrix(nrow = 1, ncol = length(polymers)+2))
names(df01) <- c(polymers, "scenario", "x")

#for loading data
df_temp <- as_tibble(matrix(nrow = length(ft_rhine), ncol = length(polymers)+2))
names(df_temp) <- c(polymers, "scenario", "x")

#load data
for (scenario in extra_names) {
	df_temp$scenario <- scenario
	df_temp$x <- len_cum
	for (mat in polymers) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_801", scenario, ".Rdata"))
		df_temp[ , mat] <- s_r[ft_rhine, paste0("water_perM_", mat, "_MaP")]
	}
	df01 <- rbind(df01, df_temp)
}


df01 <- df01[2:nrow(df01), ]


df01$sum <- rowSums(df01[ , polymers])

#find max row for PET scenario _base
df01_PET <- df01 %>% filter(scenario == "_base")
  x<- df01_PET$x[which.max(df01_PET$PET)]
  id_aare <- which.max(df01_PET$PET)
  
ids_interest <- ft_rhine[c(id_aare-1, id_aare, id_aare+1)]
len_cum[c(id_aare-1, id_aare, id_aare+1)]
df01_PET$PET[c(id_aare-1, id_aare, id_aare+1)]*1000

load("PhD/mennekes2.0/temp_data/rivers_all6_base.Rdata")
#rivers.all6
names(rivers.all6)
fac_pet <- grepl(".fac_PET_", names(rivers.all6))

#find fac. data for interesting section
rivers.all6[ids_interest, fac_pet]


#find other weirs in aare river
w_aare <- which(rivers.all6$removal.fac_PET_MaP[ft_rhine] == 0.75) #all weirs in aare
length(w_aare) # number of dams in Aare: 17

len_cum[w_aare]/1000








#############################
#effect of different scenarios


polymers <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

main.path <- "PhD/mennekes2.0/"

#load extra names
load(paste0(main.path, "temp_data/flow_files/print_round.Rdata"))
rounds <- "801"	

# load(paste0(main.path, "output_files/rdata/cont_HDPE_", rounds, extra_names[1], ".Rdata"))
# rm(s_r)
scenarios <- extra_names

###df 02 for water
df02 <- data.frame(river = rep(NA, 3*length(polymers)),
									 polymer = NA)
for (i in scenarios) {
	df02[ , i] <- NA
}


counter <- 1
for (mat in polymers) {
	c2 <- 3
	for (j in scenarios) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, j, ".Rdata"))
		df02[counter:I(counter +2), "river"] <- c("rhine", "rhone", "Le doubs") 
		df02[counter:I(counter +2), "polymer"] <- mat
		df02[counter:I(counter +2), c2] <- (s_r[c(441464, 441470, 441468), paste0("water_perM_", mat, "_MaP")])
		c2 <- c2 +1
	}
	counter <- counter +3
}


df02
total_inputs_perS <- sum(s_r$inputKawecki_perS_PET_MaP)

df03 <- melt(df02, id.vars = c("river", "polymer")) %>% group_by(river, variable) %>% summarise(value = sum(value))

x.cat <- c("weir", "lake", "river", "sedimentation", "combined", "base")
df03$plot.cat <- NA

df03$plot.cat[df03$variable %in% c("_weir05", "_weir25", "_weir50", "_weir75", "_weir95")] <- x.cat[1]
df03$plot.cat[df03$variable %in% c("_baseLakesLinear", "_baseLakes05", "_baseLakes50", "_baseLakes95")] <- x.cat[2]
df03$plot.cat[df03$variable %in% c("_baseRivers", "_baseRiversnoResus")] <- x.cat[3]
df03$plot.cat[df03$variable %in% c("_LUlow", "_LUmid", "_LUhigh", "_Qlow", "_Qmid", "_Qhigh","_Slow" ,"_Smid", "_Shigh")] <- x.cat[4]
df03$plot.cat[df03$variable %in% c("_base")] <- "base"
df03$plot.cat[df03$variable %in% c("_null")] <- "null"
df03$plot.cat[df03$variable %in% c("_high_all", "_low_all")] <- "combined"
df03$plot.cat <- factor(df03$plot.cat, levels = x.cat)
df03


### rhine and rhone ###
#calcualte percentages:
# 1 - (value / _null)

df03a <- df03
null_rhine <- as.numeric(df03[df03$river == "rhine" & df03$variable == "_null", "value"]) #null scenario
null_rhone <- as.numeric(df03[df03$river == "rhone" & df03$variable == "_null", "value"])

df03a$value_percent <- NA
df03a$value_percent[df03a$river == "rhine"] <- 1 - (df03a$value[df03a$river == "rhine"] / null_rhine) #percentage for rhine
df03a$value_percent[df03a$river == "rhone"] <- 1 - (df03a$value[df03a$river == "rhone"] / null_rhone)

df03a$value_percent <- df03a$value_percent*100


df04 <- df03a %>% filter(river %in% c("rhone", "rhine")) %>% group_by(river, plot.cat) %>% summarise(mi = min(value_percent), ma = max(value_percent))

lbs <- c("weir *\n(5%, 25%, 50%,\n 75%, 95%)", "lake *\n(5%, 50%, 95%,\n 'surface A')", "LU / Q / S *\n(low, mid, high)", "resuspension\n(yes / no)", "combined\n (min / max of *)", "'best guess'")
pc <- c("weir", "lake", "sedimentation", "river", "combined", "base")
df04$plot.cat <- factor(df04$plot.cat, levels = pc, labels = lbs)
df03_sel <- df03a %>% filter(river %in% c("rhone", "rhine")) %>% filter(variable != "_null")
df03_sel$plot.cat <- factor(df03_sel$plot.cat, levels = pc, labels = lbs)



df03_sel %>% group_by(plot.cat) %>% summarise(sd(value_percent)) #noise within each group
df03_sel %>% group_by(plot.cat) %>% summarise(mean(value_percent)) #noise within each group
df03_sel %>% filter(!(variable %in% c("_Slow", "_LUlow", "_Slow"))) %>%  group_by(plot.cat) %>% summarise(sd(value_percent)) #take out low values for LU/q/S
df03_sel %>% filter(!(variable %in% c("_Slow", "_LUlow", "_Slow"))) %>%  group_by(plot.cat) %>% summarise(mean(value_percent)) #take out low values for LU/q/S


##########################
# compare with data by Boaz2023
Basel <- 93991
Koblenz <- 93979
Basel_model_all <- df_water_perM[Basel, "sum"] *60*60
Koblenz_model_all <- df_water_perM[Koblenz, "sum"] *60*60
Koblenz_model_POsoft <- rowSums(df_water_perM[Koblenz, c("LDPE", "PP")]) *60*60
Basel_model_POsoft <- rowSums(df_water_perM[Basel, c("LDPE", "PP")]) *60*60
Basel_model_POsoft/ Basel_model_all #portion of PO Soft
Koblenz_model_POsoft/ Koblenz_model_all #portion of PO Soft



#data hammerdirt

locations <-read_excel("PhD/text/paper/macroplastic paper/data hammerdirt/data_organized.xlsx", sheet = "locations") %>% filter(water == "r") %>% select("location")
loc <- locations$location
length(loc)
mass <-read_excel("PhD/text/paper/macroplastic paper/data hammerdirt/data_organized.xlsx", sheet = "survey mass data") %>% filter(location %in% loc)
nrow(mass)
mean(mass$mac_plast_w) #in gramms
mass$mac_plast_w / mass$length #length in m -> g/m
median(mass$mac_plast_w / mass$length) #length in m -> g/m


dup_loc <- unique(mass$location[duplicated(mass$location)]) #location with multiple measurements...
dup_loc_selected <- dup_loc[c(2, 3,4)]#select the two location at the aare river

mass %>% filter(location %in% dup_loc_selected) %>% mutate(map_per_m = mac_plast_w/length) %>% select(survey_key, date, map_per_m)


library(lubridate)
date("2021-02-21")- date("2020-08-08") #time difference 197 days for schusspark aare locations
date("2020-12-31")-date("2020-11-29")#time difference 32 days for spackmatt locations
date("2021-03-29")-date("2020-06-12") #time difference 290 days for limmat
