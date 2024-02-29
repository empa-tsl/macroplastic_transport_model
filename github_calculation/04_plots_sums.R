	#### plot 02
	
	#total masses outflows:
	
	####
	# plotting figures of the model
	# author: david mennekes, david.mennekes@empa.ch, 
	# march 2022
	##################
	
	
	setwd("~/")
	main.path <- "PhD/mennekes2.0/"# library packages
	
	library(reshape)
	library(ggplot2)
	library(dplyr)
	library(tidyverse)
	library(ggrepel)
	library(patchwork) #for making all images the same dimensions.
	library(cowplot)
	
	
	
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
	outflow_1_Aare_top <- 79433
	
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
	
	######### results for rhone ##########
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
	
	
	
	
	
	#dataframe all
	
	
	### rbind dataframes
	
	df_water_perPolymer <- rbind(water_rhine, water_rhone, water_all)
	
	##### plot
	
	#total outflow of Macroplastic
	
	nice <- theme_classic()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 10),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size =10),
					legend.text = element_text(color = "black", face = "plain", size = 8),
					axis.title.x = element_blank(), 
					axis.title.y = element_text(size = 10),
					axis.ticks.x = element_blank(),
					panel.background = element_rect(fill = "transparent")) #trbl
	
	
	farben <- rev(c("#cc5700", "#c97544", "#bf9077", "#aaaaaa", "#92a1c7", "#6f98e3", "#1e90ff"))
	
	p01 <- ggplot(data=df_water_perPolymer, aes(x=river, y=value, fill=variable)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0,0,10),breaks = c(0, 1000,  2000))+
		scale_x_discrete(labels = c("all cross border flows\nincl. to unknown", "Rhine\n(border Germany)", "Rhône\n(border France)" ), expand = c(0.2,0.1))+
		labs(y = "total macroplastic outflow  \nin suspension [kg / year]  \n ")+
		scale_fill_manual("",
											values = farben)+
		nice +
		theme(plot.margin=unit(c(0.6,0.6,0.3,0.2),"cm")) #trbl
	
	
	
	p01
	
	#legende
	l <- data.frame(value = rev(c(1,1,1,1,1,1,1)),
									river = "Legend\n",
									polymer = c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET"),
									pos = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5))
	l$polymer <- factor(l$polymer,
											levels = c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET"))
	
	legende <- ggplot(data=l, aes(x=river, y=value, fill=polymer)) +
		geom_bar(stat="identity")+
		scale_y_continuous(expand = c(0,0.1,0, 0))+
		scale_x_discrete( expand = c(0.4,0.1))+
		geom_text(aes(y = pos, label = rev(polymer)), color = c( "black"),size = 2)+
		labs(y = "MiP\nin [kg / year]")+
		scale_fill_manual("",
											values = farben)+
		theme_void()+
		theme(plot.margin=unit(c(1.5,0,0.5,0),"cm"),
					legend.position = "none",
					axis.text.x = element_blank(),
					panel.border = element_blank(),
					panel.ontop = F,
					plot.subtitle = element_text(size = 7))
	
	legende
	
	
	############################ plot 2
	
	farben <- c('dodgerblue',  "red3", '#555555','#cccccc')
	
	library(ggpattern)
	
	p02 <- ggplot(data = ezg_rr, aes(x = river, y = value_percent))+
		geom_bar_pattern(stat = "identity", aes(pattern = variable, fill = color), pattern_density = .1, pattern_spacing = 0.03, pattern_fill = "black", pattern_color = NA)+
		scale_pattern_manual("", values = c(ausfluss = "none",acc_river = "none", clean_river = "none", rem = "none", acc_lake = "crosshatch", clean_lake = "crosshatch"))+
		scale_fill_manual("", values = c("a" = farben[1], "b" = farben[3], "c" = farben[4], "d" = farben[2]))+
		scale_y_continuous(expand = c(0,0,0,0.1), n.breaks = 3)+
		scale_x_discrete(expand = c(0.2,0.1))+
		nice+
		labs(y = "distribution of macroplastic\nin catchment [%]")+
		theme(plot.margin=unit(c(0.7,0.6,0,0.2),"cm"))
	
	
	p02
	
	p02L <- ggplot(data = ezg_rr, aes(x = river, y = value_percent))+
		geom_bar(stat = "identity", aes(fill = color))+
		# scale_pattern_manual("", values = c(ausfluss = "none",acc_river = "none", clean_river = "none", rem = "none", acc_lake = "crosshatch", clean_lake = "crosshatch"))+
		scale_fill_manual("", values = c("a" = farben[1], "b" = farben[3], "c" = farben[4], "d" = farben[2]), labels = c("outflow in suspension", "accumulated", "cleaned", "removed\n(hydro power plant)"))+
		scale_y_continuous(expand = c(0,0,0,0.1))+
		nice+
		theme(legend.position = "right",
					legend.title = element_blank())+
		labs(y = "distribution in [%]")
	p02L
	l021 <- get_legend(p02L)
	
	
	p02L2 <- ggplot(data = ezg_rr[2:3, ], aes(x = river, y = value_percent))+
		geom_bar_pattern(stat = "identity", aes(pattern = variable), pattern_density = .1, pattern_spacing = 0.03, pattern_fill = "black", pattern_color = NA, fill = NA, color = "grey50")+
		scale_pattern_manual("", values = c(ausfluss = "none",acc_river = "none", clean_river = "none", rem = "none", acc_lake = "crosshatch", clean_lake = "crosshatch"), labels = c(" - in rivers", " - in lakes"))+
		# scale_fill_manual("", values = c("a" = farben[1], "b" = farben[3], "c" = farben[4], "d" = farben[2]), labels = c("outflow in suspension", "accumulated", "cleaned", "removed (rivers)"))+
		scale_y_continuous(expand = c(0,0,0,0.1))+
		nice+
		theme(legend.position = "right",
					legend.title = element_blank())+
		labs(y = "distribution\nof macroplastic in [%]")
	
	
	p02L2
	l022 <- get_legend(p02L2)
	
	
	#empty plot
	
	pe <- ggplot() +                     
		annotate("text",
						 x = 1,
						 y = 1,
						 label = "       ") + 
		theme_void()+
		theme(plot.margin = unit(c(0,0,0,0.9), "cm"),
					text = element_text(face = "plain", size = 11))
	
	### built plot
	#put all together
	p2legends <- ggdraw(plot_grid(l021, l022, ncol = 1, rel_heights = c(1,0.5), align = "v"))
	p2legends
	
	
	pall <- ggdraw(plot_grid(plot_grid(p01, p02, nrow = 2, align = "v", labels = c("a)", "b)"), label_size = 10, label_fontface = "plain", label_x = 0.15), pe, plot_grid(plot_grid(legende,  pe, rel_widths = c(1,2), nrow = 1), p2legends, nrow = 2), nrow = 1, rel_widths = c(1,0.1,0.3)))
	
	ggsave(paste0(main.path, "output_files/plots/outflow.png"), pall, height = 10, width = 17.8, units = "cm", dpi = 500, bg = "transparent")
	ggsave(paste0(main.path, "output_files/plots/outflow.pdf"), pall, height = 10, width = 17.8, units = "cm", dpi = 500, bg = "transparent")
	pall
	
	
	#outflows Aaare catchment##################
	
	#from top to bottom unit confluence with Rhine
	outflow_1_Aare <- 178358
	
	
	
	trib <- data.frame(flow_to <- s_r$flow_to,
										 id_all <- s_r$id_all)
	trib$partof <- 0 #part of the tributaries
	trib$partof[outflow_1_Aare] <- 1 #all sections of the main river are selected
	for (i in 1:700) {
		ids <- trib$id_all[trib$partof == 1] #get geo IDs with one
		trib$partof[trib$flow_to %in% ids] <- 1 #write 1 when the flow to refers to a an id with 1
	}
	
	ids_trib <- trib$id_all[trib$partof==1]
	
	water_aare <- melt(df_water_perM[outflow_1_Aare, polymers])
	water_aare$value <- water_aare$value*60*60*24*365/1000 #in kg / year
	water_aare$river <- "Aare"
	
	
	ids_trib_river <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == F])]
	ids_trib_lakes <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == T])]
	
	
	ezg_aare <- data.frame(ausfluss = df_water_perM[outflow_1_Aare, "sum"],
													acc_river = sum(df_accumulation_tot$sum[ids_trib_river], na.rm = T),
													acc_lake = sum(df_accumulation_tot$sum[ids_trib_lakes], na.rm = T),
													clean_river = sum(df_clean_tot$sum[ids_trib_river], na.rm = T),
													clean_lake = sum(df_clean_tot$sum[ids_trib_lakes], na.rm = T),
													rem = sum(df_removal_tot$sum[ids_trib], na.rm = T))
	temp_storage_aare = sum(df_sediment_tot$sum[ids_trib], na.rm = T) #is per second
	ezg_aare_melt <- melt(ezg_aare)
	ezg_aare_melt$value <- ezg_aare_melt$value*60*60*24*365/1000 #in kg per year
	
	#control shows me: the null retention is the same value assuming up different values. all perfect!
	tot_input_aare <- df_null[outflow_1_Aare, "sum"]
	tot_input_aare
	
	ezg_aare_melt$value_percent <- ezg_aare_melt$value/ (tot_input_aare*60*60*24*365/1000)*100
	##
	
	ezg_aare_melt$color <- c("a", "b", "b", "c", "c", "d") #add factors for colors
	ezg_aare_melt$pattern <- c("x", "x", "y", "x", "y", "x")
	
	
	#aare only along the main river
	
	trib <- data.frame(flow_to <- s_r$flow_to,
										 id_all <- s_r$id_all)
	d <- trib 
	names(d) <- c("flow_to", "id_all")
	d$x <- 0
	d$x[outflow_1_Aare_top] <- 1
	ft <- d$flow_to[which(d$x == 1)]
	ft_long <- outflow_1_Aare_top
	for (i in 1:800) {
		d$x[ft] <- 1
		ft_long <- c(ft_long, ft)
		ft <- d$flow_to[ft]
	}
	sum(d$x)
	
	ids_trib <- ft_long[1:sum(d$x)]
	
	
	water_aare <- melt(df_water_perM[outflow_1_Aare, polymers])
	water_aare$value <- water_aare$value*60*60*24*365/1000 #in kg / year
	water_aare$river <- "Aare"
	
	
	ids_trib_river <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == F])]
	ids_trib_lakes <- ids_trib[which(ids_trib%in%s_r$id_all[s_r$isLake == T])]
	
	
	ezg_aare <- data.frame(ausfluss = df_water_perM[outflow_1_Aare, "sum"],
												 acc_river = sum(df_accumulation_tot$sum[ids_trib_river], na.rm = T),
												 acc_lake = sum(df_accumulation_tot$sum[ids_trib_lakes], na.rm = T),
												 clean_river = sum(df_clean_tot$sum[ids_trib_river], na.rm = T),
												 clean_lake = sum(df_clean_tot$sum[ids_trib_lakes], na.rm = T),
												 rem = sum(df_removal_tot$sum[ids_trib], na.rm = T))
	temp_storage_aare = sum(df_sediment_tot$sum[ids_trib], na.rm = T) #is per second
	ezg_aare_melt <- melt(ezg_aare)
	ezg_aare_melt$value <- ezg_aare_melt$value*60*60*24*365/1000 #in kg per year
	
	#control shows me: the null retention is the same value assuming up different values. all perfect!
	tot_input_aare <- df_null[outflow_1_Aare, "sum"]
	tot_input_aare
	
	ezg_aare_melt$value_percent <- ezg_aare_melt$value/ (tot_input_aare*60*60*24*365/1000)*100
	##
	
	ezg_aare_melt$color <- c("a", "b", "b", "c", "c", "d") #add factors for colors
	ezg_aare_melt$pattern <- c("x", "x", "y", "x", "y", "x")
	
	ezg_aare_melt #only along the main river!! value_percent is for final value. does not work
	
	# rm(list = ls())
