	#plot along ther rivers.
	
	# first all rivers that belong to the system
	
	library(ggplot2)
	library(dplyr)
	library(tidyverse)
	library(ggrepel)
	library(patchwork) #for making all images the same dimensions.
	library(cowplot)
	library(sf)
	library(reshape2)
	library(ggforce)
	
	#load dataset
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
	
	
	
	###rhein (Aare)
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
	
	#add catogories
	x.cat <- c("weir", "lake", "river", "sedimentation")
	
	df01$plot.cat <- NA
	df01$plot.cat[df01$scenario %in% c("_weir05", "_weir25", "_weir50", "_weir75", "_weir95")] <- x.cat[1]
	df01$plot.cat[df01$scenario %in% c("_baseLakesLinear", "_baseLakes05", "_baseLakes50", "_baseLakes95")] <- x.cat[2]
	df01$plot.cat[df01$scenario %in% c("_baseRivers", "_baseRiversnoResus")] <- x.cat[3]
	df01$plot.cat[df01$scenario %in% c("_LUlow", "_LUmid", "_LUhigh", "_Qlow", "_Qmid", "_Qhigh","_Slow" ,"_Smid", "_Shigh")] <- x.cat[4]
	df01$plot.cat[df01$scenario %in% c("_base")] <- "base"
	df01$plot.cat[df01$scenario %in% c("_null")] <- "null"
	

	#row sums
	df01$sum <- rowSums(df01[ , polymers])
	
	# add isLake to file
	df01$isLake <- rep(s_r$isLake[ft_rhine], length(extra_names))
	
	
	# add information for lakes <- start end of a lake for plotting
	lakes <- s_r$isLake[ft_rhine]
	start_end <- data.frame(start = rep(NA, length(lakes)),
													end = NA)
	counter <- 1
	y <- F
	#if first segment is lake dann muss änderung auch auf T gesetzt werden
	if(lakes[1]){
		y <- T
	}
	
	for (i in 1:I(length(lakes)-1)) {
		x <- lakes[i]# check lake für  Runde
		if(x == T & y == T){ #x ist für lake, y überprüft, ob es eine änderung gab
			start_end$start[counter] <- len_cum[i]-100 #minus 100m
		}
		if(x == F & y == T){ #x = F bedeuted es ist kein lake, d. h. es ist fluss. Wenn dann noch von see auf fluss geändert hat, dann ist der See vorbei
			start_end$end[counter] <- len_cum[i] - 100
			counter <- counter +1
		}
		y = lakes[i] != lakes[i+1]
	}
	
	start_end <- start_end[!(is.na(start_end$end)), ]
	
	df02 <- df01 %>% filter(isLake == F)
	#delete rows that are lakes.
	
	
	#calculate change towards previous
	df02$diff <- NA #create row for saving changes
	df02$i <- c(NA, df02$sum[1:I(nrow(df02)-1)])
	df02$i_1 <- c(NA, df02$sum[2:nrow(df02)])
	df02$diff <- df02$i_1 / df02$i
	df02$diff[which(df02$x == min(df02$x, na.rm = T))] <- 1 # first timestep of river aare will be no changes -> avoid overhanging due to dataframe structure. search for min distance to find first segment- set changes to 1 which equals no change
	
	#change from 0 to any positive values causes inf values (x/0) -> replace with 1
	df02[which(is.infinite(df02$diff)), "diff"] <- 1
	df02$diff
	
	# replace diff = 1 for values of 0
	df02[is.nan(df02$diff) & df02$i == 0 & df02$i_1 == 0, "diff"] <- 1
	
	df02[is.na(df02$diff) & df02$isLake == F, ]#after lakes the first section.. keep NA value
	
	df02$diff_max3 <- df02$diff
	df02$diff_max3[df02$diff_max3 >3] <- 3
	df02$sum[df02$isLake == T] <- NA
	
	#for plotting
	
	
	#colors
	blue <- "dodgerblue"
	red <- "red3"
	black <- "black"
	yellow <- "orange"
	

	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 9),
					axis.text.x = element_text(color = "black", face = "plain", size = 9),
					axis.text.y = element_text(color = "black", face = "plain", size = 9),
					panel.background = element_rect(fill = "transparent"),
					plot.subtitle = element_text(size = 9),
					panel.grid = element_blank()) #trbl
	
	
	p1 <- ggplot(df02 %>% filter(!(scenario %in% c("_null", "_base"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_point(alpha = 0.3, pch = 16)+
		geom_point(data = df02 %>% filter(scenario == "_base"), aes(x = x/1000, y = diff_max3, color = scenario))+
		scale_color_manual("", values = c("_baseLakes05" = blue, "_baseLakes50" = blue, "_baseLakes95" = blue, "_baseLakesLinear" = blue, "_baseRivers" = blue, "_baseRiversnoResus" = blue, 
																			"_LUhigh" = red, "_LUlow" = red, "_LUmid" = red, "_Qhigh" = red, "_Qmid" = red, "_Qlow" = red, "_Shigh" = red, "_Smid" = red, "_Slow" = red, 
																			"_weir05" = black, "_weir25" = black, "_weir50" = black, "_weir75" = black, "_weir95" = black, "_base" = yellow))+
		nice+
		labs(x = "river lengths in [km]",
				 y = "change between\ntwo segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0,0,0.01,0))+
		theme(axis.title.x = element_blank())+
		geom_vline(xintercept = 200)+
		geom_vline(xintercept = 250)
		# facet_zoom(xlim = c(200,250))

	
	p1	

	ggsave("PhD/mennekes2.0/output_files/plots/aenderung_aare.png", plot = p1,  width = 12, height = 8, units = "cm")
	
	df03 <- cbind(df02, zoom = FALSE)#won´t be zoomed
	df04 <- cbind(df02, zoom = TRUE) #will be zoomed
	
	
	#version with zoom
	p1zoom <- ggplot(df03 %>% filter(!(scenario %in% c("_null", "_base"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_point(alpha = 0.3, pch = 16, size = 0.8)+
		geom_point(data = df03 %>% filter(scenario == "_base"), aes(x = x/1000, y = diff_max3, color = scenario), size = 1)+
		geom_line(data = df04 %>% filter((scenario %in% c( "_weir05", "_weir95", "_baseLakes05", "_baseLakes95", "_Qlow", "_Qhigh"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_line(data = df04 %>% filter(scenario== "_base"), aes(x = x/1000, y = diff_max3, color = scenario), linewidth = 0.9)+
		geom_point(data = df04 %>% filter((scenario %in% c( "_weir05", "_weir95", "_baseLakes05", "_baseLakes95", "_Qlow", "_Qhigh", "_base"))), aes(x = x/1000, y = diff_max3, color = scenario), alpha = 0.5, pch = 16, size = 1.3)+
		scale_color_manual("", values = c("_baseLakes05" = blue, "_baseLakes50" = blue, "_baseLakes95" = blue, "_baseLakesLinear" = blue, "_baseRivers" = blue, "_baseRiversnoResus" = blue, 
																			"_LUhigh" = red, "_LUlow" = red, "_LUmid" = red, "_Qhigh" = red, "_Qmid" = red, "_Qlow" = red, "_Shigh" = red, "_Smid" = red, "_Slow" = red, 
																			"_weir05" = black, "_weir25" = black, "_weir50" = black, "_weir75" = black, "_weir95" = black, "_base" = yellow))+
		nice+
		labs(x = "river lengths in [km]",
				 y = "change between\ntwo segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0.01,0,0.01,0), breaks = c(0,1,2,3), labels = c("0", "1", "2", ">3"))+
		theme(axis.title.x = element_blank())+
		facet_zoom(xlim = c(200,250), zoom.data = zoom, zoom.size = 0.7)
	
	
	p1zoom	
	
	ggsave("PhD/mennekes2.0/output_files/plots/aenderung_aare_zoom.png", plot = p1zoom,  width = 12, height = 8, units = "cm")
	
	
	
	#mit verschiedenen Scenarien
	#colors
	blue <- c('#deebf7','#9ecae1','#3182bd')
	red <- c('#fee0d2','#fc9272','#de2d26')
	black <- c('#f7f7f7','#cccccc','#969696','#636363','#252525')
	
	
	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 9),
					axis.text.x = element_text(color = "black", face = "plain", size = 9),
					axis.text.y = element_text(color = "black", face = "plain", size = 9),
					panel.background = element_rect(fill = "transparent"),
					plot.subtitle = element_text(size = 9),
					panel.grid = element_blank()) #trbl
	
	
	p2 <- ggplot(df02 %>% filter(!(scenario %in% c("_base", "_null"))) %>% filter(plot.cat == "lake"), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_point(alpha = 0.8, pch = 16)+
		scale_color_manual("", values = c("_baseLakes05" = blue[1], "_baseLakes50" = blue[2], "_baseLakes95" = blue[3], "_baseLakesLinear" = blue[2], "_baseRivers" = blue[2], "_baseRiversnoResus" = blue[2], 
																			"_LUhigh" = red[3], "_LUlow" = red[1], "_LUmid" = red[2], "_Qhigh" = red[3], "_Qmid" = red[2], "_Qlow" = red[1], "_Shigh" = red[3], "_Smid" = red[2], "_Slow" = red[1], 
																			"_weir05" = black[1], "_weir25" = black[2], "_weir50" = black[3], "_weir75" = black[4], "_weir95" = black[5]))+
		nice+
		labs(x = "river lengths in [km]",
				 y = "change between\ntwo segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0,0,0.01,0), limits = c(0,3), breaks = c(0,1,2,3), labels = c("0", "1", "2", ">3"))
	
	
	p2	
	ggsave("PhD/mennekes2.0/output_files/plots/aenderung_aare2.png", plot = p2, width = 12, height = 8, units = "cm")
	
	
	
	
	#as line between 200 and 300 km for selected scenarios
	blue <- "dodgerblue"
	red <- "red3"
	black <- "black"
	yellow <- "orange"
	
	
	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 10),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size = 10),
					panel.background = element_rect(fill = "transparent"),
					plot.subtitle = element_text(size = 10),
					panel.grid = element_blank()) #trbl
	
	
	p3 <- ggplot(df02 %>% filter((scenario %in% c( "_weir05", "_weir95", "_baseLakes05", "_baseLakes95", "_Qlow", "_Qhigh"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_line(alpha = 0.8)+
		geom_line(data = df02 %>% filter(scenario== "_base"), aes(x = x/1000, y = diff_max3, color = scenario), linewidth = 1)+
		scale_color_manual("", values = c("_baseLakes05" = blue,  "_baseLakes95" = blue,
																			 "_Qhigh" = red,  "_Qlow" = red,
																			"_weir05" = black,  "_weir95" = black,
																			"_base" = yellow))+
		nice+
		labs(x = "river lengths in [km]",
				 y = "change between\ntwo segments [-]")+
		scale_x_continuous(expand = c(0,0), limits = c(200, 250))+
		scale_y_continuous(expand = c(0,0,0.01,0), limits = c(0,3), breaks = c(0,1,2,3), labels = c("0", "1", "2", ">3"))
	
	
	p3	
	ggsave("PhD/mennekes2.0/output_files/plots/aenderung_aare_km200_250.png", plot = p3, width = 12, height = 8, units = "cm")
	
	#legend for colors
	blue <- "dodgerblue"
	red <- "red3"
	black <- "black"
	yellow <- "orange"
	
	pL1 <- ggplot(df02 %>% filter((scenario %in% c("_baseLakes05", "_LUhigh", "_weir05", "_base"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_point( pch = 16)+
		scale_color_manual("", values = c("_baseLakes05" = blue, "_LUhigh" = red, 
																			"_weir05" = black, "_base" = yellow), labels = c('"best guess" scenario', "lake scenarios", "LU, Q, S scenarios", "weirs scenarios"))+
		nice+
		theme(legend.key = element_blank(),
					legend.key.width = unit(0.35, "cm"))+
		labs(x = "river lengths in [km]",
				 y = "change between two segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0,0,0.01,0), breaks = c(0,1,2,3), labels = c("0", "1", "2", ">3"))+
		theme(legend.position = "right")
	
	
	pL1	
	
	le <- get_legend(pL1)
	
	pL1 <- ggplot(df02 %>% filter((scenario %in% c("_baseLakes05", "_LUhigh", "_weir05", "_base"))), aes(x = x/1000, y = diff_max3, color = scenario))+
		geom_hline(yintercept = 1, color = "grey80")+
		geom_line()+
		scale_color_manual("", values = c("_baseLakes05" = blue, "_LUhigh" = red, 
																			"_weir05" = black, "_base" = yellow), labels = c('"best guess" scenario', "lake scenarios", "LU, Q, S scenarios", "weirs scenarios"))+
		nice+
		theme(legend.key = element_blank(),
					legend.key.width = unit(0.35, "cm"))+
		labs(x = "river lengths in [km]",
				 y = "change between two segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0,0,0.01,0), breaks = c(0,1,2,3), labels = c("0", "1", "2", ">3"))+
		theme(legend.position = "right")
	
	
	pL1	
	
	le3 <- get_legend(pL1)
	
	#legend for grey
	pL2 <- ggplot(df02 %>% filter((scenario %in% c("_baseLakes05"))), aes(x = x/1000, y = diff_max3, fill = scenario))+
		geom_area()+
		scale_fill_manual("", values = c("_baseLakes05" = "grey90", "_LUhigh" = red, 
																			"_weir05" = black, "_base" = yellow), labels = c("macroplastic emission\nin rivers (accumulated)*", "lake scenarios", "LU, Q, S scenarios", "weirs scenarios"))+
		nice+
		theme(legend.key = element_blank(),
					legend.key.width = unit(0.4, "cm"))+
		labs(x = "river lengths in [km]",
				 y = "change between two segments [-]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0,0,0.01,0))+
		theme(legend.position = "right")
	
	
	pL2	
	
	le2 <- get_legend(pL2)
	
	
	
	
	# Verlauf dargestellt als total mass in each segment
	#colors
	blue <- "dodgerblue"
	red <- "red3"
	black <- "black"
	yellow <- "orange"
	
	
	p4 <- ggplot(df02 %>% filter(!(scenario%in% c("_null", "_base"))), aes(x = x/1000, y = sum, color = scenario))+
		geom_area(data = df02 %>% filter(scenario == "_null"), aes(x = x/1000 , y = sum), color = "grey90", fill = "grey90")+
		geom_line(alpha = 0.7)+
		geom_line(data = df02 %>% filter(plot.cat == "sedimentation"), aes(x = x/1000, y = sum, color = scenario), linewidth = 0.55)+
		geom_line(data = df02 %>% filter(scenario == "_base"), aes(x = x/1000, y = sum, color = scenario), linewidth = 0.7)+
		scale_color_manual("", values = c("_baseLakes05" = blue, "_baseLakes50" = blue, "_baseLakes95" = blue, "_baseLakesLinear" = blue, "_baseRivers" = blue, "_baseRiversnoResus" = blue, 
																			"_LUhigh" = red, "_LUlow" = red, "_LUmid" = red, "_Qhigh" = red, "_Qmid" = red, "_Qlow" = red, "_Shigh" = red, "_Smid" = red, "_Slow" = red, 
																			"_weir05" = black, "_weir25" = black, "_weir50" = black, "_weir75" = black, "_weir95" = black, "_base" = yellow))+
		nice+
		labs(x = "river lengths in [km]",
				 y = "macroplastic mass\n in suspension in [g / s]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0.01,0,0.01,0), breaks = c(0,1,2))+
		theme(axis.title.x = element_blank())
	
	
	p4
	
	abcde <- df02 %>% filter(plot.cat == "sedimentation") %>% filter(sum!=0) %>% group_by(scenario) %>%   summarise(mean(sum),sd(sum), max(sum))
	abcde
	mean(abcde$`max(sum)`)
	min(abcde$`max(sum)`)
	df02 %>% filter(plot.cat == "base") %>% filter(sum!=0) %>%  summarise(mean(sum),sd(sum), max(sum))
	unique(df02$plot.cat)
	#find dams in Aare river
	load("PhD/mennekes2.0/temp_data/rivers_all6_base.Rdata")
	#rivers.all6
	fac_pet <- grepl(".fac_PET_", names(rivers.all6))
	
	#find fac. data for interesting section
	
	
	#find other weirs in aare river
	w_aare <- which(rivers.all6$removal.fac_PET_MaP[ft_rhine] != 0) #all weirs in aare
	length(w_aare) # number of dams in Aare: 17

	#colors of polymers for best guess plot
	df20 <- df02 %>% filter(scenario == "_base") %>% select(c(polymers,  "x"))
	df21 <- melt(df20, id.vars = c("x"), measure.vars = polymers)
	farben <- rev(c("#cc5700", "#c97544", "#bf9077", "#aaaaaa", "#92a1c7", "#6f98e3", "#1e90ff"))
	
	p6 <- ggplot(df21)+
		geom_rect(data = start_end,
							aes(xmin = start/1000, xmax = end/1000),
							ymin = -Inf, ymax = Inf, alpha = 0.15, fill ="#555555")+
		geom_vline(xintercept = len_cum[w_aare]/1000, linetype = "dashed", linewidth = 0.3)+
		geom_area(aes(x = I(x/1000), y = value*1000, fill = variable))+
		scale_y_continuous(expand = c(0,0))+
		scale_x_continuous(expand = c(0,0))+
		scale_fill_manual(values = farben)+
		labs(x = "distance [km]",
				 y = "macroplastic mass\nin suspension [mg/s]\n ")+
		annotate("label",y = 37, x = 10, label = "Aare River", hjust = "left", vjust = "left",  size = 2.5, color = "black")+
		annotate("label",y = 37, x = I(len_cum[577]/1000+5), label = "Rhine River", hjust = "left", vjust = "left",  size = 2.5, color = "black")+
		nice
	
	p6
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
		theme(plot.margin=unit(c(1,0,1,0),"cm"),
					legend.position = "none",
					axis.text.x = element_blank(),
					panel.border = element_blank(),
					panel.ontop = F,
					plot.subtitle = element_text(size = 8))
	
	legende
	
							
	# input emission based on data by Kawecki and Nowack 2020
	#load input emissions
	df_temp <- as_tibble(matrix(nrow = length(ft_rhine), ncol = length(polymers)+1))
	names(df_temp) <- c(polymers, "x")
	df_temp$x <- len_cum
		for (mat in polymers) {
			load(paste0(main.path, "output_files/rdata/cont_", mat,"_801_null.Rdata"))
			df_temp[ , mat] <- s_r[ft_rhine, paste0("inputKawecki_perS_", mat, "_MaP")]
		}

	df10 <- df_temp	
	df10$sum <- rowSums(df10[ , polymers])
	df10b <- data.frame(x = df10$x[2: nrow(df10)]-1,
											sum = df10$sum[1: I(nrow(df10) - 1)])
	df11 <- rbind(df10[ , c("x", "sum")],df10b)
	
	
	
	p5 <- ggplot(df11, aes(x = x/1000, y = sum))+
		geom_area(fill= "grey")+
		nice+
		labs(x = "river lengths in [km]",
				 y = "emission into\nwater in [g / s]")+
		scale_x_continuous(expand = c(0,1))+
		scale_y_continuous(expand = c(0.0,0,0.0,0), limits = c(0, 0.016), breaks = c(0, 0.008, 0.016))
	
	p5
	
	df
	head(s_r)



#empty plot
	nice2 <- theme(legend.position = "none",
								legend.background = element_rect(fill = "transparent", color = NA),
								plot.background = element_rect(fill = "transparent",colour = NA),
								text = element_text(color = "black", size = 12),
								legend.text = element_text(size = 8),
								plot.margin=unit(c(0.0, -0.7,0.1,-0.6),"cm"))
	t0 <- ggplot() +                     
		annotate("text",
						 x = 1,
						 y = 1,
						 size = 1,
						 label = "     ") + 
		theme_void()+
		nice2


#plots zusammenbauen

pall1 <- ggdraw(plot_grid(p3,t0, p1, p4, p5, align = "v", ncol = 1, rel_heights = c(0.8,0.2, 1,1,1), labels = c("a)", " ", "b)", "c)", "d)"), label_size = 10, label_x = 0.01))

pall2 <- ggdraw(plot_grid(pall1, plot_grid( t0, le, le2,t0, nrow = 4), nrow = 1, rel_widths = c(1,0.2)))
pall2

#save figures
ggsave(paste0(main.path, "output_files/plots/Aare_lengths01.png"), pall2, height = 17, width = 18, units = "cm", dpi = 500, bg = "transparent")

ggsave(paste0(main.path, "output_files/plots/Aare_lengths01.pdf"), pall2, height = 17, width = 18, units = "cm", dpi = 500, bg = "transparent")


#build everything with zoom figure
pall1 <- ggdraw(plot_grid(p1zoom,t0, p4, p5, align = "v", ncol = 1, rel_heights = c(2,0.1,1,1), labels = c("a)","", "b)", "c)"), label_size = 10, label_fontface = "plain", label_x = 0.01))

pall2 <- ggdraw(plot_grid(pall1, plot_grid( t0, le, le2,t0, nrow = 4), nrow = 1, rel_widths = c(1,0.25)))
# pall2



#version 2
pall1.2 <- ggdraw(plot_grid(p4, t0, p1zoom, align = "v", ncol = 1, rel_heights = c(1,0.1,1.4), labels = c("a)","", "b)"), label_size = 10, label_fontface = "plain", label_x = 0.0))
pall2.1 <- ggdraw(plot_grid(pall1.2, plot_grid( t0, le2, t0, le, t0, nrow = 5, rel_heights = c(0.2,1,0.01,1,2)), nrow = 1, rel_widths = c(1,0.25)))


#save figures
ggsave(paste0(main.path, "output_files/plots/Aare_lengths01_zoom.png"), pall2, height = 17, width = 17.8, units = "cm", dpi = 500, bg = "transparent")
ggsave(paste0(main.path, "output_files/plots/Aare_lengths02_zoom.png"), pall2.1, height = 12, width = 17.8, units = "cm", dpi = 500, bg = "transparent")

ggsave(paste0(main.path, "output_files/plots/Aare_lengths01_zoom.pdf"), pall2, height = 17, width = 17.8, units = "cm", dpi = 500, bg = "transparent")
ggsave(paste0(main.path, "output_files/plots/Aare_lengths02_zoom.pdf"), pall2.1, height = 17, width = 17.8, units = "cm", dpi = 500, bg = "transparent")


pall3 <- ggdraw(plot_grid(p6, t0, legende, rel_widths = c(1,0.08, 0.1), nrow = 1))
ggsave(paste0(main.path, "output_files/plots/Aare_lengths_polymers.png"), pall3, height = 6, width = 17.8, units = "cm", dpi = 500, bg = "transparent")

p5 <- p4+
	nice

pall4 <-ggdraw(plot_grid(p5, plot_grid(t0,le2,t0,le3, t0, rel_heights = c(0.1,0.3,0.01,1.2,0.1), ncol = 1), nrow = 1, rel_widths = c(1,0.27)))
ggsave(paste0(main.path, "output_files/plots/Aare_lengths_scenarios.png"), pall4, height = 6, width = 17.8, units = "cm", dpi = 500, bg = "transparent")

p1zoom <- p1zoom+nice
pall5 <-ggdraw(plot_grid(p1zoom, plot_grid(t0,le, t0, rel_heights = c(0.1,1.2,1.3), ncol = 1), nrow = 1, rel_widths = c(1,0.27)))
ggsave(paste0(main.path, "output_files/plots/SI_Aare_lengths_scenarios.png"), pall5, height = 10, width = 17.8, units = "cm", dpi = 500, bg = "transparent")

rm(list = ls())
# 
