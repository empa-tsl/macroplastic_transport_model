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
	library(ggspatial)
	library(scales)
	
	
	#function
	
	#round numbers and find groups:
	col.NR <- function(max.NR){
		if(is.infinite(max.NR)){
			return(c(0,0,0,0,0,0))
		}else{
			l <- log10(max.NR)
			if(l == 0){start.NR <- 1} #wenn max wert genau 1 ist
			if(l > 0){start.NR <- ceiling(max.NR/10^floor(l))*10^floor(l)}
			if(l < 0){start.NR <- ceiling(max.NR*10^ceiling(l*-1))/10^ceiling(l*-1)}
			return(c(start.NR,  start.NR*0.5, start.NR*0.25, start.NR*0.125, start.NR*0.0625, 0))
		}
	}
	
	col.NR2 <- function(values){ #enter value range
		max.NR <- max(values, na.rm = T)
		min.NR <- min(values[values>0], na.rm = T)
		#top of the range
		t <- log10(max.NR)
		if(t == 0){end.NR <- 1} #wenn max wert genau 1 ist
		if(t > 0){end.NR <- ceiling(max.NR/10^floor(t))*10^floor(t)}
		if(t < 0){end.NR <- ceiling(max.NR*10^ceiling(t*-1))/10^ceiling(t*-1)}
		#lower range
		l <- log10(min.NR)
		if(l == 0){start.NR <- 1} #wenn max wert genau 1 ist
		if(l > 0){start.NR <- floor(min.NR/10^floor(l))*10^floor(l)}
		if(l < 0){start.NR <- floor(min.NR*10^ceiling(l*-1))/10^ceiling(l*-1)}
		d <- diff(c(start.NR, end.NR)) #find differences between first and last after floor / ceiling
		if(d == 0){warning("no differences between min and max number")}
		else{
			med.NR <- median(values, na.rm = T) #find median of all numbers
			if(log10(med.NR)< 0 & d >= 1){dig.med <- ceiling(abs(log10(med.NR)))} #for big differences digits are based on med value
			if(log10(med.NR) < 0 & d < 1){dig.med <- ceiling(abs(log10(med.NR)))+ceiling(abs(log10(d)))} #if small differences ad extra digits
			if(log10(med.NR) >= 0 & d >= 10){dig.med <- 0}
			if(log10(med.NR) >= 0 & d < 10){dig.med <- ceiling(abs(log10(d)))}
			q3 <- round(med.NR, dig.med) #rounded median number
			
			#find median of first quater
			v <- values[values > q3]
			med.NR <- median(v, na.rm = T)
			if(log10(med.NR)< 0 & d >= 1){dig.med <- ceiling(abs(log10(med.NR)))} #for big differences digits are based on med value
			if(log10(med.NR) < 0 & d < 1){dig.med <- ceiling(abs(log10(med.NR)))+ceiling(abs(log10(d)))} #if small differences ad extra digits
			if(log10(med.NR) >= 0 & d >= 10){dig.med <- 0}
			if(log10(med.NR) >= 0 & d < 10){dig.med <- ceiling(abs(log10(d)))}
			q2 <- round(med.NR, dig.med) #rounded median number
			
			#find median of last quater
			v <- values[values < q3]
			med.NR <- median(v, na.rm = T)
			if(log10(med.NR)< 0 & d >= 1){dig.med <- ceiling(abs(log10(med.NR)))} #for big differences digits are based on med value
			if(log10(med.NR) < 0 & d < 1){dig.med <- ceiling(abs(log10(med.NR)))+ceiling(abs(log10(d)))} #if small differences ad extra digits
			if(log10(med.NR) >= 0 & d >= 10){dig.med <- 0}
			if(log10(med.NR) >= 0 & d < 10){dig.med <- ceiling(abs(log10(d)))}
			q4 <- round(med.NR, dig.med) #rounded median number
			
			
			
			
			return(c("q1" = end.NR, "q2" = q2 , "q3" = q3, "q4" = q4, "q5" = start.NR)) #return steps in equal steps
		}
	}
	
	#load dataset
	main.path <- "PhD/mennekes2.0/"
	load(paste0(main.path, "temp_data/forPlot.Rdata"))
	load(paste0("PhD/mennekes/output_files/rdata/cont_EPS_801_lake15.Rdata"))
	
	
	load(paste0(main.path, "temp_data/flow_files/print_round.Rdata"))
	# extra_names <- c("_weir05", "_weir95")
	rounds <- "801"
	polymers <- c("EPS", "PP", "LDPE", "HDPE", "PS", "PVC", "PET")
	
	rhein <- 65062
	doubs <- 329205
	aare <- 79433
	rhone <- 325768
	
	WWTP_ZH <- 14454
	WWTP_BE<- 161439 #wwtp next to Bern (Aare River)
	
	
	###wwtp Zürich
	d <- rivers.all %>% dplyr::select(id_all, flow_to) %>% st_drop_geometry()
	
	d$x <- 0
	d$x[WWTP_ZH] <- 1
	ft <- d$flow_to[which(d$x == 1)]
	ft_long <- WWTP_ZH
	for (i in 1:800) {
		d$x[ft] <- 1
		ft_long <- c(ft_long, ft)
		ft <- d$flow_to[ft]
	}
	sum(d$x)
	
	ft_wwtp_zh <- ft_long[1:sum(d$x)]
	
	len <- as.numeric(st_length(rivers.all[ft_wwtp_zh, ]))
	len_cum <- cumsum(len)
	ft_wwtp_zh <- ft_wwtp_zh[len_cum < 50000] #select only until km 50 from source
	len_cum <- len_cum[len_cum <- 50000]
	
	
	
	
	
	
	#load dataset
	
	lake <- s_r$isLake
	no_outflow <- s_r$outflow == 0
	rivers <- lake == F & no_outflow == T
	
	dfwater <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenarios
	names(dfwater) <- c(polymers, "all") #value per s
	
	dfsed <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenarios
	names(dfsed) <- c(polymers, "all")
	
	dfrem_acc_clean <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenarios
	names(dfrem_acc_clean) <- c(polymers, "all")

	dfinput <- as.data.frame(matrix(NA, nrow = nrow(s_r), ncol = I(length(polymers)+ 1))) #*2 for scenarios
	names(dfinput) <- c(polymers, "all")
	
	#read the different data sets for each polymer
	for (mat in polymers) {
		load(paste0(main.path, "output_files/rdata/cont_", mat,"_", rounds, "_base", ".Rdata"))
		dfwater[ , mat] <-  s_r[ , paste0("water_perM_",mat,"_MaP")]
		dfsed[ , mat] <- s_r[ , paste0("sediment_perM_", mat, "_MaP")]
		dfrem_acc_clean[ , mat] <- s_r[ , paste0("removal_perM_", mat, "_MaP")]+s_r[paste0("accumulation_perM_", mat, "_MaP")]+s_r[paste0("clean_perM_", mat, "_MaP")]
		dfinput[ , mat] <- s_r[ , paste0("inputKawecki_perS_", mat, "_MaP")]
	}
	
	dfwater$all <- rowSums(dfwater[ , polymers])
	dfsed$all <- rowSums(dfsed[ , polymers])
	dfrem_acc_clean$all <- rowSums(dfrem_acc_clean[ , polymers])
	dfinput$all <- rowSums(dfinput[ , polymers])
	
	
	
	
	
	#figure##############################
	
	n <- names(dfwater)
	n
	load("PhD/mennekes/temp_data/flow_files/0000_geo.Rdata")
	load("PhD/mennekes/data_raw/maps/lakes/lakes_poly.Rdata")
	
	#data for selection
	buffer_river <- geo
	# join the sed informatino with buffer_river
	buffer_river$sed <- (dfsed$all)*1000 #*1000 to change the concentration from per m to per km
	buffer_river$isLake <- s_r$isLake
	selection_wwtp_sed <- buffer_river[ft_wwtp_zh, ]#find selection and add it to buffer data
	selection_wwtp_sed$x <- len_cum
	#find colors for printing in 5 groups
	selection_wwtp_sed$conc_print <- "x"
	# m <- mean(d2$conc, na.rm = T)
	m_sed <- col.NR2(selection_wwtp_sed$sed) #find 5 groups of colors based on the function
	#make factors of conc. for printing
	selection_wwtp_sed$conc_print[selection_wwtp_sed$sed >= m_sed[2]] <- "a1" #biggest numbers
	selection_wwtp_sed$conc_print[selection_wwtp_sed$sed <= m_sed[2]] <- "b1"
	selection_wwtp_sed$conc_print[selection_wwtp_sed$sed <= m_sed[3]] <- "c1"
	selection_wwtp_sed$conc_print[selection_wwtp_sed$sed <= m_sed[4]] <- "d1"
	selection_wwtp_sed$conc_print[selection_wwtp_sed$sed <= m_sed[5]] <- "y" # 0 as conc
	selection_wwtp_sed$conc_print <- as.factor(selection_wwtp_sed$conc_print)
	
	
	
	#same for water conc. in selection
	conc <- dfwater[ft_wwtp_zh , "all"]
	d2 <- geo[ft_wwtp_zh, ]
	d2 <- cbind(d2, conc)
	d2$conc <- d2$conc*1000 #change from per m to per km
	d2$x <- len_cum
	d2$conc_print <- "x"
	m_w <- col.NR2(d2$conc) #find 5 groups of colors based on the function
	#make factors of conc. for printing
	d2$conc_print[d2$conc >= m_w[2]] <- "a2"
	d2$conc_print[d2$conc <= m_w[2]] <- "b2"
	d2$conc_print[d2$conc <= m_w[3]] <- "c2"
	d2$conc_print[d2$conc <= m_w[4]] <- "d2"
	d2$conc_print[d2$conc <= m_w[5]] <- "x"
	d2$conc_print <- as.factor(d2$conc_print)
	
	selection_wwtp_water <- d2 #sf with water concentration for selection only
	
	
	
	
	
		
		#get data for map of entire Switzerland
		#for MiP in river water
		conc <- dfwater[rivers, "all"]
		d2 <- geo[rivers, ]
		d2 <- cbind(d2, conc)
		d2$conc <- d2$conc*1000 #change from m to km
		d2$size <- 1
		d2$size[d2$conc >0 ] <- 2
		d2$conc[d2$conc == 0] <- NA
		d2$conc_print <- "x"
		# m <- mean(d2$conc, na.rm = T)
		m <- col.NR2(d2$conc) #find 4 groups of colors based on the function
		#make factors of conc. for printing
		d2$conc_print[d2$conc >= m[2]] <- "a"
		#extra group median of values bigger than m[2]
		mm <- round(median(d2$conc[d2$conc >=m[2]], na.rm = T))
		max(d2$conc, na.rm = T)
		d2$conc_print[d2$conc <= mm] <- "b"
		d2$conc_print[d2$conc <= m[2]] <- "c"
		d2$conc_print[d2$conc <= m[3]] <- "d"
		d2$conc_print[d2$conc <= m[4]] <- "e"
		d2$conc_print[d2$conc <= m[5]] <- "x"
		d2$conc_print <- as.factor(d2$conc_print)
		
		
		#change the theme to the selection for the rivers that are in the zoomed in area
		#	#use the same scale for all rivers in the geographical limits
		limits_data <-  st_as_sfc(st_bbox(selection_wwtp_sed))
		limits <- st_bbox(selection_wwtp_sed)
		trib <- st_intersection(geo[rivers, ], limits_data)
		trib$flow_to <- s_r$flow_to[trib$id_all_geo]
		trib$flow_to[trib$flow_to %in% s_r$id_all[s_r$outflow != 0]] <- 0 #for all the rivers that flow to unknown; change to 0
		
		####
		#find only rivers that flow into the selection
		
		#start with the last segment of the selection and add to all flow from a 1
		trib$partof <- 0 #part of the tributaries
		trib$partof[which(trib$id_all_geo %in% ft_wwtp_zh)] <- 1 #all sections of the main river are selected
		for (i in 1:500) {
			ids <- trib$id_all_geo[trib$partof == 1] #get geo IDs with one
			trib$partof[trib$flow_to %in% ids] <- 1 #write 1 when the flow to refers to a an id with 1
		}
		
		ids_trib <- trib$id_all_geo[trib$partof==1] #id_geo_all of all tributaries
		
		#bring all tributaries in the color 
		trib <- d2[d2$id_all_geo %in% ids_trib, ]
		trib$conc_print <- "x"
		max(trib$conc, na.rm = T) < m_w[1] #max value in the network should be smaller than max value in the printing ranges: TRUE should be the answer
		trib$conc_print[trib$conc >= m_w[2]] <- "a2"
		trib$conc_print[trib$conc <= m_w[2]] <- "b2"
		trib$conc_print[trib$conc <= m_w[3]] <- "c2"
		trib$conc_print[trib$conc <= m_w[4]] <- "d2"
		trib$conc_print[trib$conc <= m_w[5]] <- "x"
		trib$conc_print <- as.factor(trib$conc_print)
		
		
		
		
		
	#colors for printing
	farben <- c('#d7191c','#fdae61','#ffd700','#91bfdb','#0571b0', "grey70")
	# farben <- c('#d01c8b', '#d7191c','#fdae61','#abd9e9','#2c7bb6', "grey70")
	farben_blue<- c('#253494','#2c7fb8','#41b6c4','#a1dab4', "grey70")
	farben_red <- c('#bd0026','#f03b20','#fd8d3c','#fecc5c', "grey70") 
	
	nice <- theme_bw()+
		theme(legend.background = element_rect(fill = "white", color = NA),
					legend.margin = margin(0.1,0.1,0,0.1, unit = "cm"),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black"),
					axis.text.x = element_text(color = "black", face = "plain", size = 7),
					axis.text.y = element_text(color = "black", face = "plain", size = 7),
					legend.text = element_text(size = 7), #evtl hinzufügen: margin = margin(r = 1, unit = "cm")
					legend.title = element_text(size = 7),
					legend.spacing = unit(0.1, "cm"),
					panel.background = element_rect(fill = "transparent"),
					axis.title = element_blank(),
					plot.title = element_text(size = 7),
					legend.key = element_rect(fill = "transparent", colour = "transparent"),
					legend.key.size = unit(0.4, "cm"),
					plot.subtitle = element_text(size = 7)) #trbl
	klein = 0.02
	gross = 0.5
	
		
		
		
		
		plotlimits_big <- st_bbox(d2)
		
		
		#big plot for Switzerland
		pr <- ggplot()+
			geom_sf(data = lakes_poly[lakes_poly$area > 100000, ], color = "black", fill = "white", linewidth = klein)+
			geom_sf(data = d2[d2$size == 1, ],  aes(color = conc_print), linewidth = klein)+
			geom_sf(data = d2[d2$size == 2, ],  aes(color = conc_print), linewidth = gross)+
			scale_colour_manual(paste0("main map:\nmacroplastic mass\nin water in [g/km]"),
													values = farben,
													breaks = c("a", "b", "c", "d","e", "x"),
													labels = c(paste0(mm," - ",round(max(d2$conc, na.rm = T))),paste0(m[2]," - ",mm), paste0(m[3]," - ",m[2]),paste0(m[4]," - ",m[3]),paste0(m[5]," - ",m[4]), "0"), drop = F)+
			nice+
			scale_y_continuous(limits = c(plotlimits_big$ymin -10000, plotlimits_big$ymax+40000), breaks = c(48,47, 46))+
			scale_x_continuous(limits = c(plotlimits_big$xmin -40000, plotlimits_big$xmax+20000))+
			# labs(title = paste0(i_print, " in rivers"))+
			ggspatial::annotation_scale(location = "br", height = unit(0.2, "cm"), text_pad = unit(7, "pt"), line_width = 1)+
			ggspatial::annotation_north_arrow(width = unit(0.4, "cm"), height = unit(1, "cm"),style = north_arrow_orienteering(text_size = 7))+
			theme(legend.position = c(0.91,0.23))+
			geom_sf(data = limits_data, fill = NA, color = "black", linewidth = gross*2)
		
		# pr
		# ggsave(paste0("PhD/mennekes2.0/output_files/plots/map_river_allPolymer.png"), plot = pr, width = 17.8, height = 10, units = "cm", dpi = 1000, bg = "transparent")
		#width 17.8 for EST Water
		
	
	s1 <- 1.3 #size linewidth
	p_wwtp_ZH <- st_as_sf(st_line_sample(geo[WWTP_ZH, ], sample = 0))
	rownames(p_wwtp_ZH) <- "WWTP"
	
		
	
	#plot for selection
	p2 <- ggplot()+
		geom_sf(data = selection_wwtp_sed, aes(color = conc_print), linewidth = s1*3)+
		geom_sf(data = trib, aes(color = conc_print), linewidth = s1/2)+
		geom_sf(data = selection_wwtp_water, aes(color = conc_print), linewidth = s1)+
		geom_sf(data = p_wwtp_ZH, color = "black", size = s1*3, pch = 18)+
		# scale_color_manual(paste0("water [g/km]"),
		# 										values = farben_blue,
		# 										breaks = c("a2", "b2", "c2", "d2", "e2", "x"),
		# 										labels = c(paste0(m_w[2]," - ",m_w[1]), paste0(m_w[3]," - ",m_w[2]),paste0(m_w[4]," - ",m_w[3]),paste0(m_w[5]," - ",m_w[4]),paste0("> 0 - ",m_w[5]), "0"), drop = F)+
		scale_color_manual(paste0("inset map:\nwater [g/km]      temp. storage [g/(km * s)]"),
											values = c("a1" = farben_red[1], "b1" = farben_red[2], "c1" = farben_red[3], "d1" = farben_red[4], "x" = farben_red[5],
																 "a2" = farben_blue[1], "b2" = farben_blue[2], "c2" = farben_blue[3], "d2" = farben_blue[4], "y" = "white"),
											breaks = c("a2", "b2", "c2", "d2", "x", "a1", "b1", "c1", "d1", "y"),
											labels = c(paste0( m_w[2]," - ",m_w[1]), paste0(m_w[3]," - ",m_w[2]),paste0( m_w[4]," - ",m_w[3]), paste0( m_w[5]," - ",m_w[4]), "0", paste0( m_sed[2]," - ",m_sed[1]), paste0(m_sed[3]," - ",m_sed[2]),paste0( m_sed[4]," - ",m_sed[3]), paste0( m_sed[5]," - ",m_sed[4]), " "), drop = F)+
		nice+
		guides(color=guide_legend(ncol=2, byrow=F))+
		scale_y_continuous(limits = c(limits$ymin-800, limits$ymax+800), expand = c(0,0))+
		scale_x_continuous(limits = c(limits$xmin, limits$xmax+1300), expand = c(0,0))+
		ggspatial::annotation_scale(location = "bl", height = unit(0.1, "cm"), text_pad = unit(5, "pt"), line_width = 1)+
		geom_sf_text(data = p_wwtp_ZH, aes(label = rownames(p_wwtp_ZH)), vjust = -1.1, hjust = 0.85, size = 3)+
		theme(legend.text = element_text( margin = margin(r = 0.1, unit = "cm"), size = 6),
					legend.title = element_text(size = 6),
					panel.grid = element_blank(),
					axis.ticks = element_blank(),
					axis.text.x = element_blank(),
					axis.text.y = element_blank(),
					legend.position = c(0.9,0.8),
					panel.background = element_rect(fill = "white"),
					legend.margin = margin(1,1,1.5,1),
					legend.key.size = unit(0.22, "cm"),
					legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1),
					panel.border = element_rect(linewidth = gross*4)			)
	
	
	# p2
	
	p3 <- ggdraw(pr)+
		draw_plot(
			p2,
			x = -0.03,
			y = 0.56,
			width = 0.42,
			height = 0.42)
	Sys.sleep(1)
	ggsave(paste0("PhD/mennekes2.0/output_files/plots/map_river_all_w_insert.png"), plot = p3, width = 17.8, height = 12, units = "cm", dpi = 1000, bg = "transparent")
	ggsave(paste0("PhD/mennekes2.0/output_files/plots/map_river_all_w_insert.pdf"), plot = p3, width = 17.8, height = 12, units = "cm", dpi = 1000, bg = "transparent")
	
	
	
	
	
	
	
	

rm(list = ls())
