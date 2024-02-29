####
# plotting figures of the model
# author: david mennekes, david.mennekes@empa.ch,
# march 2023
##################

setwd("~/")
# library packages

	library(ggplot2)
	library(dplyr)
	library(tidyverse)
	library(ggrepel)
	library(patchwork) #for making all images the same dimensions.
	library(cowplot)
	library(ggpattern)
	library(reshape2)
	library(extrafont)

	
	
	#load data
	
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
	
	### rhine ###
	
	df04 <- df03 %>% filter(river == "rhine") %>% group_by(plot.cat) %>% summarise(mi = min(value), ma = max(value))
	df04$plot.cat <- factor(df04$plot.cat, levels = x.cat)
	df03_rhine <- df03 %>% filter(river == "rhine")
	#label data
	
	df03_median <- df03_rhine %>% filter(!(plot.cat %in% c("null", "base"))) %>% group_by(plot.cat) %>% summarise(med = median(value))
	
	#plot theme
	
	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 10),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size = 10),
					legend.text = element_text(color = "black", face = "plain", size = 8),
					panel.background = element_rect(fill = "transparent"),
					panel.grid = element_blank(),
					plot.subtitle = element_text(size = 10)) #trbl
	
	

	pm = 0.01
	
	p01 <- ggplot(data = df04 %>% filter(plot.cat %in% c("lake", "river", "sedimentation", "weir", "combined")))+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_base"], color = "black", lty = "dashed")+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_null"])+
		geom_segment(aes(x = plot.cat, xend = plot.cat,  y = mi, yend = ma), size = 5, colour = "grey80", alpha = 0.6)+
		# geom_segmen<t(data = df03_median, aes(x = plot.cat, xend = plot.cat, y = med-pm, yend = med+pm), size = 5, colour = "black") +
		geom_point(data = df03_rhine %>% filter(plot.cat %in% c("lake", "river", "sedimentation", "weir", "combined")), aes(x = plot.cat, y = value))+
		scale_x_discrete(breaks = c("weir", "river", "sedimentation", "lake", "combined"), labels = c("weir", "river", "LU, Q, S", "lake", "combined"))+
		scale_y_continuous(n.breaks = 3)+
		labs(x = "scenarios", y = "macroplastic transport in water\nRiver Rhine in Basel [g/s]")+
		annotate("text",x = as.factor("weir"), y = I(df03_rhine$value[df03_rhine$variable == "_null"] +2*pm), label = "no retention", hjust = "left", vjust = "right",  size = 2.5, color = "black")+
		annotate("text",x = as.factor("weir"), y = I(df03_rhine$value[df03_rhine$variable == "_base"] -2*pm), label = "base scenario", hjust = "left", vjust = "left",  size = 2.5, color = "black")+
		nice
	
	p01		
		
	# ggsave(plot = p01, paste0(main.path, "output_files/plots/plot01_rhine.png"), width = 8.5, height = 7, units = "cm", dpi = 500, bg = "transparent")
	
	
	
	### rhone ###
	
	df04 <- df03 %>% filter(river == "rhone") %>% group_by(plot.cat) %>% summarise(mi = min(value), ma = max(value))
	df04$plot.cat <- factor(df04$plot.cat, levels = x.cat)
	df03_rhine <- df03 %>% filter(river == "rhone")
	#label data
		
		df03_median <- df03_rhine %>% filter(!(plot.cat %in% c("null", "base"))) %>% group_by(plot.cat) %>% summarise(med = median(value))
	
	#plot theme
	
	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 10),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size = 10),
					legend.text = element_text(color = "black", face = "plain", size = 8),
					panel.background = element_rect(fill = "transparent"),
					legend.key = element_blank(),
					panel.grid = element_blank(),
					plot.subtitle = element_text(size = 10)) #trbl
	
	
	
	pm = 0.005
	
	p02 <- ggplot(data = df04 %>% filter(plot.cat %in% c("lake", "river", "sedimentation", "weir", "combined")))+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_base"], color = "black", lty = "dashed")+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_null"])+
		geom_segment(aes(x = plot.cat, xend = plot.cat,  y = mi, yend = ma), size = 5, colour = "grey80", alpha = 0.6)+
		# geom_segmen<t(data = df03_median, aes(x = plot.cat, xend = plot.cat, y = med-pm, yend = med+pm), size = 5, colour = "black") +
		geom_point(data = df03_rhine %>% filter(plot.cat %in% c("lake", "river", "sedimentation", "weir", "combined")), aes(x = plot.cat, y = value))+
		scale_x_discrete(breaks = c("weir", "river", "sedimentation", "lake", "combined"), labels = c("weir", "resuspension\nyes / no", "LU, Q, S", "lake", "combined"))+
		scale_y_continuous(n.breaks = 3)+
		labs(x = "\nscenarios", y = "macroplastic transport in water\nRiver Rhine in Basel [g/s]")+
		annotate("text",x = as.factor("weir"), y = I(df03_rhine$value[df03_rhine$variable == "_null"] +2*pm), label = "no retention", hjust = "left", vjust = "right",  size = 2.5, color = "black")+
		annotate("text",x = as.factor("weir"), y = I(df03_rhine$value[df03_rhine$variable == "_base"] -2*pm), label = "base scenario", hjust = "left", vjust = "left",  size = 2.5, color = "black")+
		nice
	
	p02		
	
	# ggsave(plot = p02, paste0(main.path, "output_files/plots/plot01_rhone.png"), width = 8.5, height = 7, units = "cm", dpi = 500, bg = "transparent")
	
	
	
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
	
	#plot theme
	
	
	nice <- theme_bw()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "black", size = 10),
					axis.text.x = element_text(color = "black", face = "plain", size = 10),
					axis.text.y = element_text(color = "black", face = "plain", size = 10),
					axis.title = element_text(color = "black", face = "plain", size = 10),
					legend.text = element_text(color = "black", face = "plain", size = 8),
					panel.background = element_rect(fill = "transparent"),
					panel.grid = element_blank(),
					plot.subtitle = element_text(size = 10)) #trbl
	
	
	
	pm = 0.005
	#add shape
	#low
	low <- c("_weir05", "_baseLakes05", "_LUlow", "_Qlow", "_Slow", "_low_all")
	high <- c("_weir95", "_baseLakes95", "_LUhigh", "_Qhigh", "_Shigh", "_high_all")
	
	df03_sel$shape_cat <- "mid"
	df03_sel$shape_cat[df03_sel$variable %in% low] <- "low"
	df03_sel$shape_cat[df03_sel$variable %in% high] <- "high"
	df03_sel$shape_cat <- factor(df03_sel$shape_cat, levels = c("high", "mid", "low"))
	df03_mima<- df03_sel %>% group_by(plot.cat, river) %>% summarise(mi = min(value_percent), ma = max(value_percent))
	
	p03 <- ggplot()+
		geom_segment(data = df03_mima, aes(x = river , xend = river,  y = mi, yend = ma),size = 6.5, colour = "grey80")+
		geom_point(data = df03_sel, aes(x = river, y = value_percent, fill = river, shape = shape_cat), size = 2.3)+
		facet_wrap(~ plot.cat, strip.position = "bottom", scales = "free_x", nrow = 1, labeller = labeller(lbs))+
		nice+
		theme(panel.spacing = unit(0.3, "line"),
					strip.background = element_blank(),
					strip.placement = "outside",
					legend.position = c(0.9,0.3),
					axis.text.x = element_blank(),
					legend.key.height = unit(0.35, "cm"),
					legend.title = element_blank(),
					legend.margin = margin(0,0,0,0),
					axis.ticks.x = element_blank())+
		scale_y_continuous(n.breaks = 3)+
		labs(x = "\nanalyzed parameter scenarios", y = "macroplastic retention\n in catchment in [%]")+
		scale_fill_manual(element_blank(), values = c("dodgerblue", "red3"), labels = c("Rhine", "Rhône"))+
		scale_shape_manual(element_blank(), values = c(24,21,25), labels = c("highest", "other", "lowest"))+
		guides(fill = guide_legend("Legend fill", override.aes = list(shape = 21)))
	
	p03
	
	ggsave(plot = p03, paste0(main.path, "output_files/plots/plot01_rhonerhine.png"), width = 17.8, height = 7, units = "cm", dpi = 500, bg = "transparent")
	
	
	#for SI
	df04 <- df03_sel
	df04$variable
	df04$x <- rep(c(5,25,50,75,95,NA,NA, 5,50,95,NA, NA, 14,50,86,14,50,86,14,50,86,11,89),2)
	
	df04$plot.cat2 <- NA
	df04$plot.cat2[df04$variable %in% c("_weir05", "_weir25", "_weir50", "_weir75", "_weir95")] <- x.cat[1]
	df04$plot.cat2[df04$variable %in% c("_baseLakesLinear", "_baseLakes05", "_baseLakes50", "_baseLakes95")] <- x.cat[2]
	df04$plot.cat2[df04$variable %in% c("_baseRivers", "_baseRiversnoResus")] <- x.cat[3]
	df04$plot.cat2[df04$variable %in% c("_LUlow", "_LUmid", "_LUhigh", "_Qlow", "_Qmid", "_Qhigh","_Slow" ,"_Smid", "_Shigh")] <- x.cat[4]
	df04$plot.cat2[df04$variable %in% c("_base")] <- "base"
	df04$plot.cat2[df04$variable %in% c("_null")] <- "null"
	df04$plot.cat2[df04$variable %in% c("_high_all", "_low_all")] <- "combined"
	
	
	
	p04 <- ggplot(data = df04 %>% filter(!(is.na(x))), aes(x = x, y = value_percent, fill = plot.cat2, shape = river))+
		geom_point( color = "black")+
		scale_fill_manual("", values = c("combined" = '#e41a1c',"lake" = '#377eb8',"sedimentation" = '#4daf4a',"weir" = '#984ea3'),
											labels = c("comdined *", "lake", "LU / Q / S *", "weirs"))+
		scale_shape_manual("", values = c("rhine" = 21, "rhone"= 23), labels = c("Rhine", "Rhône"))+
		guides(fill = guide_legend(override.aes = list(shape = 22, size = 3, color = NA)))+
		scale_x_continuous(breaks = c(5,12.5, 25, 50, 75, 87.5, 95), labels = c("5", "low", "25", "50\nmid\n ", "75", "high", "95"))+
		nice+
		theme(legend.position = "right")+
		labs(x = "retention per parameter [%]",
				 y = "retention in catchment [%]")
	
	
	
	p04
	ggsave(plot = p04, paste0(main.path, "output_files/plots/SI_plot01.png"), width = 13, height = 8, units = "cm", dpi = 500, bg = "transparent")
	
	
	
	
	#plot for EGU
	
	nice2 <- theme_classic()+
		theme(legend.position = "none",
					legend.background = element_rect(fill = "transparent", color = NA),
					plot.background = element_rect(fill = "transparent",colour = NA),
					text = element_text(color = "white"),
					axis.line = element_line(color = "white"),
					axis.ticks = element_line(color = "white"),
					axis.text.x = element_text(color = "white", face = "plain", size = 18),
					axis.text.y = element_text(color = "white", face = "plain", size = 18),
					axis.title = element_text(size = 15),
					panel.background = element_rect(fill = "transparent")) #trbl
	
	
	
	p01_egu <- ggplot(data = df04 %>% filter(plot.cat %in% c("lake",  "sedimentation", "weir")))+
		geom_segment(aes(x = plot.cat, xend = plot.cat,  y = mi, yend = ma), size = 20, colour = "black", alpha = 0)+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_base"], color = "white", lty = "dashed")+
		geom_hline(yintercept = df03_rhine$value[df03_rhine$variable == "_null"], color = "white")+
		geom_segment(data = df04 %>% filter(plot.cat %in% c("lake", "weir", "sedimentation")), aes(x = plot.cat , xend = plot.cat,  y = mi, yend = ma), size = 20, colour = "grey90", alpha = 0.6)+
		# geom_segment(data = df03_median, aes(x = plot.cat, xend = plot.cat, y = med-pm, yend = med+pm), size = 5, colour = "black") +
		geom_point(data = df03_rhine %>% filter(variable != "_baseLakesLinear") %>% filter(plot.cat %in% c("sedimentation")), aes(x = plot.cat, y = value), fill = "#ff9900", color = "black",pch = 21, size = 8)+
		geom_point(data = df03_rhine %>% filter(variable != "_baseLakesLinear") %>% filter(plot.cat %in% c("lake", "weir")), aes(x = plot.cat, y = value), fill = "white", color = "black",pch = 21, size = 8)+
		scale_x_discrete(breaks = c("weir",  "sedimentation", "lake"), labels = c("weir",  "LU, Q, S", "lake"))+
		labs(x = "scenarios", y = "macroplastic flow in [g/s]")+
		# annotate("text",x = as.factor("weir"), y = I(df03_rhine$value[df03_rhine$variable == "_null"] -2*pm), label = "no retention", hjust = "center", vjust = "right",  size = 2, color = "black")+
		# annotate("text",x = as.factor("lake"), y = I(df03_rhine$value[df03_rhine$variable == "_base"] +2*pm), label = "base scenario", hjust = "center", vjust = "left",  size = 2, color = "black")+
		nice2
	
	p01_egu		
	
	# ggsave(plot = p01_egu, "PhD/präsi/für Konferenzen/2023 EGU/plot1e.png", width = 25, height = 16, units = "cm", dpi = 900, bg = "transparent")
	
	rm(list = ls())
	
	

	
	