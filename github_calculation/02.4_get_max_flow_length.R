######################
# check how long the longest flow distance is to figure out how many time steps are needed.
# author: david mennekes, PhD Student at Empa St. Gallen / ETH Zürich, Switzerland, david.mennekes@empa.ch, 
# march 2021, last edit: 
######################


###############################
#packages and path
###############################

library(tidyverse)
library(sf)

#path to sub-folders
setwd("~/")
main.path <- "PhD/mennekes/"
# 


###############################
# load data
###############################

# # load river data
load(paste0(main.path, "temp_data/rivers_all5.Rdata"))

t <- st_drop_geometry(rivers.all5)
test <- data.frame(flow_to = t$flow_to,
									 len = 1,
									 len_acc = 0,
									 outflow = t$outflow,
									 outflow_name = t$name_river,
									 id_al = t$id_all)


#
ntest <- nrow(test)
test <- rbind(test, c(ntest+1, 1,0,0, NA, ntest+1))
tail(test)

#alle outflows fließen in letzte Zeile
test$flow_to[which(test$outflow==1)] <- ntest+1



#run model once

#find duplicated numbers
dup <- unique(test$flow_to[duplicated(test$flow_to)])
multi_flow_to <- test$flow_to %in% dup
dup_id <- test$id_al[multi_flow_to]
single_flow_to <- !(multi_flow_to)

df_outflow <- data.frame(name = test$outflow_name[which(test$outflow == 1)],
												 ids = test$id_al[which(test$outflow == T)],
												 len0 = 0)

rm(rivers.all5)
rm(t)

rr <- sample(1:ntest, size = 500)
N = 2500
rr_df <- as.data.frame(matrix(NA, nrow = N, ncol = length(rr)))
rr_df <- cbind(1:N, rr_df)
names(rr_df) <- c("ts", as.character(rr))
P = c(1, seq(0,N, by=100), N)
	for (i in 1:N) {
		# create empty container for data
		temp <- rep(0, nrow(test))
		
		
		
		
		
		########## for loop ################
		#if one river flows into the next one
		temp[test$flow_to[single_flow_to]] <- rowSums(test[single_flow_to, c("len", "len_acc")], na.rm = T) 
		
		# if two rivers flow into one
		# check for the longest river -> the highest number are given by the ongest river
		temp_dup <- test[dup_id, ] %>% group_by(flow_to) %>% summarise(l_len_acc = max(len_acc))
		
		
		temp[temp_dup$flow_to] <- temp_dup$l_len_acc+1 #use the longer river as input data (= the higher number)
		
		
		# write temp data to new
		test$len_acc <- temp
		
		#write data df_outflow 
		
		df_outflow <- cbind(df_outflow, temp[df_outflow$ids])
		names(df_outflow)[i+3] <- paste0("len", i)
		
		#random sections:
		rr_df[i, 2:I(length(rr)+1)] <- temp[rr]
		
		if(sum(df_outflow[ ,"len0"] == df_outflow[ , paste0("len", i)]) == nrow(df_outflow)){ #break bedingungen
			print("fertig")
			break
		}
		if(i %in% P){
			print(paste0(Sys.time(), "; round: ", i))
		}
		
	}


results <- data.frame(ts = 1:N,
											Inn = as.numeric(df_outflow[6, 4:I(N+3)]),
											Rhone = as.numeric(df_outflow[10, 4:I(N+3)]),
											"Le Doubs" = as.numeric(df_outflow[9, 4:I(N+3)]),
											Rhein = as.numeric(df_outflow[7, 4:I(N+3)]),
											unknown = as.numeric(df_outflow[11, 4:I(N+3)]))
summary(results)
library(reshape2)
library(ggplot2)
results2 <- melt(results, id.vars = "ts")
head(results2)

p1 <- ggplot(results2, aes(x = ts, y = value, color = variable))+
	geom_line()+
	theme_bw()

p1


#add random numbers
rr_df2 <- melt(rr_df, id.vars = "ts")

p1 + geom_line(data = rr_df2, aes(ts, value, color = variable), color = "grey50", alpha = 0.5)

 rm(list = ls())	

