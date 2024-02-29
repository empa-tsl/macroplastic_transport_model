###################
# this script is used to determine sedimentation curves for the lakes. We aimed for a about 80% removal rate for the lake Geneva (500km2) in average based on existing measurements (see paper)
##sedimentation curve lakes
#######################

library(ggplot2)
library(zoo)
flache <- seq(1,800000000, by = 10000) #Area of lakes
#area in m2


lakeGeneva <- 0.95*(1- exp(1)^(-0.005*flache*0.000001)) #lake Geneva
y <- 0.95*(1- exp(1)^(-0.004*flache*0.000001))
y_schwer <- 0.95*(1- exp(1)^(-0.012*flache*0.000001)) #heavier polymers. triple the reduction
y_sehrleicht <- 0.95*(1- exp(1)^(-0.0025*flache*0.000001)) #very light polymers
x <- flache * 0.000001					
total <- (y+y+y+y*0.6+y*0.8+y_schwer+y_schwer)/7


ggplot()+
	theme_bw()+
	geom_line(data = data.frame(x = x, y = y*100), aes(x,y, color = "LDPE, HDPE, PS"))+
	geom_line(data = data.frame(x = x, y = lakeGeneva*100), aes(x,y, color = "lakeGeneva"), linetype = "dashed")+
	geom_line(data = data.frame(x = x, y = 0.5*y*100), aes(x,y, color = "EPS"))+
	geom_line(data = data.frame(x = x, y = 0.75*y*100), aes(x,y, color = "PP aktuell"))+
	geom_line(data = data.frame(x = x, y = y_schwer *100), aes(x,y, color = "heavy"))+
	geom_hline(yintercept = 95, linetype = "dashed")
	
	
#check for the integral
AUC <- function(x, y){
	sum(diff(x)*rollmean(y,2))
}

n <- AUC(flache, y) #normal
s <- AUC(flache, y_schwer)
sl <- AUC(flache, y*0.5)
l <- AUC(flache, y*0.75)
AUC(flache,y)

mean(c(n,n,n,s,s,sl,l))
