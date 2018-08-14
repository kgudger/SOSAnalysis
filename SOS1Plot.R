mdata <- read.csv("../SOSdata/Merged.csv")
mdata$Cleanup.Date <- as.Date(mdata$Cleanup.Date, format = "%m/%d/%Y")
# plots below
library(plyr)
pot <-ddply(mdata, .(year), summarise, POT=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
por <-ddply(mdata, .(year), summarise, POR=sum(Pounds.of.Recycle.Collected, na.rm = TRUE))
cbs <- ddply(mdata, .(year), summarise, CBS=sum(Cigarette.Butts, na.rm = TRUE))
# add in previous years' plastic bags?
pgb <- ddply(mdata, .(year), summarise, PGB=sum(Plastic.Bags.Grocery,Plastic.Bags, na.rm = TRUE))
pst <- ddply(mdata, .(year), summarise, PST=sum(Plastic.straws.stirrers, na.rm = TRUE))
# for plot add in Metal Cans
mdc <- ddply(mdata, .(year), summarise, MDC=sum(Metal.Cans, na.rm = TRUE))
mcp <- ddply(mdata, .(year), summarise, MCP=sum(Metal.Bottle.Caps.Pulls, na.rm = TRUE))
# for plot add in Plastic Lids or Caps
pcr <- ddply(mdata, .(year), summarise, PCR=sum(Plastic.Bottle.Caps,Plastic.Lids.or.Caps, na.rm = TRUE))
# for plot add in Beverage.Bottles..Plastic.
pbt <- ddply(mdata, .(year), summarise, PBT=sum(Plastic.Bottles,Beverage.Bottles..Plastic., na.rm = TRUE))
# for plot add in Beverage.Bottles..Glass.
gbt <- ddply(mdata, .(year), summarise, GBT=sum(Glass.Bottles,Beverage.Bottles..Glass., na.rm = TRUE))
barplot(pot$POT, names.arg = pot$year, main="Pounds of Trash")
barplot(por$POR, names.arg = por$year, main="Pounds of Recycle")
# line plots of Trash, Recycling
plot(x=pot$year,y=pot$POT,type="o",xlab="Year",ylab="Pounds",pch="O",col="red",main="Trash / Recycling Collected")
lines(x=por$year,y=por$POR,type="o",col="green", pch="X")
legend("topleft", c("Trash","Recycling"), 
       col=c("red","green"), pch=c("O","X"));
#
barplot(cbs$CBS, names.arg=cbs$year, main="Cigarette Butts")
barplot(pgb$PGB, names.arg=pgb$year, main="Plastic Grocery Bags")
barplot(pst$PST, names.arg=pst$year, main="Plastic Straws / Stirrers")
barplot(mdc$MDC, names.arg=mdc$year, main="Metal Drink Cans")
barplot(mcp$MCP, names.arg=mcp$year, main="Metal Cans Tops")
barplot(pcr$PCR, names.arg=pcr$year, main="Plastic Tops and Rings")
barplot(pbt$PBT, names.arg=pbt$year, main="Plastic Bottles")
barplot(gbt$GBT, names.arg=gbt$year, main="Glass Bottles")
# line plots of above items
#plot(x=cbs$year,y=cbs$CBS,type="o",xlab="Year",ylab="Total",pch="O",col="red",main="Items Per Year")
#lines(x=pgb$year,y=pgb$PGB,type="o",col="green", pch="X")
g_range <- range(0, pgb$PGB, pst$PST,mdc$MDC,mcp$MCP,pcr$PCR,pbt$PBT,gbt$GBT)
plot(x=pgb$year,y=pgb$PGB,type="o",ylim=g_range,xlab="Year",ylab="Total",pch="O",col="red",main="Items Per Year")
lines(x=pst$year,y=pst$PST,type="o",col="blue", pch="P")
lines(x=mdc$year,y=mdc$MDC,type="o",col="black", pch="M")
lines(x=mcp$year,y=mcp$MCP,type="o",col="cyan", pch="T")
lines(x=pcr$year,y=pcr$PCR,type="o",col="magenta", pch="R")
lines(x=pbt$year,y=pbt$PBT,type="o",col="yellow", pch="B")
lines(x=gbt$year,y=gbt$GBT,type="o",col="green", pch="G")
legend("topleft", c("Plastic Bags","Straws","Metal Cans","Metal Tabs","Plastic Tops","Plastic Bottles","Glass Bottles"), 
       col=c("red","blue","black","cyan","magenta","yellow","green"), pch=c("X","P","M","T","R","B","G"));
#
ind <- which(sapply(mdata, is.numeric))
library(ggplot2)
library(GGally)
ggcorr(mdata[,agrep("Plastic",colnames(mdata))],method="pairwise",label=TRUE,alpha=TRUE, label_size = 3, hjust = 0.75, size = 2, layout.exp = 2)
ggcorr(mdata[,agrep("Bag",colnames(mdata))], method="pairwise",label=TRUE,alpha=TRUE, label_size = 3, hjust = 0.75, size = 2, layout.exp = 2)
qplot(mdata$Plastic.straws.stirrers,mdata$Plastic.Bottle.Caps,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()
MCOR <- cor(mdata[sapply(mdata, is.numeric)],use="p") # correlation matrix
MCOR[lower.tri(MCOR, diag = TRUE)] <- NA          # lower tri and diag set to NA
sSet <- subset(na.omit(data.frame(expand.grid(dimnames(MCOR)), value = c(MCOR))), value > .6999)
sSet <- sSet[order(sSet$value), ]
write.csv(sSet, file="../SOSdata/Corr.csv",row.names=FALSE, na="")
qplot(mdata$Beverage.Cans,mdata$Beverage.Bottles..Glass.,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()

qplot(mdata$Plastic.Bottles,mdata$Glass.Bottles,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()

qplot(mdata$Condoms,mdata$X6.Pack.Holders,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()

qplot(mdata$Lids..Plastic.,mdata$Straws.Stirrers,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()

qplot(mdata$Cigarette.Lighters,mdata$Cigarette.box.or.wrappers,data=mdata,geom = c("point", "smooth"),method = "lm", alpha = I(1 / 5), se = FALSE) +
  scale_x_log10() + scale_y_log10()

# by beach name
potb <-ddply(mdata, .(Cleanup.Site), summarise, POTB=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
potb <- potb[potb$POTB>800,]
barplot(potb$POTB, names.arg = potb$Cleanup.Site, cex.names = .5,las=2, main="Pounds of Trash")
# by City / County
ccty <-ddply(mdata, .(City.County), summarise, CCTY=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
barplot(ccty$CCTY, names.arg = ccty$City.County, cex.names = .5,las=2, main="Pounds of Trash")
sdtt <- ddply(mdata, .(Cleanup.Date), summarise, SDTT=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
startdate <- as.Date(c("01/01/2017"), format = "%m/%d/%Y")
enddate   <- as.Date(c("01/01/2018"), format = "%m/%d/%Y")
sdtt <- sdtt[sdtt$Cleanup.Date<enddate,]
sdtt <- sdtt[sdtt$Cleanup.Date>=startdate,]
#  trash plot for 2017
barplot(sdtt$SDTT, names.arg = sdtt$Cleanup.Date, cex.names = .5,las=2, main="Pounds of Trash")
# setup for Fireworks
frw <- ddply(mdata, .(Cleanup.Date), summarise, FRW=sum(Fireworks, na.rm = TRUE))
frw <- frw[frw$Cleanup.Date<enddate,]
frw <- frw[frw$Cleanup.Date>=startdate,]
#  Firworks plot for 2017
barplot(frw$FRW, names.arg = frw$Cleanup.Date, cex.names = .5,las=2, main="Fireworks")
# setup for Volunteer Hours
vhr <- ddply(mdata, .(Cleanup.Date), summarise, VHR=sum(Volunteer.Hours, na.rm = TRUE))
vhr <- vhr[vhr$Cleanup.Date<enddate,]
vhr <- vhr[vhr$Cleanup.Date>=startdate,]
#  Volunteer Hours plot for 2017
barplot(vhr$VHR, names.arg = vhr$Cleanup.Date, cex.names = .5,las=2, main="Volunteer Hours")

