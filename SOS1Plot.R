mdata <- read.csv("../SOSdata/Merged.csv")
mdata$Cleanup.Date <- as.Date(mdata$Cleanup.Date, format = "%m/%d/%Y")
mdata[,"Volunteer.Hours"][is.na(mdata[ ,"Volunteer.Hours"] ) ] = 0
mdata[,"X..of.Adults"][is.na(mdata[ ,"X..of.Adults"] ) ] = 0
mdata[,"X..of.Youth"][is.na(mdata[ ,"X..of.Youth"] ) ] = 0
mdata[,"X..of.Volunteers"][is.na(mdata[ ,"X..of.Volunteers"] ) ] = 0
mdata$Volunteers.Total <- mdata$X..of.Adults + mdata$X..of.Youth + mdata$X..of.Volunteers
# Adjusted volunteer hours.
# If the number of volunteers exceeds the number of hours reported
# and the number of hours reported is less than 7
# then adjusted hours is volunteers * hours
# if no entry for hours or volunteers, enter 1?
mdata$Volunteer.Hours.Adjusted = ifelse((mdata$Volunteers.Total>mdata$Volunteer.Hours & mdata$Volunteer.Hours < 7),
                                        (ifelse(mdata$Volunteer.Hours>=1,mdata$Volunteers.Total*mdata$Volunteer.Hours,mdata$Volunteers.Total)),
                                        (ifelse(mdata$Volunteer.Hours==0,1,mdata$Volunteer.Hours)))
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
plot(x=pot$year,y=pot$POT,type="o",xlab="Year",ylab="Pounds",pch="O",col="red",main="Trash & Recycling Collected")
lines(x=por$year,y=por$POR,type="o",col="green", pch="X")
legend("topleft", c("Trash","Recycling"), 
       col=c("red","green"), pch=c("O","X"));
plot(x=mdata$year,y=mdata$Pounds.of.Trash.Collected,
     xlab="Year",ylab="Pounds of Trash",
     main="Pounds of Trash vs Year")
plot(x=mdata$year,y=mdata$Pounds.of.Recycle.Collected,
     xlab="Year",ylab="Pounds of Recycle",
     main="Pounds of Recycle vs Year")
# Now per Volunteer Hour Adjusted
vol <-ddply(mdata, .(year), summarise, VOL=sum(Volunteers.Total, na.rm = TRUE))
volh <-ddply(mdata, .(year), summarise, VOLH=sum(Volunteer.Hours.Adjusted, na.rm = TRUE))
plot(x=pot$year,y=pot$POT/volh$VOLH,type="o",xlab="Year",ylab="Pounds",pch="O",col="red",main="Trash & Recycling Collected / Volunteer Hour Adjusted")
lines(x=por$year,y=por$POR/volh$VOLH,type="o",col="green", pch="X")
legend("topleft", c("Trash","Recycling"), 
       col=c("red","green"), pch=c("O","X"));
plot(x=mdata$year,y=mdata$Pounds.of.Trash.Collected/mdata$Volunteer.Hours.Adjusted,
     xlab="Year",ylab="Pounds of Trash",
     main="Pounds of Trash per Volunteer Hour Adjusted vs Year")
plot(x=mdata$year,y=mdata$Pounds.of.Recycle.Collected/mdata$Volunteer.Hours.Adjusted,
     xlab="Year",ylab="Pounds of Recycle",
     main="Pounds of Recycle per Volunteer Hour Adjusted vs Year")
#
barplot(cbs$CBS, names.arg=cbs$year, main="Cigarette Butts")
barplot(cbs$CBS[-1]/pot$POT[-1],names.arg=pot$year[-1],main="Cigarette Butts / Pounds of Trash")
#
barplot(volh$VOLH, names.arg = volh$year, main="Volunteer Hours Adjusted")
barplot(vol$VOL, names.arg = vol$year, main="Volunteers")
barplot(volh$VOLH[-1]/vol$VOL[-1],names.arg=vol$year[-1],main="Volunteer Hours Adjusted / Volunteers")
barplot(pot$POT/volh$VOLH, names.arg = pot$year, main="Pounds of Trash Per Volunteer Hour Adjusted")
barplot(por$POR/volh$VOLH, names.arg = por$year, main="Pounds of Recycle Per Volunteer Hour Adjusted")

barplot(pot$POT/vol$VOL, names.arg = pot$year, main="Pounds of Trash Per Volunteer")
barplot(por$POR/vol$VOL, names.arg = por$year, main="Pounds of Recycle Per Volunteer")
barplot(cbs$CBS[-1]/vol$VOL[-1], names.arg=cbs$year[-1], main="Cigarette Butts per Volunteer")
barplot(cbs$CBS[-1]/volh$VOLH[-1], names.arg=cbs$year[-1], main="Cigarette Butts per Volunteer Hour Adjusted")

barplot(pgb$PGB, names.arg=pgb$year, main="Plastic Grocery Bags")
barplot(pgb$PGB[-1]/volh$VOLH[-1], names.arg=pgb$year[-1], main="Plastic Grocery Bags Per Volunteer Hour Adjusted")
barplot(pgb$PGB[-1]/vol$VOL[-1], names.arg=pgb$year[-1], main="Plastic Grocery Bags Per Volunteer")

barplot(pst$PST[-1]/volh$VOLH[-1], names.arg=pst$year[-1], main="Plastic Straws & Stirrers per Volunteer Hour Adjusted")
barplot(mdc$MDC[-1]/volh$VOLH[-1], names.arg=mdc$year[-1], main="Metal Drink Cans per Volunteer Hour Adjusted")
barplot(mcp$MCP[-1]/volh$VOLH[-1], names.arg=mcp$year[-1], main="Metal Cans Tops per Volunteer Hour Adjusted")
barplot(pcr$PCR[-1]/volh$VOLH[-1], names.arg=pcr$year[-1], main="Plastic Tops and Rings per Volunteer Hour Adjusted")
barplot(pbt$PBT[-1]/volh$VOLH[-1], names.arg=pbt$year[-1], main="Plastic Bottles per Volunteer Hour Adjusted")
barplot(gbt$GBT[-1]/volh$VOLH[-1], names.arg=gbt$year[-1], main="Glass Bottles per Volunteer Hour Adjusted")
# line plots of above items
#plot(x=cbs$year,y=cbs$CBS,type="o",xlab="Year",ylab="Total",pch="O",col="red",main="Items Per Year")
#lines(x=pgb$year,y=pgb$PGB,type="o",col="green", pch="X")
g_range <- range(0, pgb$PGB[-1]/volh$VOLH[-1], pst$PST[-1]/volh$VOLH[-1],mdc$MDC[-1]/volh$VOLH[-1],mcp$MCP[-1]/volh$VOLH[-1],pcr$PCR[-1]/volh$VOLH[-1],pbt$PBT[-1]/volh$VOLH[-1],gbt$GBT[-1]/volh$VOLH[-1])
plot(x=pgb$year[-1],y=pgb$PGB[-1]/volh$VOLH[-1],type="o",ylim=g_range,xlab="Year",ylab="Total",pch="X",col="red",main="Items Per Year Per Volunteer Hour Adjusted")
lines(x=pst$year[-1],y=pst$PST[-1]/volh$VOLH[-1],type="o",col="blue", pch="P")
lines(x=mdc$year[-1],y=mdc$MDC[-1]/volh$VOLH[-1],type="o",col="black", pch="M")
lines(x=mcp$year[-1],y=mcp$MCP[-1]/volh$VOLH[-1],type="o",col="cyan", pch="T")
lines(x=pcr$year[-1],y=pcr$PCR[-1]/volh$VOLH[-1],type="o",col="magenta", pch="R")
lines(x=pbt$year[-1],y=pbt$PBT[-1]/volh$VOLH[-1],type="o",col="yellow", pch="B")
lines(x=gbt$year[-1],y=gbt$GBT[-1]/volh$VOLH[-1],type="o",col="green", pch="G")
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
vhr <- ddply(mdata, .(Cleanup.Date), summarise, VHR=sum(Volunteer.Hours.Adjusted, na.rm = TRUE))
vhr <- vhr[vhr$Cleanup.Date<enddate,]
vhr <- vhr[vhr$Cleanup.Date>=startdate,]
#  Volunteer Hours plot for 2017
barplot(vhr$VHR, names.arg = vhr$Cleanup.Date, cex.names = .5,las=2, main="Volunteer Hours Adjusted")
# Looking at difference between 2009 and 2010
tpot10 <- mdata[mdata$year=="2010",]
tpot09 <- mdata[mdata$year=="2009",]
plot(x=tpot10$Volunteer.Hours.Adjusted,y=tpot10$Pounds.of.Trash.Collected,type="o",xlab="Hours Adj.",ylab="Pounds",pch="O",col="red",main="Trash vs. Vol Hours")
lines(x=tpot09$Volunteer.Hours.Adjusted,y=tpot09$Pounds.of.Trash.Collected,col="green",type="o",pch="X")
legend("topright",c("2010","2009"),col=c("red","green"),pch=c("O","X"))
summary(tpot09$Volunteers.Total)
summary(tpot10$Volunteers.Total)
plot(x=mdata$year,y=mdata$Volunteers.Total,
     xlab="Year",ylab="Volunteers",
     main="Volunteers vs Year")
# Let's look at 2009
startdate <- as.Date(c("01/01/2009"), format = "%m/%d/%Y")
enddate   <- as.Date(c("01/01/2010"), format = "%m/%d/%Y")
sdtt <- ddply(mdata, .(Cleanup.Date), summarise, SDTT=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
sdtt <- sdtt[sdtt$Cleanup.Date<enddate,]
sdtt <- sdtt[sdtt$Cleanup.Date>=startdate,]
#  trash plot for 2017
barplot(sdtt$SDTT, names.arg = sdtt$Cleanup.Date, cex.names = .5,las=2, main="Pounds of Trash")
# Let's look at 2010
startdate <- as.Date(c("01/01/2010"), format = "%m/%d/%Y")
enddate   <- as.Date(c("01/01/2011"), format = "%m/%d/%Y")
sdtt <- ddply(mdata, .(Cleanup.Date), summarise, SDTT=sum(Pounds.of.Trash.Collected, na.rm = TRUE))
sdtt <- sdtt[sdtt$Cleanup.Date<enddate,]
sdtt <- sdtt[sdtt$Cleanup.Date>=startdate,]
#  trash plot for 2017
barplot(sdtt$SDTT, names.arg = sdtt$Cleanup.Date, cex.names = .5,las=2, main="Pounds of Trash")
