b2017data <- read.csv("../SOSdata/2017-working-transpose.csv")
b2017adata <- read.csv("../SOSdata/2017acc-working-transpose.csv")
b2016data <- read.csv("../SOSdata/2016-working-transpose.csv")
b2016adata <- read.csv("../SOSdata/2016acc-working-transpose.csv")
b2015data <- read.csv("../SOSdata/2015-working-transpose.csv")
b2014data <- read.csv("../SOSdata/2014-working-transpose.csv")
b2013data <- read.csv("../SOSdata/2013-working-transpose.csv")
b2012data <- read.csv("../SOSdata/2012-working-transpose.csv")
b2011data <- read.csv("../SOSdata/2011-working-transpose.csv")
b2010data <- read.csv("../SOSdata/2010-working-transpose.csv")
b2009data <- read.csv("../SOSdata/2009-working-transpose.csv")
b2008data <- read.csv("../SOSdata/2008-working-transpose.csv")
list.of.data.frames = list(b2017data, b2017adata, b2016data, b2016adata, b2015data, b2014data, b2013data, b2012data, b2011data, b2010data, b2009data, b2008data)
merged.data.frame = Reduce(function(...) merge(..., all=T), list.of.data.frames)
#tail(merged.data.frame)
library(plyr)
# clean up data, remove NAs
merged.data.frame$Pounds.of.Trash.Collected <- as.numeric(as.character(merged.data.frame$Pounds.of.Trash.Collected))
ind <- which(sapply(merged.data.frame, is.numeric))
for(j in ind){
  merged.data.frame[[j]] <- as.numeric(as.character(merged.data.frame[[j]]))
#  merged.data.frame[[j]] <- replace(merged.data.frame[[j]],is.na(merged.data.frame[[j]]),0)
}
# combine all cigarette butts into one column
merged.data.frame$Cigarette.Butts.Total = merged.data.frame$Cigarette.Butts + merged.data.frame$Cigarette.butts
# delete old columns
merged.data.frame <- subset(merged.data.frame, select= -c(Cigarette.Butts,Cigarette.butts))
# combine all definite plastic grocery bags into one column
merged.data.frame$Plastic.Grocery.Total = merged.data.frame$Plastic.Bags..grocery..shopping. + merged.data.frame$Grocery.Bags..Plastic.
# delete old plastic grocery bags columns
merged.data.frame <- subset(merged.data.frame, select= -c(Plastic.Bags..grocery..shopping.,Grocery.Bags..Plastic.))
# combine all plastic straws into one column
merged.data.frame$Plastic.Straws.Total = merged.data.frame$Plastic.straws.or.stirrers + merged.data.frame$Straws.Stirrers + merged.data.frame$Plastic.straws.stirrers
# delete old plastic straws columns
merged.data.frame <- subset(merged.data.frame, select= -c(Plastic.straws.or.stirrers,Straws.Stirrers,Plastic.straws.stirrers))
# combine all syringes into one column
merged.data.frame$Syringes.Total = merged.data.frame$Syringes + merged.data.frame$Syringes.or.needles + merged.data.frame$Hazardous.Syringes.Needles
# delete old syringes columns
merged.data.frame <- subset(merged.data.frame, select= -c(Syringes,Syringes.or.needles,Hazardous.Syringes.Needles))
# combine all Cardboard into one column
merged.data.frame$Cardboard.Total = merged.data.frame$Cardboard + merged.data.frame$Paper.Cardboard
# delete old Carboard columns
merged.data.frame <- subset(merged.data.frame, select= -c(Cardboard,Paper.Cardboard))
# combine all Tires into one column
merged.data.frame$Tires.Total = merged.data.frame$Tires + merged.data.frame$Tires.1
# delete old Tires columns
merged.data.frame <- subset(merged.data.frame, select= -c(Tires,Tires.1))
# combine all id columns
merged.data.frame$id = merged.data.frame$id2008 + merged.data.frame$id2009 +
  merged.data.frame$id2010 + merged.data.frame$id2011 + merged.data.frame$id2012 + 
  merged.data.frame$id2013 + merged.data.frame$id2014 + merged.data.frame$id2015 + 
  merged.data.frame$id2016 + merged.data.frame$id2016b + merged.data.frame$id2017
# delete extra id columns
merged.data.frame <- subset(merged.data.frame, select= -c(id2008,id2009,id2010,id2011,id2012,id2013,id2014,id2015,id2016,id2017,id2016b))
# combine all metal beverage cans into one column
merged.data.frame$Metal.Drink.Cans.Total = merged.data.frame$Metal.beverage.cans + merged.data.frame$Metal.Beer.Cans + merged.data.frame$Metal.Soda.cans
# delete old Metal Drink Cans columns
merged.data.frame <- subset(merged.data.frame, select= -c(Metal.beverage.cans,Metal.Beer.Cans,Metal.Soda.cans))
# combine all metal can tops and tabs into one column
merged.data.frame$Metal.Caps.Tabs.Total = merged.data.frame$Metal.bottle.caps.or.can.pulls + merged.data.frame$Bottle.Caps..Metal. + merged.data.frame$Metal.Bottle.caps + merged.data.frame$Metal.Bottle.Caps.or.Pull.Tabs
# delete old Metal Cans Tops and Tabs columns
merged.data.frame <- subset(merged.data.frame, select= -c(Metal.bottle.caps.or.can.pulls,Bottle.Caps..Metal.,Metal.Bottle.caps,Metal.Bottle.Caps.or.Pull.Tabs))
# combine all plastic tops and rings into one column
merged.data.frame$Plastic.Caps.Rings.Total = merged.data.frame$Plastic.bottle.caps.or.rings + merged.data.frame$Bottle.Caps..Plastic. + merged.data.frame$Plastic.Bottle.Caps.Rings
# delete old Plastic Tops and rings columns
merged.data.frame <- subset(merged.data.frame, select= -c(Plastic.bottle.caps.or.rings,Bottle.Caps..Plastic.,Plastic.Bottle.Caps.Rings))
# combine all plastic bottles into one column
merged.data.frame$Plastic.Bottles.Total = merged.data.frame$Plastic.bottles + merged.data.frame$Plastic.Bottles
# delete old Plastic bottles columns
merged.data.frame <- subset(merged.data.frame, select= -c(Plastic.bottles,Plastic.Bottles))
# combine all glass bottles into one column
merged.data.frame$Glass.Bottles.Total = merged.data.frame$Glass.bottles + merged.data.frame$Glass.Bottles
# delete old Glass bottles columns
merged.data.frame <- subset(merged.data.frame, select= -c(Glass.bottles,Glass.Bottles))
# combine all Fireworks into one column
merged.data.frame$Fireworks = merged.data.frame$Fireworks + merged.data.frame$Fireworkds
# delete Fireworkds columns
merged.data.frame <- subset(merged.data.frame, select= -c(Fireworkds))
# cleanup Cleanup.Site de-dup
merged.data.frame$Cleanup.Site[agrep("Davenport",merged.data.frame$Cleanup.Site)]<-"Davenport Main Beach"
merged.data.frame$Cleanup.Site[agrep("Seacliff",merged.data.frame$Cleanup.Site)]<-"Seacliff State Beach"
merged.data.frame$Cleanup.Site[agrep("Twin",merged.data.frame$Cleanup.Site)]<-"Twin Lakes State Beach"
merged.data.frame$Cleanup.Site[agrep("Seabright",merged.data.frame$Cleanup.Site)]<-"Seabright State Beach"
merged.data.frame$Cleanup.Site[agrep("Natural",merged.data.frame$Cleanup.Site)]<-"Natural Bridges State Beach"
merged.data.frame$Cleanup.Site[agrep("Capitola",merged.data.frame$Cleanup.Site)]<-"Capitola City Beach"
merged.data.frame$Cleanup.Site[agrep("Cowell",merged.data.frame$Cleanup.Site)]<-"Cowell and Main Beach"
merged.data.frame$Cleanup.Site[agrep("Santa",merged.data.frame$Cleanup.Site)]<-"Cowell and Main Beach"
merged.data.frame$Cleanup.Site[grep("Main Beach",merged.data.frame$Cleanup.Site)]<-"Cowell and Main Beach"
merged.data.frame$Cleanup.Site[agrep("Hidden",merged.data.frame$Cleanup.Site)]<-"Hidden Beach"
merged.data.frame$Cleanup.Site[agrep("Panther",merged.data.frame$Cleanup.Site)]<-"Panther State Beach"
merged.data.frame$Cleanup.Site[agrep("Sunny",merged.data.frame$Cleanup.Site)]<-"Sunny Cove Beach"
merged.data.frame$Cleanup.Site[agrep("Sunset",merged.data.frame$Cleanup.Site)]<-"Sunset State Beach"
merged.data.frame$Cleanup.Site[agrep("Palm",merged.data.frame$Cleanup.Site)]<-"Palm State Beach"
merged.data.frame$Cleanup.Site[agrep("Rio",merged.data.frame$Cleanup.Site)]<-"Rio Del Mar State Beach"
merged.data.frame$Cleanup.Site[agrep("Manres",merged.data.frame$Cleanup.Site)]<-"Manresa State Beach"
merged.data.frame$Cleanup.Site[agrep("New",merged.data.frame$Cleanup.Site)]<-"New Brighton State Beach"
merged.data.frame$Cleanup.Site[agrep("Carmel",merged.data.frame$Cleanup.Site)]<-"Carmel City Beach"
# finds Del Monte Beach at Wharf 2, but not Casa Verde Beach/North Del Monte
merged.data.frame$Cleanup.Site[agrep("Del Monte Beach",merged.data.frame$Cleanup.Site)]<-"Del Monte Beach at Wharf 2"
merged.data.frame$Cleanup.Site[agrep("Corcoran",merged.data.frame$Cleanup.Site)]<-"Corcoran Lagoon"
merged.data.frame$Cleanup.Site[agrep("Bonny",merged.data.frame$Cleanup.Site)]<-"Bonny Doon Beach"
merged.data.frame$Cleanup.Site[agrep("Lover",merged.data.frame$Cleanup.Site)]<-"Lovers Point Beach"
merged.data.frame$Cleanup.Site[agrep("Black",merged.data.frame$Cleanup.Site)]<-"Blacks Beach"
merged.data.frame$Cleanup.Site[agrep("Mile",merged.data.frame$Cleanup.Site)]<-"4 Mile Beach"
merged.data.frame$Cleanup.Site[agrep("Its",merged.data.frame$Cleanup.Site)]<-"Its Beach & Lighthouse Field"
merged.data.frame$Cleanup.Site[agrep("Lighthouse",merged.data.frame$Cleanup.Site)]<-"Its Beach & Lighthouse Field"
merged.data.frame$Cleanup.Site[agrep("Casa",merged.data.frame$Cleanup.Site)]<-"Casa Verde Beach/North Del Monte"
merged.data.frame$Cleanup.Site[agrep("Ord",merged.data.frame$Cleanup.Site)]<-"Fort Ord Dunes State Beach"
merged.data.frame$Cleanup.Site[agrep("Hall",merged.data.frame$Cleanup.Site)]<-"EA Hall Middle School"
merged.data.frame$Cleanup.Site[agrep("Elkhorn",merged.data.frame$Cleanup.Site)]<-"Elkhorn Slough"
merged.data.frame$Cleanup.Site[agrep("Pleasure",merged.data.frame$Cleanup.Site)]<-"Pleasure Point"
merged.data.frame$Cleanup.Site[agrep("Sand",merged.data.frame$Cleanup.Site)]<-"Sand City Beach"
merged.data.frame$Cleanup.Site[agrep("Shark",merged.data.frame$Cleanup.Site)]<-"Sharks Tooth Beach"
merged.data.frame$Cleanup.Site[agrep("Marina",merged.data.frame$Cleanup.Site)]<-"Marina State Beach"
merged.data.frame$Cleanup.Site[agrep("Seascape",merged.data.frame$Cleanup.Site)]<-"Seascape Beach"
merged.data.frame$Cleanup.Site[agrep("Francis",merged.data.frame$Cleanup.Site)]<-"Francis State Beach"
merged.data.frame$Cleanup.Site[agrep("Scott",merged.data.frame$Cleanup.Site)]<-"Scott Creek Beach"
merged.data.frame$Cleanup.Site[agrep("Carr",merged.data.frame$Cleanup.Site)]<-"Upper Carr Lake"
merged.data.frame$Cleanup.Site[agrep("Dolphin",merged.data.frame$Cleanup.Site)]<-"Dolphin/Summer Beach"
merged.data.frame$Cleanup.Site[agrep("Pilarcitos",merged.data.frame$Cleanup.Site)]<-"Pilarcitos Creek"
merged.data.frame$Cleanup.Site[agrep("Montara",merged.data.frame$Cleanup.Site)]<-"Montara State Beach"
merged.data.frame$Cleanup.Site[agrep("Felker",merged.data.frame$Cleanup.Site)]<-"SLR at Felker St."
merged.data.frame$Cleanup.Site[agrep("Beer",merged.data.frame$Cleanup.Site)]<-"Beer Can Beach"
merged.data.frame$Cleanup.Site[agrep("Asilomar",merged.data.frame$Cleanup.Site)]<-"Asilomar State Beach"
# Now work on City.County
merged.data.frame$City.County[grep("Cruz/",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[grep("Cruz /",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[grep("santa Cruz",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[grep("Santa cruz",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[grep("Stanta Cruz",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[grep("Santa Cruz Santa Cruz",merged.data.frame$City.County)]<-"Santa Cruz/ Santa Cruz"
merged.data.frame$City.County[merged.data.frame$City.County== "Santa Cruz"]<- "Santa Cruz/ Santa Cruz"
#
merged.data.frame$City.County[agrep("Aptos",merged.data.frame$City.County)]<-"Aptos/ Santa Cruz"
merged.data.frame$City.County[agrep("Watsonville",merged.data.frame$City.County)]<- "Watsonville/ Santa Cruz"
merged.data.frame$City.County[agrep("Davenport",merged.data.frame$City.County)]<- "Davenport/ Santa Cruz"
merged.data.frame$City.County[agrep("Capitola",merged.data.frame$City.County)]<- "Capitola/ Santa Cruz"
merged.data.frame$City.County[agrep("Valley",merged.data.frame$City.County)]<- "Scotts Valley/Santa Cruz"
merged.data.frame$City.County[agrep("Live",merged.data.frame$City.County)]<- "Live Oak/ Santa Cruz"
merged.data.frame$City.County[agrep("Soquel",merged.data.frame$City.County)]<- "Soquel/ Santa Cruz"
#
merged.data.frame$City.County[agrep("Salinas",merged.data.frame$City.County)]<- "Salinas/ Monterey"
merged.data.frame$City.County[grep("Monterey/",merged.data.frame$City.County)]<-"Monterey/ Monterey"
merged.data.frame$City.County[grep("Monterey /",merged.data.frame$City.County)]<-"Monterey/ Monterey"
merged.data.frame$City.County[grep("Montery",merged.data.frame$City.County)]<-"Monterey/ Monterey"
merged.data.frame$City.County[grep("Monterey,",merged.data.frame$City.County)]<-"Monterey/ Monterey"
merged.data.frame$City.County[agrep("Pacific",merged.data.frame$City.County)]<- "Pacific Grove/ Monterey"
merged.data.frame$City.County[agrep("Carmel",merged.data.frame$City.County)]<- "Carmel/ Monterey"
merged.data.frame$City.County[agrep("Seaside",merged.data.frame$City.County)]<- "Seaside/Monterey"
merged.data.frame$City.County[agrep("Half",merged.data.frame$City.County)]<- "Half Moon Bay/San Mateo"
merged.data.frame$City.County[agrep("Moss",merged.data.frame$City.County)]<- "Moss Landing/ Monterey"
merged.data.frame$City.County[agrep("Marina",merged.data.frame$City.County)]<- "Marina/ Monterey"
merged.data.frame$City.County[agrep("Sand",merged.data.frame$City.County)]<- "Sand City/ Monterey"
# move NAs to "Other"
levels(merged.data.frame$City.County)<- c(levels(merged.data.frame$City.County),"Other")
merged.data.frame$City.County[is.na(merged.data.frame$City.County)] <- "Other"
levels(merged.data.frame$Cleanup.Site)<- c(levels(merged.data.frame$Cleanup.Site),"Other")
merged.data.frame$Cleanup.Site[is.na(merged.data.frame$Cleanup.Site)] <- "Other"
# drop unused levels
#merged.data.frame$Cleanup.Date <- as.Date(merged.data.frame$Cleanup.Date, format = "%m/%d/%Y")
library(dplyr)
mdata <- merged.data.frame %>% 
  select(year, Name, Group.Name, Cleanup.Site,Cleanup.Date, 
         Cleanup.Area,X..of.Adults,X..of.Youth,Volunteer.Hours,
         Pounds.of.Trash.Collected,Pounds.of.Recycle.Collected,
         everything())
mdata <- droplevels(mdata)
write.csv(mdata, file="../SOSdata/Merged.csv",row.names=FALSE, na="")

