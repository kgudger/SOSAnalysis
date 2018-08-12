read_rename <- function(csv) {
  df <- read.csv(csv)
  colnames(df)[agrep("Cigarette.Butts",colnames(df))] <- 
    "Cigarette.Butts"
  colnames(df)[agrep("Plastic.Bags.Grocery",colnames(df))] <- 
    "Plastic.Bags.Grocery"
  colnames(df)[agrep("Plastic.straws.stirrers",colnames(df))] <- 
    "Plastic.straws.stirrers"
  colnames(df)[agrep("Syringes",colnames(df))] <- 
    "Syringes.or.Needles"
  colnames(df)[agrep("Cardboard",colnames(df))] <- 
    "Cardboard"
  colnames(df)[agrep("Tires",colnames(df))] <- 
    "Tires"
  colnames(df)[agrep("id20",colnames(df))] <- 
    "id"
  colnames(df)[agrep("Metal.Cans",colnames(df))] <- 
    "Metal.Cans"
  colnames(df)[agrep("Metal.Bottle.Caps",colnames(df))] <- 
    "Metal.Bottle.Caps"
  colnames(df)[agrep("Metal.Can.Pulls",colnames(df))] <- 
    "Metal.Can.Pulls"
  colnames(df)[agrep("Plastic.Bottle.Caps",colnames(df))] <- 
    "Plastic.Bottle.Caps"
  colnames(df)[grep("Plastic.Bottles",colnames(df),ignore.case=TRUE)] <-
    "Plastic.Bottles"
  colnames(df)[grep("Glass.Bottles",colnames(df),ignore.case=TRUE)] <-
    "Glass.Bottles"
  colnames(df)[agrep("Fireworks",colnames(df))] <- 
    "Fireworks"
  colnames(df)[grep("Styrofoam.Cups|Styrofoam.food.containers|Polystyrene.food..to.go|Polystyrene.cups",colnames(df))] <- 
    "Styrofoam.Cups.Plates.Bowls"
  colnames(df)[grep("Styrofoam.Pieces|Pieces..Styrofoam",colnames(df),ignore.case=TRUE)] <-
    "Styrofoam.Pieces"
  colnames(df)[agrep("Cigarette.Lighters",colnames(df))] <- 
    "Cigarette.Lighters"
  colnames(df)[grep("Metal.Fishing.hooks|Lures.or.Hooks",colnames(df),ignore.case=TRUE)] <-
    "Metal.Fishing.Hooks.Lures"
  colnames(df)[grep("Condoms",colnames(df),ignore.case=TRUE)] <-
    "Condoms"
  colnames(df)[grep("Diapers",colnames(df),ignore.case=TRUE)] <-
    "Diapers"
  colnames(df)[grep("Pallets",colnames(df),ignore.case=TRUE)] <-
    "Pallets"
  return(df)
}
b2017data <- read_rename("../SOSdata/2017-working-transpose.csv")
b2017adata <- read_rename("../SOSdata/2017acc-working-transpose.csv")
b2016data <- read_rename("../SOSdata/2016-working-transpose.csv")
b2016adata <- read_rename("../SOSdata/2016acc-working-transpose.csv")
b2015data <- read_rename("../SOSdata/2015-working-transpose.csv")
b2014data <- read_rename("../SOSdata/2014-working-transpose.csv")
b2013data <- read_rename("../SOSdata/2013-working-transpose.csv")
b2012data <- read_rename("../SOSdata/2012-working-transpose.csv")
b2011data <- read_rename("../SOSdata/2011-working-transpose.csv")
b2010data <- read_rename("../SOSdata/2010-working-transpose.csv")
b2009data <- read_rename("../SOSdata/2009-working-transpose.csv")
b2008data <- read_rename("../SOSdata/2008-working-transpose.csv")
list.of.data.frames = list(b2017data, b2017adata, b2016data, b2016adata, b2015data, b2014data, b2013data, b2012data, b2011data, b2010data, b2009data, b2008data)
library(plyr)
merged.data.frame <- join_all(list.of.data.frames,type='full')
#merged.data.frame = Reduce(function(...) merge(..., all=T), list.of.data.frames)
#tail(merged.data.frame)
# clean up data, remove NAs
merged.data.frame$Pounds.of.Trash.Collected <- as.numeric(as.character(merged.data.frame$Pounds.of.Trash.Collected))
ind <- which(sapply(merged.data.frame, is.numeric))
for(j in ind){
  merged.data.frame[[j]] <- as.numeric(as.character(merged.data.frame[[j]]))
#  merged.data.frame[[j]] <- replace(merged.data.frame[[j]],is.na(merged.data.frame[[j]]),0)
}
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

