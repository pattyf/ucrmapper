# 
# UCR data - manipulating in R
# 
# data downloaded from:
#
# In this script we are processing Violent Crime (VC) data for all US cities with> 50K persons
# Though we could just keep all data summaries & factor on group (which indicates city size)

#clean start
rm(list = ls())
setwd("/Users/pattyf/ucrdata/data")
options(digits=3)  #keep rates simple

#load libraries
library(foreign) #for reading dta files
library(stringr) #for reformatting dates
library(sp) # for creating and working with spatial data objects
library(rgdal) # for assigning and transforming spatial coordinates
library(ggplot2) #plotting
library(plyr) #cleaning
library(plotly) # for putting plots on plotly website to share
library("R.utils")
library(maps) #for getting geodata for cities
library(leafletR) # for writing data to json for web

simpleCap <- function(x) {
  # function simpleCap to standardize city names
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

fourDigitYear <- function(x) {
  #function to convert single/double digit date to year
  if (x > 59) {
    x <- 1900 + x
  } else {
    x <- 2000 + x
  }
  return(x)
}

#list of all ucr files to process, out of set 1960 - 2010
# in format: "ucr2005.dta"
#myfiles <- Sys.glob("u*.dta")
#format of data files on website: ucr1960.dta.gz
#location of data is justin's web site directory
my_url = "http://eml.berkeley.edu/~jmccrary/UCR/"
startyear <- 1960
endyear <- 2010

for (i in startyear:endyear) {
  infile <- paste0("ucr",i,".dta")
  gzfile <- paste0("ucr",i,".dta.gz")
  myurl <- paste0(my_url,gzfile)

  if (!file.exists(infile)) {
    print(paste0("The file [",infile,"] does NOT exist so checking if GZ file exists."))
    if (!file.exists(gzfile)) {
      print(paste0("The file [",gzfile,"] does not exist so downloading"))
      download.file(myurl,gzfile,method="wget")
    } else {
      print(paste0("The file [",gzfile,"] does exist so NOT downloading"))
    }
    print(paste0("unzipping file:", gzfile))
    gunzip(gzfile)
    
  } else {
    print(paste0("The file [",infile,"] exists so not downloading and unzipping"))
  }

  ## AFTER WE process the file, remove it using file.remove(infile) but keep the gz files
 
  #NOW WE HAVE FILE LOCALLY TO PROCESS
  # read in and process data sequentially from each file
	ucr <- read.dta(infile)
  
	# keep data for cities where popl >= 50k (group value contains 1, 2 or 3)
	city50k <- subset(ucr, (grepl("1",group) | group == "2" | group == "3"))
  #Standardize format of city name so we can join to spatial data
	city50k$city <- sapply(city50k$agency_name, simpleCap)
  city50k$citystate <- paste0(city50k$city, " ",substr(city50k$ori,1,2))
  #standardize format of year
	city50k$year <- sapply(city50k$year, fourDigitYear)

	# reduce complexity by removing rows where num_months != 12
	##### HEY SINCE NOT KEEPING MONTH DATA DONT NEED TO DO THIS ANYMORE
	#you can review this by table command
	#table(cacity25k$num_months)
	#cacity50k <- subset(cacity50k, num_months == "12")

	#  extract columns only for key violent crimes of interest 
	#murder = f1
	#manslaughter = f2
	#rape, total (forcible & attempted)  = f3
	#robberies, total f6
  #agg assault total = f11

  #ucr columns we want to keep
	ucr_cols <- c('ori','group','year','pop1','city','citystate')
  #crime columns to sum on ./ collapse
	murder_cols <- c('c1f1_1','c1f1_2','c1f1_3','c1f1_4','c1f1_5','c1f1_6','c1f1_7','c1f1_8','c1f1_9','c1f1_10','c1f1_11','c1f1_12')
	mans_cols <- c('c1f2_1','c1f2_2','c1f2_3','c1f2_4','c1f2_5','c1f2_6','c1f2_7','c1f2_8','c1f2_9','c1f2_10','c1f2_11','c1f2_12')
	rape_cols <- c('c1f3_1','c1f3_2','c1f3_3','c1f3_4','c1f3_5','c1f3_6','c1f3_7','c1f3_8','c1f3_9','c1f3_10','c1f3_11','c1f3_12')
	rob_cols <- c('c1f6_1','c1f6_2','c1f6_3','c1f6_4','c1f6_5','c1f6_6','c1f6_7','c1f6_8','c1f6_9','c1f6_10','c1f6_11','c1f6_12')
	aggass_cols <- c('c1f11_1','c1f11_2','c1f11_3','c1f11_4','c1f11_5','c1f11_6','c1f11_7','c1f11_8','c1f11_9','c1f11_10','c1f11_11','c1f11_12')
	
  #subset dataset by col name only keeping our columns of interest
	city50k2 <- city50k[ucr_cols]

	#Compute Total murders & add to our data frame
	#cacity50k2$murder_tot <- rowSums(cacity50k[,murder_cols])
  murderNmans <- cbind(murder_cols, mans_cols)
  city50k2$murder_tot <- rowSums(city50k[,murderNmans])
	 
	#Compute Total manslaughter & add to our data frame
	#cacity50k2$mans_tot <- rowSums(cacity50k[,mans_cols])

	#compute Total rape & add to our data frame
	city50k2$rape_tot <- rowSums(city50k[,rape_cols])
	 
	#compute Total rob & add to our data frame
	city50k2$rob_tot <- rowSums(city50k[,rob_cols])
	
	#compute Total aggrevated assuault & add to our data frame
	city50k2$agg_tot <- rowSums(city50k[,aggass_cols])
  
  #######
  # RATES
  ########
	#Compute total murders_per100k & add to our data frame
	city50k2$murder_per100k <- city50k2$murder_tot / (city50k2$pop1 / 100000)
	 
	#Compute manslaughter mans_per100k
	#city50k2$mans_per100k <- city50k2$mans_tot / (city50k2$pop1 / 100000)
	# 
	#Compute total rape rape_per100k 
	city50k2$rape_per100k <- city50k2$rape_tot / (city50k2$pop1 / 100000)
 
	#Compute total robery rates
	city50k2$rob_per100k <- city50k2$rob_tot / (city50k2$pop1 / 100000)
 
	#Compute total agg ass rates 
	city50k2$agg_per100k <- city50k2$agg_tot / (city50k2$pop1 / 100000)
 
  #Compute TOTAL VC rate
  city50k$vc_per100k
  
  # Combine all years into one data frame called city_vc for violent crimes
	if (!exists("city_vc")){
    #if no data frame exists create it
		city_vc <- city50k2
	} else {
    # else append to it
		city_vc <-rbind(city_vc,city50k2)
	}
}

#At this point we have yearly summaries for each city and crime type of interest
# let's only keep complete cases
city_vc <- na.omit(city_vc)
# not sure if this is necessary or even a good idea
factor(city_vc$year)

# RATES - for all VC
#TOTAL Violent Crimes
city_vc$vc_total <- rowSums(city_vc[,c("agg_tot","rob_tot","rape_tot","murder_tot")]) 
city_vc$vc_per100k <- city_vc$vc_total/ (city_vc$pop1 / 100000)

usa_vc_mean <- ddply(city_vc,"year",summarize,mean=mean(vc_per100k))
head(usa_vc_mean)
ggplot(usa_vc_mean,aes(x=year,y=mean)) +geom_line()

# I think below is better but it doesnt match justin's data
# usa_vc_per100k <- ddply(city_vc2,"year",summarize,myrate=sum(vc_per100k)/(sum(pop1)/100000))
#ggplot(usa_vc_per100k,aes(x=year,y=myrate)) +geom_line()

## In this example we are grabbing the points for each city from the maps package
## and then plotting bubble plot
## The problem with this is that the bubbles overlap so no familiarity with city - hard to interpret
## This approach might work with something other than a bubble plot.
#require(maps)
data(us.cities)
head(us.cities)
## Get just the cities just for CA
## So that we can have the lat long values for plotting points
#ca_pts <- us.cities[us.cities$country.etc == "CA",]
#ca_pts$city <- gsub(" CA", "", ca_pts$name)
#WE DONT DO THE ABOVE IF OUR SET INCLUDES ALL STATES

## Promote this data frame to a SpatialPointsDataFrame
## by stating which columns contain the x and y coordinates
#coordinates(ca_pts) <- ~long + lat
city_vc_lonlat <- merge(city_vc, us.cities, by.x = "citystate", by.y = "name")
head(city_vc_lonlat)

#drop unnecessary columns
city_vc_lonlat <- subset(city_vc_lonlat, select=-c(country.etc,pop,capital))
head(city_vc_lonlat)
#write data out to csv now so we can import to postgres
#write.csv(city_vc_lonlat,"city50k_vc_lonlat.csv",row.names=FALSE)

#write files for each rate of interest
#keep_cols <- c("lat","long","ori", "group", "year","pop1","citystate")
keep_cols <- c("lat","long","year","pop1","citystate")
city50k_mur <- city_vc_lonlat[,c(keep_cols,"murder_per100k")]
city50k_vc <- city_vc_lonlat[,c(keep_cols,"vc_per100k")]
city50k_rob <- city_vc_lonlat[,c(keep_cols,"rob_per100k")]
city50k_rape <- city_vc_lonlat[,c(keep_cols,"rape_per100k")]
city50k_agg <- city_vc_lonlat[,c(keep_cols,"agg_per100k")]

#sort it do in leaflet we can control draw order and have highest vals display on top
city50k_mur <- city50k_mur[ order(city50k_mur[,6]),]
city50k_vc <- city50k_vc[ order(city50k_vc[,6]),]
city50k_rob <- city50k_rob[ order(city50k_rob[,6]),]
city50k_rape <- city50k_rape[ order(city50k_rape[,6]),]
city50k_agg <- city50k_agg[ order(city50k_agg[,6]),]

#write out data to a csv files
write.csv(city50k_vc,"city50k_vc.csv",row.names=FALSE)
write.csv(city50k_mur,"city50k_mur.csv",row.names=FALSE)
write.csv(city50k_rape,"city50k_rape.csv",row.names=FALSE)
write.csv(city50k_rob,"city50k_rob.csv",row.names=FALSE)
write.csv(city50k_agg,"city50k_agg.csv",row.names=FALSE)

#Write out to a geojson files
toGeoJSON(city50k_mur,"city50k_mur")
toGeoJSON(city50k_vc,"city50k_vc")
toGeoJSON(city50k_rob,"city50k_rob")
toGeoJSON(city50k_rape,"city50k_rape")
toGeoJSON(city50k_agg,"city50k_agg")

#==================================================================
#

make_plots <- 0
if (make_plots == 1) {

#plotting means
  usa_murder_mean <- ddply(city_vc,"year",summarize,mean=mean(murder_per100k))
  head(usa_murder_mean)
  ggplot(usa_murder_mean,aes(x=year,y=mean)) +geom_line()
  
  
#try some plots
city_vc_ll_2010 <- subset(city_vc_lonlat,year=='2010')
ggplot(city_vc_ll_2010,aes(x = long, y = lat,color=murder_per100k)) + geom_point() + scale_color_gradient(low="grey", high="red")

##
###########################################
##
ggplot(ca_all_ll_2010,aes(x = long, y = lat,color=murder_per100k)) + geom_point() + scale_color_gradient(low="grey", high="red")
mid<-mean(ca_all_ll_2010$murder_per100k)
ggplot(ca_all_ll_2010,aes(x = long, y = lat,color=murder_per100k)) + geom_point() + scale_color_gradient2(midpoint=mid,low="blue", mid="white", high="red")
#plot points in order, low values to high so high on top and real noticeable
ggplot(ca_all_ll_2010[order(ca_all_ll_2010$murder_per100k),],aes(x = long, y = lat,color=murder_per100k)) + geom_point() + scale_color_gradient2(midpoint=mid,low="blue", mid="white", high="red")

#grab a subset of data to try in plotly and cartodb
#plotly turned out to be tooo slow

#city murder_per100k,lat, long - just get these columns
options(digits=3) #limit sig digits
ca_2010_murders <- ca_all_ll_2010[c('city','murder_per100k','lat','long')]
ggplot(ca_2010_murders[order(ca_2010_murders$murder_per100k),],aes(x = long, y = lat,color=murder_per100k)) + geom_point() + scale_color_gradient2(midpoint=mid,low="blue", mid="white", high="red")
#write.csv(ca_2010_murders,"ca_2010_murderper100k.csv",row.names=FALSE)
install.packages("htmlwidgets")
#############################################
#creare spatial daa set
#or export to shapefile to load into postgres
ca_all_pts <- ca_all_lonlat #make a copy

coordinates(ca_all_pts) <- ~long+lat #promote to a spatial points data frame
bubble(ca_all_pts[ca_all_pts$year =='2005',],"murder_per100k", fill=FALSE,do.sqrt=TRUE)

#Lets get the polygons for all california cities
#to do thematic mapping by crime rate
#Requires library(sp) and library(rgdal)
cpolys <- readOGR(dsn="cacitysimp.shp",layer="cacitysimp")
#plot(cploys)

#fortify the polys which means i think turn them into a data frame with id of region value
#so we can use ggplot to plot them as polys with all the goodness that comes with ggplot
#requires libraries ggplot2 and plyr
ca.polys2 <- fortify(cpolys, region="NAME")
#for some reason the above command will make the id field in ca.polys2 be "id" and not "NAME"
ca_all$id <- ca_all$city # so we create an id column in our ucr data frame
cacityall.df <- join(ca.polys2,ca_all,by="id") #and then join the polys to the data (big data frame)

#plot rape rate by year
ggplot(cacityall.df[!is.na(cacityall.df$rape_per100k),], aes(x = long, y = lat, group = group, fill = rape_per100k,na.rm=TRUE)) + geom_polygon(colour = "white", size = 0.01, aes(group = group),na.rm=T) + theme() + scale_fill_gradient(low="white", high="red") +facet_wrap(~year, nrow=2)
d

#plot murder rate by year
#ggplot(cacityall.df[!is.na(cacityall.df$murder_per100k),], aes(x = long, y = lat, group = group, fill = murder_per100k,na.rm=TRUE)) + geom_polygon(colour = "white", size = 0.01, aes(group = group),na.rm=T) + theme() + scale_fill_gradient(low="white", high="blue") +facet_wrap(~year, nrow=2)

#plot robbery rate by year
#ggplot(cacityall.df[!is.na(cacityall.df$rob_per100k),], aes(x = long, y = lat, group = group, fill = rob_per100k,na.rm=TRUE)) + geom_polygon(colour = "white", size = 0.01, aes(group = group),na.rm=T) + theme() + scale_fill_gradient(low="white", high="green") +facet_wrap(~year, nrow=2)

#try this color scale
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#create-a-single-row-of-plots-based-on-one-variable-facet_wrap
#it shows us the long right tail in the data (a few cities with very high rates)
#ggplot(cacityall.df[!is.na(cacityall.df$murder_per100k),], aes(x = long, y = lat, group = group, fill = murder_per100k,na.rm=TRUE)) + geom_polygon(colour = "white", size = 0.01, aes(group = group),na.rm=T) + theme() +scale_fill_gradient2(midpoint=mid, low="blue", mid="white", high="red" ) +facet_wrap(~year, nrow=2)

#try this
# http://zevross.com/blog/2014/09/10/quickly-create-online-and-interactive-plots-using-plot-ly/
# doesn't work too much data
#library(plotly)
#py <- plotly(username="pfrontiera", key="XXXXX")
#response<-py$ggplotly()
#then check my plotly account for interactive plot

#write to csv
#write.csv(k, "ca_ucr_nona2.csv", row.names=FALSE)

#writeout a shapefile
#ca25k2010 <- merge(cpolys,k2[k2$year == '2010',],by.x="NAME", by.y="city")
#writeOGR(ca25k2010,".","ca25k2010",driver="ESRI Shapefile")

} 
# end make_plots
