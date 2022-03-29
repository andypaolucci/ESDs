library(aqp)
library(soilDB)
library(sf)

# get a SoilProfileCollection of all components in Soil SUrvey. Specify Soil Survey Area
f <- fetchSDA(WHERE=(areasymbol, 2) = 'WI', duplicates=TRUE)
head(f)
unique(f$mukey)
length(unique(f$mukey))

<- SDA_query("SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol = 'CA653'")


# Get list of components with duplicate ES assignments 
t <- get('component.ecosite.problems', envir = soilDB.env)

# Convert to character
a <- as.character(t)

# subset each profile with duplicate ecological site 
f.1 <- filter(f, grepl("19883002", cokey, ignore.case=TRUE))
f.1$compname
f.1$mukey
f.1$nationalmusym
f.1$ecoclassid

f.2 <- filter(f, grepl("19884614", cokey, ignore.case=TRUE))
f.2$compname
f.2$mukey
f.2$nationalmusym
f.2$ecoclassid

f.3 <- filter(f, grepl("19886516", cokey, ignore.case=TRUE))
f.3$compname
f.3$mukey
f.3$nationalmusym
f.3$ecoclassid

f.4 <- filter(f, grepl("19941422", cokey, ignore.case=TRUE))
f.4$compname
f.4$mukey
f.4$nationalmusym
f.4$ecoclassid

f.5 <- filter(f, grepl("19942232", cokey, ignore.case=TRUE))
f.5$compname
f.5$mukey
f.5$nationalmusym
f.5$ecoclassid

f.6 <- filter(f, grepl("19944423", cokey, ignore.case=TRUE))
f.6$compname
f.6$mukey
f.6$nationalmusym
f.6$ecoclassid

f.7 <- filter(f, grepl("19948009", cokey, ignore.case=TRUE))
f.7$compname
f.7$mukey
f.7$nationalmusym
f.7$ecoclassid

f.8 <- filter(f, grepl("19952389", cokey, ignore.case=TRUE))
f.8$compname
f.8$mukey
f.8$nationalmusym
f.8$ecoclassid

f.9 <- filter(f, grepl("19955824", cokey, ignore.case=TRUE))
f.9$compname
f.9$mukey
f.9$nationalmusym
f.9$ecoclassid

f.10 <- filter(f, grepl("19955826", cokey, ignore.case=TRUE))
f.10$compname
f.10$mukey
f.10$nationalmusym
f.10$ecoclassid

f.11 <- filter(f, grepl("19956964", cokey, ignore.case=TRUE))
f.11$compname
f.11$mukey
f.11$nationalmusym
f.11$ecoclassid

f.12 <- filter(f, grepl("19960136", cokey, ignore.case=TRUE))
f.12$compname
f.12$mukey
f.12$nationalmusym
f.12$ecoclassid

f.13 <- filter(f, grepl("19960139", cokey, ignore.case=TRUE))
f.13$compname
f.13$mukey
f.13$nationalmusym
f.13$ecoclassid

f.14 <- filter(f, grepl("19960142", cokey, ignore.case=TRUE))
f.14$compname
f.14$mukey
f.14$nationalmusym
f.14$ecoclassid

f.15 <- filter(f, grepl("20286878", cokey, ignore.case=TRUE))
f.15$compname
f.15$mukey
f.15$nationalmusym
f.15$ecoclassid

f.16 <- filter(f, grepl("20287286", cokey, ignore.case=TRUE))
f.16$compname
f.16$mukey
f.16$nationalmusym
f.16$ecoclassid

f.17 <- filter(f, grepl("20287287", cokey, ignore.case=TRUE))
f.17$compname
f.17$mukey
f.17$nationalmusym
f.17$ecoclassid

f.18 <- filter(f, grepl("20287306", cokey, ignore.case=TRUE))
f.18$compname
f.18$mukey
f.18$nationalmusym
f.18$ecoclassid

f.19 <- filter(f, grepl("20287352", cokey, ignore.case=TRUE))
f.19$compname
f.19$mukey
f.19$nationalmusym
f.19$ecoclassid


f.20 <- filter(f, grepl("20287355", cokey, ignore.case=TRUE))
f.20$compname
f.20$mukey
f.20$nationalmusym
f.20$ecoclassid


f.21 <- filter(f, grepl("20287359", cokey, ignore.case=TRUE))
f.21$compname
f.21$mukey
f.21$nationalmusym
f.21$ecoclassid

#combine into one SPC
f.all <- union(list(f.1, f.2, f.3, f.4, f.5, f.6, f.7, f.8, f.9, f.10, f.11, f.12, f.13, f.14, f.15, f.16, f.17, f.18, f.19, f.20, f.21))


library(knitr)
#Get list of mukeys
x <- site(f.all)
x.2 <-(x$cokey, x$mukey, x$nationalmusym, x$compname, x$comppct_r, x$ecoclassid)


# use MUKEY to download the SDA spatial data (takes a little bit)
# optionally load it from file -- though you will 
spatial <- fetchSDA_spatial(unique(f.all$mukey), chunk.size = 1)
library(rgdal)
plot(spatial)

head(spatial)
writeOGR(spatial, "C:/Paolucci/Projects/ESDs", layer='duplicateES_10_21_2020', driver='ESRI Shapefile')



# use mukey, a site-level attribute, to split components into mapunits
f.mukey <- aqp::split(f, 'mukey')

# determine mapunit dominant condition for a site-level attribute attr
#   where dmu is a SPC where all profiles have identical MUKEY  
dominant.condition <- function(dmu, attr) {
  # get needed vars
  mukey <- unique(dmu$mukey)
  d.attr <- dmu[[attr]]
  d.comppct <-  dmu[["comppct_r"]]
  
  # sum up comppct for each level of attr
  res <- aggregate(d.comppct, list(d.attr), sum)
  if(!nrow(res))
    res <- data.frame(a=character(0), b=character(0))
  
  # select the first occurence of maximum value
  #  might not be stable if two attr have same percentage
  suppressWarnings(res <- res[which(res$x == max(res$x, na.rm=TRUE))[1],])
  names(res) <- c("dominant_condition", "dominant_condition_pct")
  
  # create data.frame output
  return(data.frame(mukey=mukey, res))
}

# example usage
dominant.condition(f.mukey[[1]], "ecoclassid") 

# calculate for all MUKEYs in CA630
dominant.condition.table <- do.call('rbind', lapply(f.mukey, dominant.condition, "ecoclassid"))

# join in identifying info
q <-  paste0("select * from mapunit where mukey in ",
             format_SQL_in_statement(dominant.condition.table$mukey))
legendinfo <- SDA_query(q)

# add legend info to spatial layer
spatial_sf <- st_as_sf(spatial)
spatial_sf <- merge(spatial_sf, legendinfo, sort=FALSE)

# add dominant condition info to spatial layer
spatial_sf <- merge(spatial_sf, dominant.condition.table, sort=FALSE)

# output # Specify path 
plot(spatial_sf[,'dominant_condition'], lty=0)
st_write(spatial_sf, "C:/Paolucci/Projects/CA630/CA630_ESDs/EcositeMap/CA630_ES_dominant_condition.shp")

#Read shapefile
#CA630 <-shapefile("C:/Paolucci/Projects/CA630/CA630_ESDs/EcositeMap/CA630_ES_dominant_condition.shp")

