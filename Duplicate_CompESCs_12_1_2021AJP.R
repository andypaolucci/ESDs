#load package
library(soilDB)
library(aqp)

#set up selected set in NASIS. I used the query: "Whole legend by area symbol!= additional and DMU = yes ++" Under MLRA02_Davis with WI*** to get all the legends in WI 

#Get Component Data From NASIS
comp <- get_component_data_from_NASIS_db()

#omponents with multiple ecosites per component
problems <- get('multiple.ecosite.per.coiid', envir=soilDB.env)

#subset df by rows where component ID matches list of coiids from previous step
comp.problems <- subset(comp, (coiid %in% problems))

#Export to CSV. Specify file path with .csv at the end
#Optional: write.csv(comp.subset, file="c:/Paolucci/ESDs/duplicate_ecosites.csv", row.names=FALSE)


# Get Component ESD Data and Other Veg Data From NASIS
comp.esd <- get_component_esd_data_from_NASIS_db()


#join comp.esd with comp data
comp.full <-merge(comp.esd, comp, by='coiid')

# load correlation data
comp.corr <- get_component_correlation_data_from_NASIS_db()

#join correlation and component data
comp.full.corr <- merge(comp.full, comp.corr, by='dmuiid')

#subset only wanted columns 
df <- subset(comp.full.corr, select = c(dmuiid, coiid, ecositeid, ecositenm, ecositemlra, dmudesc, compname, comppct_r, localphase, taxsubgrp, lmapunitiid, muiid, musym, nationalmusym, muname, mustatus) )
head(df)

# use lmapunitiid to split components into mapunits
f.lmapunitiid <- aqp::split(df, 'lmapunitiid')
head(f.lmapunitiid)

# determine mapunit dominant condition for component attr
#   where dmu is a SPC where all profiles have identical MUKEY  
dominant.condition <- function(dmu, attr) {
  # get needed vars
  lmapunitiid<- unique(dmu$lmapunitiid)
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
  return(data.frame(lmapunitiid=lmapunitiid, res))
}

# example usage
dominant.condition(f.lmapunitiid[[1]], "ecositeid") 

# calculate for all lmapunitiids
dominant.condition.table <- do.call('rbind', lapply(f.lmapunitiid, dominant.condition, "ecositeid"))
head(dominant.condition.table)
