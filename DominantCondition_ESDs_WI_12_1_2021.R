#load package
library(soilDB)
library(aqp)


#set up selected set in NASIS. I used the query: "Whole legend by area symbol!= additional and DMU = yes ++" Under MLRA02_Davis with WI*** to get all the legends in WI 

#Get Component Data From NASIS
comp <- get_component_data_from_NASIS_db()
comp.esd <- get_component_esd_data_from_NASIS_db()
comp.corr <- get_component_correlation_data_from_NASIS_db()

comp.full <- merge(comp, comp.esd, by='coiid', all = TRUE)
comp.full.corr <- merge(comp.full, comp.corr, by='dmuiid', all=TRUE)
str(comp.mu)


#components with multiple ecosites per component
problems <- get('multiple.ecosite.per.coiid', envir=soilDB.env)

#subset df by rows where component ID matches list of coiids from previous step
comp.problems <- subset(comp, (coiid %in% problems))

#Export to CSV. Specify file path with .csv at the end
#Optional: write.csv(comp.subset, file="c:/Paolucci/ESDs/duplicate_ecosites.csv", row.names=FALSE)

# Get Component ESD Data and Other Veg Data From NASIS
comp.esd <- get_component_esd_data_from_NASIS_db()

# If multiple sites are assigned this takes the first site assigned 
comp.esd <- dplyr::slice(group_by(comp.esd, coiid), 1)

#join comp.esd with comp data. Keeping components with no ESD assigned 
comp.full <-merge(comp.esd, comp, by='coiid', all.y=TRUE)
head(comp.full)

test <- merge(comp.full, mu, by='dmuiid', all.x=TRUE)

head(test)

# load correlation data
comp.corr <- get_component_correlation_data_from_NASIS_db()
head(comp.corr)
#join correlation and component data
comp.full.corr <- merge(comp.full, comp.corr, by='dmuiid', all.x=TRUE, all.y=TRUE)

str(comp.full.corr)

#subset only wanted columns 
df <- subset(comp.full.corr, select = c(dmuiid, coiid, ecositeid, ecositenm, ecositemlra, dmudesc, compname, compkind, comppct_r, slope_l, slope_h, localphase, taxsubgrp, lmapunitiid, muiid, musym, mukind, repdmu, ssastatus, nationalmusym, muname, mustatus, areaname, areasymbol) )
head(df)

#write to csv 
write.csv(df, file="c:/Paolucci/ESDs/WI_NASIS_Legend_short_12_9_2021.csv", row.names=FALSE)

# use lmapunitiid to split components into mapunits
f.lmapunitiid <- base::split(df, df$lmapunitiid)
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

#write to csv 
write.csv(dominant.condition.table, file="c:/Paolucci/ESDs/pESD_Status.csv", row.names=FALSE)
