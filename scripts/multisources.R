source("scripts/settings.R")

tile <- "m4"
filename_rdata <- "dt.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)

# Load rdata
dt <- fread(path_rdata)


# Rename columns
colnames(dt) <- c("x","y","biomass_spruce","biomass_bl","biomass_pine","age","fert","dbh","h","ba")

# Run garbage collection
gc()

# Total biomass
dt[, biomass_total := rowSums(.SD), .SDcols = 3:5]

# Run garbage collection
gc()

# Biomass shares
dt[, biomass_spruce_share := biomass_spruce/biomass_total]
dt[, biomass_bl_share := biomass_bl/biomass_total]
dt[, biomass_pine_share := biomass_pine/biomass_total]

# Run garbage collection
gc()

# Ba shares
dt[, ba_spruce_share := biomass_spruce_share*ba]
dt[, ba_bl_share := biomass_bl_share*ba]
dt[, ba_pine_share := biomass_pine_share*ba]

# Run garbage collection
gc()

# Variable for processed table
dt_processed <- dt

# Remove old var
rm(dt)

# Run garbage collection
gc()





tile <- "m4"
filename_rdata <- "dt_m4_processed.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)


# Load rdata
dt_processed <- fread(path_rdata)

gc()

# Assign group ids
dt_processed[, groupID := .GRP, by=list(x,y)]


gc()


# Drop na rows
comp_dt <- dt_processed[complete.cases(dt_processed),]


rm(dt_processed)
gc()


keep_cols <- c("x","y","age","fert","dbh","h","ba_spruce_share","ba_bl_share","ba_pine_share","groupID")

# Drop unnecessary cols
cols_dt <- comp_dt[, ..keep_cols]

rm(comp_dt)
gc()


# Remove cloud cover pixels
cols_dt <- cols_dt[fert!=32766]

gc()

# Melt
melted <- melt.data.table(cols_dt, 
                             measure.vars = c("ba_pine_share","ba_spruce_share", "ba_bl_share"), value.name = "ba")

rm(cols_dt)
gc()

# Assign speciesIDs
melted[, speciesID := .GRP, by=variable]

gc()

melted[,variable:=NULL]

gc()

# Sort
setorder(melted, cols="groupID")

gc()

# # Bl speciesID to 99
# melted[speciesID==3, speciesID := 99]

# gc()

# Height from dm to m
melted[, h := h/10]

gc()

filtered <- unique(melted[,c("x", "y", "groupID")])

rm(melted)
gc()

dt_ids <- filtered

tile <- "m4"
filename_rdata <- "dt_ids.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)
# fwrite(dt_ids,file=path_rdata, row.names = F)











sites <- melted[groupID<22]

rm(melted)
gc()


nSites <- length(unique(sites$groupID))
nLayers <- (sites %>% count(groupID))$n
nSpecies <- (sites %>% count(speciesID,groupID) %>% count(groupID))$n

siteInfo[,8] <- nLayers
siteInfo[,9] <- nSpecies

maxNlayers <- max(nLayers)



multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
multiInitVar[,6:7,NA]
for(i in 1:nSites){
  filtered <- sites %>% filter(groupID==i)
  multiInitVar[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
  multiInitVar[i,2,1:nLayers[i]] <- filtered$age # age by tree from NFI
  multiInitVar[i,3,1:nLayers[i]] <- filtered$h # height from NFI data
  multiInitVar[i,4,1:nLayers[i]] <- filtered$dbh # dbh from NFI data
  multiInitVar[i,5,1:nLayers[i]] <- filtered$ba # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
  multiInitVar[i,6,1:nLayers[i]] <- NA
}
























