source("scripts/settings.R")



# Set tile and paths
tile <- "m4"
filename_rdata <- "dt.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)

# Load rdata
dt <- fread(path_rdata)

# Rename columns
colnames(dt) <- c("x","y","biomass_spruce","biomass_bl","biomass_pine","age","fert","dbh","h","ba")

# Assign group ids
dt[, groupID := .GRP, by=list(x,y)]

# Remove NAs
dt <- dt[complete.cases(dt),]

# # Remove cloud cover and other no value pixels
dt <- dt[fert!=32766]
dt <- dt[fert!=32767]


# Total biomass
dt[, biomass_total := rowSums(.SD), .SDcols = 3:5]

# Run garbage collection
gc()



# Biomass shares. If total biomass is 0 then pine and spruce are 0.4 and birch is 0.2
dt[, biomass_pine_share := fifelse(biomass_total == 0, 0.4, biomass_pine/biomass_total)]
dt[, biomass_spruce_share := fifelse(biomass_total == 0, 0.4, biomass_spruce/biomass_total)]
dt[, biomass_bl_share := fifelse(biomass_total == 0, 0.2, biomass_bl/biomass_total)]


# CLEARCUTS


# Height from dm to m
dt[, h := h/10]

# Convert h, dbh and ba to double
dt$h <- as.double(dt$h)
dt$dbh <- as.double(dt$dbh)
dt$ba <- as.double(dt$ba)

# Init values
init_h <- initSeedling.def[1]
init_dbh <- initSeedling.def[2]
init_ba <- initSeedling.def[3]


# Init values when ba is 0
dt[ba == 0, c("h", "dbh", "ba") := list(init_h, init_dbh, init_ba)]


gc()



# COEFFS AT START!!


# LINEAR MODELS

# Linear relationship between ba and dbh
ba_dbh_rel <- lm(dt$dbh~dt$ba)
ba_dbh_coef_1 <- ba_dbh_rel$coefficients[1]
ba_dbh_coef_2 <- ba_dbh_rel$coefficients[2]

# Rm model
rm(ba_dbh_rel)
gc()

# Linear relationship between dbh and h
dbh_h_rel <- lm(dt$h~dt$dbh)
dbh_h_coef_1 <- dbh_h_rel$coefficients[1]
dbh_h_coef_2 <- dbh_h_rel$coefficients[2]

# Rm model
rm(dbh_h_rel)
gc()

# Set init dbh when value < 0.5
dt[dbh < 0.5, dbh :=  ba_dbh_coef_1+ba_dbh_coef_2*ba]

# Set init h when value < 1.31
dt[h < 1.31, h := dbh_h_coef_1 + dbh_h_coef_2 * dbh]

# Rm coeffs
rm(ba_dbh_coef_1, ba_dbh_coef_2, dbh_h_coef_1, dbh_h_coef_2)
gc()


# Calculate total number of trees
dt[, Ntot := ba/(pi*(dbh/200)^2)]


# Basal areas
dt[, ba_pine := biomass_pine_share*ba]
dt[, ba_spruce := biomass_spruce_share*ba]
dt[, ba_bl := biomass_bl_share*ba]

# Run garbage collection
gc()


# Columns to keep
keep_cols <- c("x","y","age","fert","dbh","h","ba_pine","ba_spruce","ba_bl","groupID")

# Drop unnecessary cols
cols_dt <- dt[, ..keep_cols]

rm(dt)
gc()



# # Write cols_dt
# filename_cols_dt <- paste0("cols_dt_", tile, ".csv")
# path_cols_dt <- paste0("data/multisources/csv/2019/", tile,"/", filename_cols_dt)
# fwrite(cols_dt, file = path_cols_dt, row.names = F)


# # REMOVE ALL VARIABLES: May not be necessary!
# rm(list=ls())





# ATTACH IDS HERE

# dt_ids <- cols_dt[,c("x", "y", "groupID")]
# 
# rm(cols_dt)
# gc()

# tile <- "m4"
# filename_rdata <- "dt_ids_test.rdata"
# path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)
# fwrite(dt_ids,path_rdata)

# Write ids and process in gridID.R

############## PROCESS ##############


# Read ids
filename <- paste0("16m_100m_1km_ids_", tile, ".csv")
path <- paste0("data/multisources/csv/2019/", tile,"/", filename)

dt_16_100_1 <- fread(path)


# # Read cols_dt
# filename_cols_dt <- paste0("cols_dt_", tile, ".csv")
# path_cols_dt <- paste0("data/multisources/csv/2019/", tile,"/", filename_cols_dt)
# 
# cols_dt <- fread(path_cols_dt)



# Join ids
cols_dt <- left_join(cols_dt, dt_16_100_1, by = c('groupID'))

rm(dt_16_100_1)
gc()




# Melt
melted <- melt.data.table(cols_dt, 
                             measure.vars = c("ba_pine", "ba_spruce", "ba_bl"), value.name = "ba")

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


melted <- melted[ba!=0]

gc()



filename <- "dt_ids_all.csv"
path_csv <- paste0("data/multisources/csv/2019/", tile, "/", filename)
# fwrite(melted, file = path_csv, row.names = F)

rm(melted)
gc()

dt_ids_all <- fread(path_csv)















# sites <- melted[groupID<22]
# 
# rm(melted)
# gc()
# 
# 
# nSites <- length(unique(sites$groupID))
# nLayers <- (sites %>% count(groupID))$n
# nSpecies <- (sites %>% count(speciesID,groupID) %>% count(groupID))$n
# 
# siteInfo[,8] <- nLayers
# siteInfo[,9] <- nSpecies
# 
# maxNlayers <- max(nLayers)
# 
# 
# 
# multiInitVar <- array(0,dim=c(nSites,7,maxNlayers))
# multiInitVar[,6:7,NA]
# for(i in 1:nSites){
#   filtered <- sites %>% filter(groupID==i)
#   multiInitVar[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
#   multiInitVar[i,2,1:nLayers[i]] <- filtered$age # age by tree from NFI
#   multiInitVar[i,3,1:nLayers[i]] <- filtered$h # height from NFI data
#   multiInitVar[i,4,1:nLayers[i]] <- filtered$dbh # dbh from NFI data
#   multiInitVar[i,5,1:nLayers[i]] <- filtered$ba # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
#   multiInitVar[i,6,1:nLayers[i]] <- NA
# }
























