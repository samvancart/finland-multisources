source("scripts/settings.R")

tile <- "m4"
filename_rdata <- "dt.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)

# Load rdata
dt <- fread(path_rdata)



# CLEAN THIS UP

# Rename columns
colnames(dt) <- c("x","y","biomass_spruce","biomass_bl","biomass_pine","age","fert","dbh","h","ba")

# siteXs <- sample(1:nrow(dt), 50000)
# dt[siteXs,plot(h,dbh,pch=".")]

# Height from dm to m
dt[, h := h/10]

dt$h <- as.double(dt$h)
dt$dbh <- as.double(dt$dbh)
dt$ba <- as.double(dt$ba)

# Init values
dt[h<13.1,h:=initSeedling.def[1]]
dt[dbh<0.5,dbh:=initSeedling.def[2]]
dt[ba==0,ba := initSeedling.def[3]]

dt[, Ntot := ba/(pi*(dbh/200)^2)]

dt <- dt[complete.cases(dt),]
dt <- dt[fert!=32766]
dt <- dt[fert!=32767]


hist(dt$Ntot, breaks=seq(0,210000,100),xlim=c(0,5000))
dt[Ntot>200000]

dt[siteXs,plot(ba,h,pch=".")]
dt[siteXs,plot(ba,dbh,pch=".")]


# Linear model
ba_dbh_rel <- lm(dt$ba~dt$dbh)
dbh_h_rel <- lm(dt$h~dt$dbh)

dbh_4 <- ba_dbh_rel$coefficients[1] + ba_dbh_rel$coefficients[2] * 4
h_4 <- dbh_h_rel$coefficients[1] + ba_dbh_rel$coefficients[2] * dbh_4

Ntot_4 <- 4/(pi*(dbh_4/200)^2)





# # Rename columns
# colnames(dt) <- c("x","y","biomass_spruce","biomass_bl","biomass_pine","age","fert","dbh","h","ba")

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



# DT PROCESSED



tile <- "m4"
filename_rdata <- "dt_processed.rdata"
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


# Remove cloud cover and other no value pixels
cols_dt <- cols_dt[fert!=32766]
cols_dt <- cols_dt[fert!=32767]

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


# # Height from dm to m
# melted[, h := h/10]

gc()



# FILTER CLEARCUTS HERE

# Rows where height <= 1.5
# SHOULD THIS BE <= OR < ??
h_idxs <- which(melted$h <= 1.5)

# Height to 1.5
melted[h_idxs, h := 1.5]

# Dbh to 1
melted[h_idxs, dbh := 1]

# Set ba initial state
melted[h_idxs, ba := fifelse(speciesID > 2, 0.00863938, 0.01727876)]

init_ba <- 0.01727876*2+0.00863938

initSeedling.def



# Rows to remove because height > 1.5 & ba == 0
h_ba_idxs <- which(melted$h > 1.5 & melted$ba==0)

# Drop rows
melted <- melted[!h_ba_idxs]


# WHAT TO DO HERE??
melted[dbh==0 | ba == 0]
melted[h > 1.5 & ba == 0]

range(melted[dbh==0]$h)

gc()

# Get rows with < 3 species
melted[, rows_removed := length(.SD$speciesID)<3, by=c("groupID")]
rows_removed <- melted[rows_removed==T]
rows_not_removed <- melted[rows_removed==F]
length(unique(rows_removed$id_100m)) + length(unique(rows_not_removed$id_100m))

length(unique(melted$id_100m))

filename <- "dt_ids_all.csv"
path_csv <- paste0("data/multisources/csv/2019/", tile, "/", filename)
# fwrite(melted, file = path_csv, row.names = F)


dt_ids_all <- fread(path_csv)






# filename_rdata <- "dt_ids.rdata"
# path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)
# # fwrite(dt_ids,file=path_rdata, row.names = F)








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
























