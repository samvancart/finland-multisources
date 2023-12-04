source("scripts/settings.R")
source('./r/utils.R')
source('./r/parallelProcess.R')


sf_path <- paste0("data/qgis/finland_shapefile/")

sf1_file <- paste0(sf_path, "finland_1km_grid_m4.shp")
sf100_file <- paste0(sf_path, "finland_100m_grid_m4.shp")

sf1 <- st_read(sf1_file)
sf100 <- st_read(sf100_file)

gc()

# 100m and 1km centroids
sf100 <- st_centroid(sf100)
sf1 <- st_centroid(sf1)


# For each 100m point find nearest 1km point and join tables
joined <- st_join(sf100, sf1, join=st_nearest_feature)

rm(sf1,sf100)
gc()

# Raster ids and coords file
tile <- "m4"
filename_rdata <- "dt_ids.rdata"
path_rdata <- paste0("data/rdata/2019/", tile, "/", filename_rdata)


# Read rdata
dt_ids <- fread(path_rdata)

gc()

# Filter joined
joined <- joined[,c("id.x","id.y")]

gc()


# Get available cores
cores <- parallelly::availableCores()

gc()

# ids_head <- head(ids_m4, n=nrow(ids_m4))

# Rows to include for each splitID
splitID_len <- ceiling(nrow(dt_ids)/cores)

# SplitIDs as vector
splitIDs <- head(rep(c(1:cores), each=splitID_len), n=nrow(dt_ids))

# Assign splitIDs to table
dt_ids[, splitID := splitIDs]

rm(splitID_len, splitIDs)
gc()

# Fast split data table
ids_dt_list <- split(dt_ids, by='splitID')

rm(dt_ids)
gc()

# Drop splitID col from each data table chunk
ids_dt_list <- lapply(ids_dt_list, function(x) x[, ('splitID') := NULL])

# Cast each chunk to sf
ids_sf_list <- lapply(ids_dt_list, st_as_sf, coords=c('x','y'), crs = st_crs(joined))

rm(ids_dt_list)
gc()

# Define libraries, sources and arguments needed for parallel processing
libs <- c("sf")
sources <- c("./r/utils.R")
fun_kwargs <- list(joined)

rm(joined)
gc()

# Get id indexes with nearest neighbour function
# 8 cores seems to be faster even when 16 are available?!
idxs <- unlist(get_in_parallel(ids_sf_list, fun = st_nearest_feature, cores = 8,
                                  libs = libs, sources = sources, fun_kwargs = fun_kwargs))


rm(fun_kwargs, ids_sf_list)
gc()

# Load rdata again
dt_ids <- fread(path_rdata)

# Add 100m id column
dt_ids[, id_100m := idxs]

rm(idxs)
gc()

# Read ids lookup table
path <- "data/multisources/csv/100m_1km_ids_m4.csv"
ids_100_1 <- fread(path)


# Left inner join data tables
ids_16_100_1 <- left_join(dt_ids, ids_100_1, by = c("id_100m"))

gc()

# Test
ids_16_100_1[6000]
ids_100_1[id_100m==1296961]
dt_ids[6000]

rm(dt_ids, ids_100_1)
gc()

ids_16_100_1[id_1km==1]


# Write csv
path <- "data/multisources/csv/16m_100m_1km_ids_m4.csv"

# # Only include ids
# fwrite(ids_16_100_1[,3:5], path, row.names = F)


















