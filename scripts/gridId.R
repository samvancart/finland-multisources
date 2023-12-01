source("scripts/settings.R")
source('./r/utils.R')
source('./r/parallelProcess.R')


sf_path <- paste0("data/qgis/finland_shapefile/")

sf1_file <- paste0(sf_path, "finland_1km_grid_m4.shp")
sf100_file <- paste0(sf_path, "finland_100m_grid_m4.shp")

sf1 <- st_read(sf1_file)
sf100 <- st_read(sf100_file)

gc()

sf100 <- st_centroid(sf100)
sf1 <- st_centroid(sf1)


# 100m grid cells within 1km grid cell
joined <- st_join(sf100,sf1, join=st_nearest_feature)


rm(sf1,sf100)
gc()

# Ids and coords file
filename_rdata <- "ids_m4.rdata"
path_rdata <- paste0("data/rdata/2019/", filename_rdata)

# Load rdata
load(path_rdata)

gc()

# Filter joined
joined <- joined[,c("id.x","id.y")]

gc()


coords <- as.data.table(st_coordinates(joined))
joined <- as.data.table(joined)
joined$x <- coords$X
joined$y <- coords$Y
joined <- joined[,c(1,2,4,5)]



ids_head <- head(ids_m4, n=1000)

ids_x <- ids_head$x
ids_y <- ids_head$y

j_x <- joined$x
j_y <- joined$y

# Transform to lat lons first
idxs <- ids_head[, geoDistance := distHaversine(
  matrix(c(ids_x, ids_y), ncol = 2), 
  matrix(c(j_x, j_y), ncol = 2))]




# ids_sf <- head(ids_sf, n=4500000)

# ids_sf <- as.data.frame(ids_sf)

# Get available cores
cores <- parallelly::availableCores()


# Define libraries, sources and arguments needed for parallel procesing
libs <- c("sf")
sources <- c("./r/utils.R")
fun_kwargs <- list(joined)

# Split 10by10 sf for parallel processing
ids_df_list <- do.call(split_df_equal, list(df = as.data.frame(ids_sf), n = cores))

rm(ids_sf)
gc()

ids_sf_list <- lapply(ids_sf, st_as_sf)

rm(ids_df_list)
gc()

# Get id idxs with nearest neighbour function
# idxs <- st_nearest_feature(ids_sf, joined)
idxs <- unlist(get_in_parallel(ids_sf_list, fun = st_nearest_feature, cores = 8,
                                  libs = libs, sources = sources, fun_kwargs = fun_kwargs))


gc()

rm(joined)
gc()

# Unlist
ids_sf_list <- bind_rows(ids_sf_list)

# groupIDs to data table
ids_sf_list <- as.data.table(ids_sf[,1])

# Add 100m id column
ids_sf_list$id_100m <- idxs

rm(idxs)
gc()

# Read ids lookup table
path <- "data/multisources/csv/100m_1km_ids_m4.csv"
ids_100_1 <- fread(path)


# Left inner join data tables
# ids_16_100_1 <- ids_sf[ids_100_1, on=c("id_100m"), nomatch = 0]
ids_16_100_1 <- left_join(ids_sf_list, ids_100_1, by = c("id_100m"))

gc()



ids_16_100_1[6000]
ids_100_1[id_100m==1296961]
ids_sf_list[6000]























