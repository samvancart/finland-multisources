source("scripts/settings.R")

sf_path <- paste0("data/qgis/finland_shapefile/")

sf1_file <- paste0(sf_path, "finland_1km_grid_m4.shp")
sf100_file <- paste0(sf_path, "finland_100m_grid_m4.shp")

sf1 <- st_read(sf1_file)
sf100 <- st_read(sf100_file)

gc()

# 100m grid cells within 1km grid cell
joined <- st_join(sf100,sf1, join=st_within)

rm(sf1,sf100)
gc()

filename_rdata <- "ids_m4.rdata"
path_rdata <- paste0("data/rdata/2019/", filename_rdata)

# Load rdata
load(path_rdata)

gc()

ids_sf <- st_as_sf(ids_m4, coords=c("x","y"))

rm(ids_m4)
gc()


st_crs(ids_sf) <- st_crs(joined)

gc()

joined <- joined[,c("id.x","id.y")]

gc()

# ids_sf <- head(ids_sf, n=1000)

idxs <- st_nearest_feature(ids_sf, joined)

gc()

rm(joined)
gc()

ids_sf <- as.data.table(ids_sf[,1])

ids_sf$id_100m <- idxs

rm(idxs)
gc()

path <- "data/multisources/csv/100m_1km_ids_m4.csv"

ids_100_1 <- fread(path)

ids_16_100_1 <- ids_sf[ids_100_1, on = c("id_100m")]

rm(ids_16_100_1)
gc()


























