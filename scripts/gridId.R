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


st_within(head(ids_sf, n = 10000000), joined)

ids_sf[7,]

joined_raster <- st_join(ids_sf,joined[which(joined$id.y==1),], join=st_within)

















