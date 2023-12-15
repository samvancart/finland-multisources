source('scripts/settings.R')
source('./r/parallelProcess.R')
source('./r/clusters.R')




# Load sorted data
tile <- "m4"
filename <- "dt_ids_all.csv"
path <- paste0("data/multisources/csv/2019/", tile, "/", filename)

dt <- fread(path)

# Remove NAs. Number of NAs would already be 0 had correct table been joined earlier.
dt <- dt[complete.cases(dt),]

gc()

# dt[, n_pixels := length(groupID), by=.(id_1km, id_100m, speciesID)]

ids_1km <- unique(dt$id_1km)
sampled <- sample(ids_1km,1)

dt_1 <- dt[id_1km %in% sampled]

dt_1 <- dt[id_1km %in% 10978]
         

dt_1[, c("x","y","age","fert","ba") := NULL]

# Clustering group becomes groupID (id_100m in this case)
setnames(dt_1, c("groupID", "id_100m"), c("id_16m", "groupID"))

groups_vector <- unique(dt_1$groupID)

cores <- parallelly::availableCores()

# # Get dt chunks by id_1km
# dts <- get_chunked_dfs(dt_1, 20, dt_1$id_1km)
# system.time(
#   dts <- get_chunked_dfs(dt, 16, dt$id_100m)
# )

# Get dt chunks by speciesID
dts <- split(dt_1, by=("speciesID"))

# Get dt chunks by 100m ID
# dts <- split(dt_1, by=("groupID"))


# Keyword arguments for parallel processing

# Omit speciesID if possible
by <- c("groupID")

# Max number of clusters for a species
max_k = 5

# Sources, libraries and arguments for parallel processing
sources <- c('./r/clusters.R')
libs <- c('data.table', 'factoextra')
fun_kwargs <- list(by = by, max_k = max_k)

gc()

# Parallel process
dts_c <- get_in_parallel(dts, get_clusters_dt, cores = cores, libs = libs, sources = sources, fun_kwargs = fun_kwargs)


# Bind list
dt_1 <- rbindlist(dts_c)


# Join clusterID column
dt_1_base <- dt[id_1km %in% sampled]
setnames(dt_1, c("id_16m", "groupID"),c("groupID", "id_100m"))
dt_joined <- as.data.table(left_join(dt_1_base, dt_1[,c(3,6,7)], by=c("groupID","speciesID")))

rm(dt_1_base,dt_1,dts,dts_c)
gc()

unique(dt_joined$id_100m)
round(median(dt_joined[id_100m==723532]$fert))
sort(unique(dt_joined[speciesID==3 & id_100m==723532]$clusterID))

(16*16)/10000
# ba for pixel
dt_joined[, ba_actual := ba*0.0256]

# n per ha
dt_joined[, N := ba_actual/(pi*(dbh/200)^2)]

dt_joined[, n_pixel_forest := length(unique(groupID)), by=.(id_1km,id_100m)]

maxNpixel <- max(dt_joined$n_pixel_forest)

49*(16*16)

dt_joined[n_pixel_forest==49]

40*(16*16)

# CHECK IN QGIS 49 pixels??
# 100m 724499   1km 7266

range(dt_joined$p_cover)

dt_joined[p_cover>1]

39*(16*16)

(100/16)^2

# Calculate dbh_cluster and h_cluster
dt_joined[, dbh_cluster := mean(dbh), by=list(clusterID,speciesID)]
dt_joined[, h_cluster := mean(h), by=list(clusterID,speciesID)]




