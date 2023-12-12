source('scripts/settings.R')
source('./r/parallelProcess.R')
source('./r/clusters.R')




# Load sorted data
tile <- "m4"
filename <- "dt_ids_all.csv"
path <- paste0("data/multisources/csv/2019/", tile, "/", filename)

dt <- fread(path)

gc()

ids_1km <- unique(dt$id_1km)
sampled <- sample(ids_1km,1)

dt_1 <- dt[id_1km==sampled]
dt_1

length(unique(dt$id_1km))



dt_1[, c("x","y","age","fert","ba") := NULL]

# Clustering group becomes groupID (id_100m in this case)
setnames(dt_1, c("groupID", "id_100m"), c("id_16m", "groupID"))

groups_vector <- unique(dt_1$groupID)

cores <- parallelly::availableCores()

# # Get dt chunks by id_1km
# dts <- get_chunked_dfs(dt_1, 16, dt_1$id_1km)

# Get dt chunks by speciesID
dts <- split(dt_1, by=("speciesID"))

# Sources, libraries and arguments for parallel processing
sources <- c('./r/clusters.R')
libs <- c('data.table', 'factoextra')
fun_kwargs <- list(by=c("groupID","speciesID"))

gc()

# Parallel process
dts_c <- get_in_parallel(dts, get_clusters_dt, cores = 3, libs = libs, sources = sources, fun_kwargs = fun_kwargs)


# Bind list
dt_1 <- rbindlist(dts_c)

# Join clusterID column
dt_1_base <- dt[id_1km==sampled]
setnames(dt_1, c("id_16m", "groupID"),c("groupID", "id_100m"))
dt_joined <- left_join(dt_1_base, dt_1[,c(3,6,7)], by=c("groupID","speciesID"))

rm(dt_1_base,dt_1,dts,dts_c)
gc()


# Calculate dbh_cluster and h_cluster
dt_joined[, dbh_cluster := mean(dbh), by=list(clusterID,speciesID)]
dt_joined[, h_cluster := mean(h), by=list(clusterID,speciesID)]








