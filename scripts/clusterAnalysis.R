source('scripts/settings.R')
source('./r/parallelProcess.R')
source('./r/clusters.R')




# Load sorted data
tile <- "m4"
filename <- "dt_ids_all.csv"
path <- paste0("data/multisources/csv/2019/", tile, "/", filename)
dt <- fread(path)

gc()

dt_1 <- dt[id_1km<=2000 & speciesID==1]

length(unique(dt$id_1km))



dt_1[, c("x","y","age","fert","ba") := NULL]

# Clustering group becomes groupID (id_100m in this case)
setnames(dt_1, c("groupID", "id_100m"), c("id_16m", "groupID"))

groups_vector <- unique(dt_1$groupID)

cores <- parallelly::availableCores()

# Get dt chunks
dts <- get_chunked_dfs(dt_1, 16, dt_1$id_1km)


# Sources and libraries for parallel processing
sources <- c('./r/clusters.R')
libs <- c('data.table', 'factoextra')

gc()

# Parallel process
dts_c <- get_in_parallel(dts, get_clusters_dt, cores = cores, libs = libs, sources = sources)


# Bind list
dt_1 <- rbindlist(dts_c)


# Checks
((nrow(dt[speciesID==1])/nrow(dt_1)) * (683/60))/60

dt_1[944,]
sort(unique(dt_1[id_1km==865 & groupID==92161]$clusterID))
unique(dt_1$id_1km)
length(unique(dt_1$id_1km))
dt_1$clusterID


# Test clustering with data table
# Faster by using colnames as keys directly instead of in a variable as in .SD[, ..cluster_cols]
# TRY IN PARALLEL
system.time(
  dt_1[, clusterID := kmeans(cbind(dbh,h), centers=get_centres(cbind(dbh,h), get_kmax(cbind(dbh,h))))$cluster, by=c("groupID","speciesID")]
)



dt_1_base <- dt[id_1km<=10]
setnames(dt_1, c("id_16m", "groupID"),c("groupID", "id_100m"))
dt_joined <- left_join(dt_1_base, dt_1[,c(3,7)], by=c("groupID"))





# TEST THAT ALL SPECIES ARE IN RESULTS
allSpeciesExist <- c()
for(i in groups_vector) {
  result <- setequal(unique(dt_joined[id_100m==i]$speciesID), c(1,2,3))
  allSpeciesExist <- append(allSpeciesExist, result)
}

# SHOULD BE integer(0)
which(allSpeciesExist==F)

# TEST THAT CLUSTERS ARE IDENTICAL
speciesClustersIdentical <- c()
for(i in groups_vector) {
  sp1 <- dt_joined[id_100m==i & speciesID==1]$clusterID
  sp2 <- dt_joined[id_100m==i & speciesID==2]$clusterID
  sp3 <- dt_joined[id_100m==i & speciesID==3]$clusterID
  
  r1 <- setequal(sp1,sp2)
  r2 <- setequal(sp1,sp3)
  speciesClustersIdentical <- append(speciesClustersIdentical, fifelse(r1&r2,T,F))
}
# SHOULD BE integer(0)
which(speciesClustersIdentical==F)









# Get number of available cores
cores <- detectCores(logical=T)

# Get all groupIDs
ids <- df$groupID

# Get chunked dfs
dfs <- get_chunked_dfs(df = df, max_chunk_size = cores, ids = ids)

# Number of chunks = Number of cores to use
chunks <- length(dfs)


# Get clusterIDs vector with parallel processing


# Combine clusterIDs from all runs
clusterIDs_list <- Reduce(append, clusterIDs_lists, c())

# Add clusterIDs to df
df$clusterID <- clusterIDs_list

# Sort by group then species then cluster
df_sorted <- df[with(df,order(df$groupID,df$speciesID,df$clusterID)),]





