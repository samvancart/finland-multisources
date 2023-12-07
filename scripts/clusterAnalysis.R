source('scripts/settings.R')
source('./r/parallelProcess.R')
source('./r/clusters.R')


# Load sorted data
tile <- "m4"
filename <- "dt_ids_all.csv"
path <- paste0("data/multisources/csv/2019/", tile, "/", filename)
dt <- fread(path)

gc()

# dt[, c("x","y","age","fert","ba") := NULL]

dt_1 <- dt[id_1km<=1]


dt_1[, c("x","y","age","fert","ba","id_1km") := NULL]
# Clustering group becomes groupID (id_100m in this case)
setnames(dt_1, c("groupID", "id_100m"), c("id_16m", "groupID"))


groups_vector <- unique(dt_1$groupID)
cluster_cols <- c("dbh","h")


which(dt_1$groupID==21162)
dt_1[473995,]

# Test chunks function
chunked <- get_chunked_dfs(dt,16,dt$id_1km)
dt1 <- chunked[[1]]

length(unique(dt1$id_1km))

# Test clustering with data table
# Faster by using colnames as keys directly instead of in a variable as in .SD[, ..cluster_cols]
# TRY IN PARALLEL
system.time(
  dt_1[, clusterID := kmeans(cbind(dbh,h), centers=get_centres(cbind(dbh,h), get_kmax(cbind(dbh,h))))$cluster, by=c("groupID","speciesID")]
)




dt_c[groupID==1 & speciesID==1]
dt_1[groupID==1 & speciesID==1]

system.time(
  clusterIDs_list <- get_clusterIDs_groups_species(dt_1, groups_vector, cluster_cols)
)



setequal(dt_1$clusterID,clusterIDs_list)

dt_1$clusterID <- clusterIDs_list

dt_1[groupID==1 & speciesID==3]

gc()





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





