

# Chooses optimal number of clusters (fviz_nbclust function)
# for all subgroups of a grouped data frame based on given columns in that data frame,
# generates the clusters for each subgroup, and assigns a clusterID to each subgroup member.
# Result is the data frame sorted by original groupID, original subgroupID and assigned clusterID.

# Uses parallel processing to speed up process.
# Data must first be split into chunks (based on available cores on system) to enable this.


# Functions

# Divide data frame into chunks. First divides df into equal chunks based on number of rows.
# Actual chunks are determined by parameter 'ids' values. Each unique id will belong to same chunk.
# Data frame must be sorted by 'ids' prior to running.
# Params:
# df (data.frame): The original data frame sorted by 'ids'.
# max_chunk_size (integer): The max number of chunks to split df into.
# ids (numeric or integer vector): The column of ids from original df.
# Returns:
# List: List of data frames

get_chunked_dfs <- function(df, max_chunk_size, ids) {
  if(max_chunk_size==0){stop(paste0("max_chunk_size must be > 0."))}
  if(max_chunk_size>nrow(df)){stop(paste0("max_chunk_size must be <= nrow(df)."))}
  if(class(ids)!="integer" & class(ids)!="numeric"){stop(paste0("Parameter ids must be of class numeric or integer."))}
  if(length(ids)!=nrow(df)){stop(paste0("Length of parameter ids must match nrow(df)."))}
  
  groups <- ids
  max_chunk_size <- max_chunk_size
  chunk <- floor(length(groups)/max_chunk_size)
  firstidx <- 1
  dfs <- c()
  toidx <- chunk
  
  while (toidx <= length(groups) & firstidx < length(groups)){
    toidx <- (firstidx +(chunk)-1)
    if(toidx > length(groups)) {toidx <- length(groups)}
    
    last <- tail(groups[firstidx:toidx],n=1)
    lastidx <- tail(which(groups==last),n=1)
    chunk_df <- df[(firstidx:lastidx),]
    dfs <- append(dfs,list(as.data.table(chunk_df)))
    firstidx <- lastidx+1
  }
  return(dfs)
}


# Optimal number of cluster centers
get_centres <- function(df, kmax) {
  
  centres <- 1
  
  if (nrow(unique(df))>2) {
    optimal_num_clusters <- fviz_nbclust(df, kmeans, method = "silhouette", k.max = kmax)
    centres <- which.max(optimal_num_clusters$data$y)
  }
  return(centres)
}


# Kmax for optimising number of clusters
get_kmax <- function(df) {
  
  if(nrow(unique(df))>2) {
    kmax <- nrow(unique(df))-1
  } else {
    kmax <- 2
  }
  return(kmax)
}


# Get all clusterIDs from groups and species
get_clusterIDs_groups_species <- function(df, groups_vector, cluster_cols=c("Dbh", "Height")) {
  # Import libraries here to enable parallel processing 
  library(tibble)
  library(dplyr)
  library(factoextra)
  
  # Convert df to tibble
  df <- as_tibble(df)
  
  # Initialise list for clusterIDs
  clusterIDs_list <- c()
  
    for (i in groups_vector) {
      # Choose 1 site
      filtered_groups <- df %>% filter(groupID == i)
      
      # Species vector
      species_vector <- unique(filtered_groups$speciesID)
      
      for (j in species_vector) {
        # Choose species
        filtered_species <- filtered_groups %>% filter(speciesID == j)
        
        # Df for clustering
        df_cluster_cols <- filtered_species[, cluster_cols]
        
        # Get max number of clusters
        kmax <- get_kmax(df_cluster_cols)
        
        # Get optimal number of clusters
        centres <- get_centres(df_cluster_cols, kmax)
        
        # K-means set up
        set.seed(123)
        model <- kmeans(df_cluster_cols, centers = centres, nstart = 25, iter.max=50)
        
        ## Append clusterIDs to list
        clusterIDs <- model$cluster
        clusterIDs_list <- append(clusterIDs_list, clusterIDs)
      }
      
    }

  return(clusterIDs_list)
}



get_clusters_dt <- function(dt){
  dt[, clusterID := kmeans(cbind(dbh,h), centers=get_centres(cbind(dbh,h), get_kmax(cbind(dbh,h))))$cluster, by=c("groupID","speciesID")]
  return(dt)
}















