source("scripts/settings.R")

path <- paste0("data")

# links <- c("IbbK1IB3F49ZHNsHOi3","pwwYg8NuWjBSpp25aeb","5iCvso3y70aaCbpxfEr","6UKV7j09JzLcLt2dXgb",
#            "PoVCohSAHB4SghbB8Ob","rQIwAXG6zlQ0aPZptgP","nfp2PyXSA5vjfy6iv5J", "cVNbCOzoZ8E7ZBfsQMf")
# 
# for(i in 1:length(links)) {
#   url <- paste0("http://kartta.luke.fi/lataus/download/", links[i], ".zip")
#   filename <- paste0(path, "/file_", i, ".zip")
#   download.file(url, filename)
# }

unzipped <- paste0(path, "/multisources/unzipped/2019/unzipped")
files <- list.files(unzipped)

# for(i in 1:length(files)) {
#   z_file <- paste0(unzipped,"/", files[i])
#   out <- paste0(unzipped, "/unzipped")
#   unzip(z_file, exdir=out)
# }


dt_m4 <- as.data.table(read_stars(paste0(unzipped, "/", files[1])))
for(i in 2:length(tif_files)) {
  dt_temp <- as.data.table(read_stars(paste0(unzipped, "/", files[i])))
  dt_m4 <- left_join(dt_m4, dt_temp, by = c("x", "y"))
}

filename_rdata <- "dt_m4.rdata"
path_rdata <- paste0("data/rdata/2019/", filename_rdata)

# save(dt_m4, file = path_rdata)




file_ids_csv_m4 <- "data/multisources/csv/16m_100m_1km_ids_m4.csv"
ids <- fread(file_ids_csv_m4)

ids[id_100m==41296]

id_1km_1<-sort(ids[id_100m==1924])

length(id_1km_1)




# TEST 16 BY 16 IDS

# Read ids
filename <- paste0("16m_100m_1km_ids_", tile, ".csv")
path <- paste0("data/multisources/csv/2019/", tile,"/", filename)

dt_16_100_1 <- fread(path)

test_filename <- paste0("1km_test_7266.csv")
test_path <- paste0("data/multisources/csv/test/", test_filename)


test_ids <- dt_16_100_1[id_1km==7266]$groupID

dt_ids <- cols_dt[,c("x", "y", "groupID")]

test_ids_dt <- dt_ids[groupID %in% test_ids]

unique(dt_16_100_1[id_100m %in% idxs]$id_1km)


test_sf1_7266 <- st_as_sf(rbindlist(ids_sf_list))


test_sf_filename <- paste0("1km_test_7266.shp")
test_sf_path <- paste0("data/qgis/test/", test_sf_filename)

st_write(test_sf1_7266,test_sf_path)

dt1 <- dt_16_100_1[id_1km==7266]

length(dt1[id_100m==724498]$groupID)


ids_100m <- c(723538,724498,725458)

length(dt1[id_100m %in% ids_100m]$groupID)


lapply(ids_100m, function(x) length(dt1[id_100m == x]$groupID))



# SPLIT DT
get_dts <- function(dt,cores){
  u_ids <- unique(dt$id_100m)
  
  # Rows to include for each splitID
  splitID_len <- ceiling(length(u_ids)/cores)
  
  # SplitIDs as vector
  splitIDs <- head(rep(c(1:cores), each=splitID_len), n=length(u_ids))
  
  dt_split_ids <- data.table("id_100m"=u_ids, "splitID"=splitIDs)
  
  splitID_dts <- split(dt_split_ids, by="splitID")
  
  dts <- c()
  for(i in splitID_dts){
    dts <- append(dts, list(dt[id_100m %in% i$id_100m]))
  }
  return(dts)
} 



get_dts2 <- function(dt, cores) {
  u_ids <- unique(dt$id_100m)
  
  # Rows to include for each splitID
  splitID_len <- ceiling(length(u_ids)/cores)
  
  # SplitIDs as vector
  splitIDs <- head(rep(c(1:cores), each=splitID_len), n=length(u_ids))
  
  dt_split_ids <- data.table("id_100m"=u_ids, "splitID"=splitIDs)
  
  dts <- split(dt[dt_split_ids, on=c("id_100m")], by= c("splitID"))
  
  return(dts)
}





system.time(
  dts <- get_dts(dt,cores)
)

rm(dts)
gc()

system.time(
  dts <- get_dts(dt,cores)
)









           
