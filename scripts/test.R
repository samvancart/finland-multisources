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