####################################################################################################

msp_dir    <- paste0(normalizePath("~"),"/mspa_results/")
tmp_dir    <- paste0(normalizePath("~"),"/tmp/MSPA/")

dir.create(msp_dir,recursive=T,showWarnings = F)
dir.create(tmp_dir,recursive=T,showWarnings = F)

dir.create(paste0(tmp_dir,"input/"), showWarnings = F)
dir.create(paste0(tmp_dir,"output/"),showWarnings = F)

####################################################################################################
################# PIXEL COUNT FUNCTION
pixel_count <- function(x){
  info    <- gdalinfo(x,hist=T)
  buckets <- unlist(str_split(info[grep("bucket",info)+1]," "))
  buckets <- as.numeric(buckets[!(buckets == "")])
  hist    <- data.frame(cbind(0:(length(buckets)-1),buckets))
  hist    <- hist[hist[,2]>0,]
}


