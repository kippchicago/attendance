# Let's create a get_load function that checks the meta data on a bucket's object


gcs_check_md5 <- function(file_name){
    local_md5hash <- ""
    meta_file <- sprintf("%s.meta",file_name)
    if(file.exists(meta_file)){
      local_md5hash <- jsonlite::fromJSON(meta_file)$md5Hash
    }
    meta_data <- gcs_get_object(file_name, saveToDisk = meta_file, meta = TRUE, overwrite = TRUE)
    gcs_md5hash  <- meta_data$md5Hash
    
    gcs_md5hash == local_md5hash
}

gcs_get_feather <- function(file_name){
  
  message(sprintf("Checking MD5 Hash for %s", file_name))
  if(!gcs_check_md5(file_name)) {
  message(sprintf("MD5 Hashes didn't match. Getting %s from GCS", file_name))
  gcs_get_object(object_name = file_name, 
                 saveToDisk = file_name,
                 overwrite = TRUE)
  }
  
  message(sprintf("Reading %s from disk", file_name))
  read_feather(file_name)
}


             
gcs_reactive_poll <- function(file_name) {
  #confgcs_check_md5(file_name) 
  gcs_get_feather(file_name)
}