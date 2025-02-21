unzip <- function(folder, run){
  
  if(run == T){
  
  dirs <- list.dirs(path = folder, full.names = TRUE, recursive = FALSE)
  
  for(j in 1:length(dirs)){
    
    files <- list.files(dirs[j])
    files <- file.path(dirs[j], files)
    
    out <- NULL
    for(i in 1:length(files)){
      out[[i]] <- read_rds(files[i])
    }
    
    out <- bind_rows(out)
    
    
    saveRDS(out,
            paste(dirs[j], '.rds', sep =''))
    
  }
  remove_subfolders <- function(folder_path) {
    # Get the list of subfolders
    subfolders <- list.dirs(path = folder, full.names = TRUE, recursive = FALSE)
    
    # Loop through each subfolder and delete it
    for (subfolder in subfolders) {
      # Check if the subfolder exists
      if (dir.exists(subfolder)) {
        # Recursively delete the subfolder and all its contents
        unlink(subfolder, recursive = TRUE)
        message(paste("Deleted subfolder and its contents:", subfolder))
      } else {
        message(paste("Subfolder not found:", subfolder))
      }
    }
  }
  
  remove_subfolders(folder)
  }
  

  
  else{}
}