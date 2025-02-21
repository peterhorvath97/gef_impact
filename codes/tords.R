tords <- function(folder){
  
  get_file_info <- function(folder_path) {
    # Get all files in the folder
    files <- list.files(folder_path, full.names = TRUE)
    
    # Get file information (names and sizes in bytes)
    file_info <- data.frame(
      name = basename(files),
      size = file.info(files)$size
    )
    
    return(file_info)
  }
  
  notrds <-get_file_info(folder) %>% 
    filter(!str_detect(name, '.rds')) %>% 
    pull(name)
  
  if(!is_empty(notrds)){
    for(i in 1:length(notrds)){
      data <- get(load(file.path(folder, notrds[i]))) %>% 
        as_tibble()
      dot <- str_locate(notrds[i], DOT)[1, 'start']
      newname <- paste(substr(notrds[i], 1, dot-1), '.rds', sep = '')
      saveRDS(data, file.path(folder, newname))
      file.remove(file.path(folder, notrds[i]))
    }
  }
  
  
}
