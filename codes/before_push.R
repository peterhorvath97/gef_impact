zip <- function(folder, run){
  
  if(run == T){
# Function to get file names and sizes
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

large_files <- get_file_info(folder) %>% 
  filter(size >25*1024*1024)

create_subfolders_for_large_files <- function(folder_path, large_files) {
  # Loop through each large file
  for (i in 1:nrow(large_files)) {
    # Get the file name (without extension)
    file_name <- tools::file_path_sans_ext(large_files$name[i])
    
    # Define the new subfolder path (within the original folder)
    new_folder_path <- file.path(folder_path, file_name)
    
    # Check if the subfolder already exists; if not, create it
    if (!dir.exists(new_folder_path)) {
      dir.create(new_folder_path)
      message(paste("Created folder:", new_folder_path))
    } else {
      message(paste("Folder already exists:", new_folder_path))
    }
  }
}


create_subfolders_for_large_files(folder, large_files)



slice_and_save <- function(folder_path, file_name) { 
  data <- read_rds(file.path(folder_path, file_name))
  #size <- object.size(data)
  size <- large_files %>% 
    filter(name == file_name) %>% 
    pull(size)
  nchunk <- round(as.numeric(size)/(25*1024*1024))+5
  if(size > 250*1024*1024){
    nchunk <- nchunk + 25
  }
  #nchunk <- 20
  
  slice_start <- seq(from = 1,
                     to = nrow(data),
                     by = round(nrow(data)/nchunk))
  
  slice_end <- pmin(slice_start+round(nrow(data)/nchunk), nrow(data))
  
  slice_start <- slice_start+1
  slice_start[1] <- 1
  
  slice_start
  slice_end
  
  out <- NULL
  for(i in 1:length(slice_start)){
    out[[i]] <- data %>% 
      slice(slice_start[i]:slice_end[i])
  }
  
  for(i in 1:length(out)){
    saveRDS(out[[i]],
            paste(file.path(file.path(folder_path, str_remove_all(file_name, '.rds')), 
                            str_remove_all(file_name, '.rds')),
                  i,
                  '.rds',
                  sep = ''))
  }
  
}

for(j in 1:nrow(large_files)){
  slice_and_save(folder, large_files$name[j])
}

# Function to delete the original large files
delete_large_files <- function(folder_path, large_files) {
  for (i in 1:nrow(large_files)) {
    # Get the full path of the large file
    file_name <- large_files$name[i]
    file_full_path <- file.path(folder_path, file_name)
    
    # Check if the file exists before attempting to delete
    if (file.exists(file_full_path)) {
      # Delete the file
      file.remove(file_full_path)
      message(paste("Deleted original file:", file_name))
    } else {
      message(paste("File not found:", file_name))
    }
  }
}


delete_large_files(folder, large_files)
  }
  else{}

}
