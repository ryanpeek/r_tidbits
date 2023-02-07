# function to replace spaces in filenames

replace_spaces <- function(folder) {
  # Get a list of all files in the folder
  file_list <- list.files(path = folder, full.names = TRUE)

  # Loop through the list of files
  for (file in file_list) {
    # Get the current file's name and extension
    file_name <- basename(file)
    file_ext <- file_ext(file_name)

    # Replace all spaces with underscores in the file name
    new_name <- gsub(" ", "_", file_name)

    # Use file.rename() to rename the file
    file.rename(from = file, to = file.path(folder, paste0(new_name, file_ext)))
  }
}

# run with: replace_spaces("/path/to/folder")
