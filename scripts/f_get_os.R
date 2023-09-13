
# use this to check the operating system
f_get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

f_get_ospath <- function() {
  if(.Platform$OS.type=="windows"){
    (input_path <- "C:/Users/RPeek/") }
  else if(.Platform$OS.type=="unix"){
    (input_path <- "/Users/rapeek/") }
  return(input_path)
}
