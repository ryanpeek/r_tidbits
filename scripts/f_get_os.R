get_ospath <- function() {
  if(.Platform$OS.type=="windows"){
    (input_path <- "C://Users/RPeek/") }
  else if(.Platform$OS.type=="unix"){
    (input_path <- osx_path) }
}
