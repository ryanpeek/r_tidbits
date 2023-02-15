# function to setup showtext quickly

# on win/osx
library(showtext)
showtext_opts(dpi=300)
library(glue)
library(dplyr)


# use this to check the operating system
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

f_get_fonts <- function(fnt_head=NULL, fnt_txt=NULL, fnt_pr=4){

  # check font paths
  osys <- get_os()

  # here we add new path to local fonts if needed
  if(osys=="windows"){
    font_paths(
      new = r"(C:\Users\RPeek\AppData\Local\Microsoft\Windows\Fonts)")
  } else {
    print("not on windows")
  }
  print("Getting Fonts...")
  # now pick some fonts
  fontprs <- data.frame(
    fnt_pair = c(1:6),
    fnt_headers = c("PT Sans", "Merriweather","Lora", "Roboto", "Assistant", "Atkinson Hyperlegible"),
    fnt_text = c("Noto Sans", "Source Sans Pro", "Libre Franklin", "Roboto Condensed", "Assistant", "Atkinson Hyperlegible"))

  if(is.null(fnt_head) & is.null(fnt_txt)){
    fnt_header <<- fontprs[fnt_pr,2]
    fnt_text <<- fontprs[fnt_pr, 3]
  }
  if(!is.null(fnt_head) & !is.null(fnt_txt)){
    fnt_header <<- fnt_head
    fnt_text <<- fnt_txt
  }

  if(osys=="mac"){
    # add straight from google
    font_add_google(fnt_header)
    font_add_google(fnt_text)
  } else if(osys=="windows"){
    # ttf name
    try(ttf1 <- font_files() %>%
          filter(family %in% fnt_header) %>%
          filter(grepl("Regular", face)) %>%
          select(file))
    try(ttf2 <- font_files() %>%
          filter(family %in% fnt_text) %>%
          filter(grepl("Regular", face)) %>%
          select(file))

    # add locally
    font_add(family = fnt_header, regular = glue("{ttf1}"))
    font_add(family = fnt_text, regular = glue("{ttf2}"))
  }
  showtext_auto()

}



