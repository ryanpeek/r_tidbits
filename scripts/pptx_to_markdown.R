# using something to convert pptx to markdown?
# lets see


# Libraries ---------------------------------------------------------------

# main install
#devtools::install_github("datalorax/slidex")
library(slidex)

# supporting packages
library(xaringan)
library(knitr)
library(kableExtra)
library(tibble)
library(fs)

# Implement ---------------------------------------------------------------

ppt_dir <- "/Users/rapeek/Library/CloudStorage/Dropbox/DOCS/Archive/2023/presentations/elkhorn_rabo_workshop"

pptx <- fs::dir_ls(ppt_dir, recurse = TRUE, glob = "*.pptx")
pptx[13]

## Make dir for slides
fs::dir_create("slides")

convert_pptx(path = pptx[13], author = "R. Peek", out_dir = "slides")

# works!


## Test 2 -----------------
pptx[1]
convert_pptx(path = pptx[1],title = "unnatural_history", author = "R. Peek", out_dir = "slides")
