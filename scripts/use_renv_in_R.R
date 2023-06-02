# use Renv

# initialize a new project in R: --------------

library(usethis)

# setup a project on github first and then clone?
#usethis::create_project(path = "~/Documents/github/tst_usethis")
#usethis::use_git()

# if need to reset origin:
#usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)
#usethis::use_github()

# setup init renv and create snapshot ------------

renv::init()
renv::snapshot()
