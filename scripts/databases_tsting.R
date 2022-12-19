# dealing with databases in R


# Library -----------------------------------------------------------------

library(dbplyr)
library(DBI)
library(RMariaDB)
library(odbc)
#library(RPostgreSQL)
#library(RMySQL)
#library(odbc)
#library(RODBC)
#library(RSQLite)

library(fs)
library(glue)

# Connect -----------------------------------------------------------------

# connect in memory
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

# connect on server
# con <- DBI::dbConnect(RMariaDB::MariaDB(),
#                       host = "database.rstudio.com",
#                       user = "hadley",
#                       password = rstudioapi::askForPassword("Database password")

# connect to mdb
mdb <- "U:/LANDS/Biological Resources Db/BRI.mdb"
fs::dir_exists(mdb)
file_exists(mdb)


# Connect to MySQL --------------------------------------------------------

# setup the rs-dbi config [rs-dbi]
# database="test"
# user="root"
# password=""

con <- DBI::dbConnect(RMySQL::MySQL(),
                      host="path_to_db",
                      user="itsme",
                      password="pw")

query
