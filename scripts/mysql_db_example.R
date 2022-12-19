# mysql_db_exmaple

library(dplyr)
library(dbplyr)
library(DBI)
library(RMariaDB)

# basic example of genomics data in MySQL db
myConn = dbConnect(RMariaDB::MariaDB(),
                   db = "Rfam",
                   host = "mysql-rfam-public.ebi.ac.uk",
                   user = "rfamro",
                   port = 4497)

# connect & query
tbl(myConn, "taxonomy") %>%
  filter(ncbi_id == 10116)

# count
tbl(myConn, "taxonomy") %>% select(ncbi_id) %>% distinct() %>% tally()

# Alternate ---------------------------------------------------------------
# from here: https://tavareshugo.github.io/data_carpentry_extras/dbplyr_ensembl/dbplyr_ensembl.html

library(tidyverse)
library(dbplyr)
library(RMariaDB)

ensembl_con <- dbConnect(MariaDB(),
                         host = "ensembldb.ensembl.org",
                         user = "anonymous",
                         port = 5306,
                         password = "")

dbGetQuery(ensembl_con, "SHOW DATABASES") # many!

dbGetQuery(ensembl_con, "SHOW DATABASES") %>%
  # filter the table for cases where the the string "compara" is present
  filter(str_detect(Database, "compara")) %>%
  tail()

# pick one of most recent:
compara_con <- dbConnect(MariaDB(),
                         dbname = "ensembl_compara_95",
                         host = "ensembldb.ensembl.org",
                         user = "anonymous",
                         port = 5306,
                         password = "")

# List table names in the database
src_dbi(compara_con)

compara_homology <- tbl(compara_con, "homology")
head(compara_homology)

# pull specifics
compara_linksp <- tbl(compara_con, "method_link_species_set")
head(compara_linksp) # this is "lazy" so not actually in memory unless "collect()"

# compare two either or
sapiens_ortho <- compara_linksp %>%
  filter(name == "H.sap-P.tro orthologues" | name == "H.sap-M.mus orthologues")

# join
compara_result <- sapiens_ortho %>%
  left_join(compara_homology, by = "method_link_species_set_id") %>%
  collect() # now actually pulling into R memory...takes a min

# view results
head(compara_result)
compara_result %>%
  count(name, description)

# viz
compara_result %>%
  ggplot(aes(name, ds/dn, fill = description)) +
  geom_boxplot() + scale_y_log10()
