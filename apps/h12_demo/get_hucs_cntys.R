# get hucs
# get county data
suppressPackageStartupMessages({
  library(dplyr);
  library(nhdplusTools);
  library(tigris);
  library(sf);
  library(glue);
  library(readr);
  library(here)
})

# function to download and store a list of huc8 and huc12s for CA
f_get_hucs <- function(){
  if(!file.exists(here("h12_spp/data/ca_hucs_h8.rds"))){
    ca <- tigris::states(progress_bar=FALSE) |>
      filter(STUSPS == "CA")
    write_rds(ca, file = here("h12_spp/data/ca_state_boundary.rds"))
  } else({
    print("loading ca boundary")
    ca <- read_rds(here("h12_spp/data/ca_state_boundary.rds"))
  })
  if(!file.exists(here("h12_spp/data/ca_hucs_h8.rds"))){
    huc8 <- nhdplusTools::get_huc(AOI = ca, type = "huc08")
    huc8 <- huc8 |>  rename(huc=huc8)
    write_rds(huc8, file = here("h12_spp/data/ca_hucs_h8.rds"), compress = "gz")
    print("HUC8 file saved!")
    huc12 <- nhdplusTools::get_huc(AOI = ca, type = "huc12")
    huc12 <- huc12 |> rename(huc=huc12)
    write_rds(huc12, file = here("h12_spp/data/ca_hucs_h12.rds"), compress = "gz")
    print("HUC12 file saved!")
    return(list("ca"=ca, "huc8"=huc8, "huc12"=huc12))
  } else({
    print("File exists...loading hucs")
    huc8 <- read_rds(here("h12_spp/data/ca_hucs_h8.rds"))
    huc12 <- read_rds(here("h12_spp/data/ca_hucs_h12.rds"))
    return(list("ca"=ca, "huc8"=huc8, "huc12"=huc12))})
}
hucs <- f_get_hucs()

# function
f_get_counties <- function(){
  if(!file.exists(here("h12_spp/data/ca_cnty_fips.rds"))){
    fips <- tigris::counties(state = "CA") |>
      janitor::clean_names() |>
      select(name, statefp:countyns, geoid)
    write_rds(fips, file = here("h12_spp/data/ca_cnty_fips.rds"))
    print("File saved")
    return(fips)
  } else({
    print("File exists...loading fips")
    fips <- read_rds(here("h12_spp/data/ca_cnty_fips.rds"))
    return(fips)})
}
ca_cnty <- f_get_counties()
