# amtrack data

# see this github wrangling by jfangmeier: https://github.com/jfangmeier/table-contest-2022/blob/main/R/data-prep.qmd

# here: https://data-usdot.opendata.arcgis.com/datasets/usdot::amtrak-stations/about
# here: https://data-usdot.opendata.arcgis.com/datasets/usdot::amtrak-routes/about


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(rvest)
library(readxl)
library(tigris)
library(osmdata)
library(fuzzyjoin)


# Data --------------------------------------------------------------------

amtrak_rout_sf <- st_read("data_raw/Amtrak_Routes.geojson")
amtrak_stat_sf <- st_read("data_raw/Amtrak_Stations.geojson")

# ridership downloaded from: https://www.bts.dot.gov/browse-statistical-products-and-data/state-transportation-statistics/amtrak-ridership

station_codes <-
  amtrak_stat_sf %>%
  st_drop_geometry() %>%
  filter(StnType == "TRAIN") %>%
  mutate(
    StationNam = case_when(
      str_detect(Name, "Station") ~ paste0(StationNam, " - ", Name),
      TRUE ~ StationNam
    )
  ) %>%
  select(Code, StationNam, City, StateAbb = State, Name) %>%
  mutate(
    Station = str_remove(StationNam, paste0("\\,[:space:]{1}", StateAbb)),
    Station = str_replace(Station, " \\- ", "-"),
    Station = ifelse(Station == "Santa Clara", paste0(Station, "-", Name), Station),
    Station = ifelse(Station == "San Diego", paste0(Station, "-", Name), Station)) %>%
  tibble()
# read raw data
riders_raw <-
  read_xlsx("data_raw/amtrak_ridership_2005_2021.xlsx") %>%
  filter(!is.na(Station))


# Clean Station -----------------------------------------------------------

# clean station data and names
riders_station <-
  riders_raw %>%
  select(Station, Note) %>%
  mutate(Station_Name = Station) %>%
  separate(Station, into = c("Station", "State"), sep = "\\, ") %>%
  left_join(
    tibble(
      State = c(state.name, "District of Columbia"),
      StateAbb = c(state.abb, "DC")
    ),
    by = "State"
  ) %>%
  filter(!Station_Name %in% c(
    "Napa, California",
    "Waldo, Florida",
    "Galena, Illinois",
    "Bend, Oregon",
    "Madison, Florida",
    "Ocala, Florida",
    "Crawfordsville (Bus), Indiana",
    "Birmingham, Michigan",
    "Akron, Ohio",
    "Galveston, Texas",
    "Mobile, Alabama",
    "Dade City, Florida",
    "Pensacola, Florida",
    "Wildwood, Florida",
    "Louisville, Kentucky",
    "Scranton, Pennsylvania",
    "Atmore, Alabama",
    "Encinitas, California",
    "Lake City, Florida",
    "Duluth, Minnesota",
    "Gulfport, Mississippi",
    "Sparks, Nevada",
    "Fostoria, Ohio",
    "Hamilton, Ohio",
    "Guthrie, Oklahoma",
    "Moreno Valley, California",
    "Crestview, Florida",
    "Pascagoula, Mississippi",
    "Coos Bay, Oregon",
    "Ogden, Utah",
    "Burlington, Wisconsin",
    "Laguna Niguel, California",
    "Sorrento Valley, California",
    "Georgetown, Delaware",
    "Chipley, Florida",
    "Tallahassee, Florida",
    "Nappanee, Indiana",
    "Amherst, Massachusetts",
    "Biloxi, Mississippi",
    "Carlsbad Poinsettia, California",
    "Carlsbad Village, California",
    "Bay St. Louis, Mississippi",
    "Youngstown, Ohio",
    "New York (Grand Central), New York",
    "Fair Haven, Vermont",
    "Greenfield Village, Michigan",
    "Franconia-Springfield, Virginia",
    "Meridian, Michigan",
    "Mt. Pleasant, Iowa")
  ) %>%
  mutate(
    Station = str_replace(Station, "\\/", "-"),
    Station = str_remove(Station, "\\*"),
    Station = case_when(
      Station == "Little Rock" ~ "Little Rock-Union Station",
      Station == "Great America (Santa Clara)" ~ "Santa Clara-Great America",
      Station == "Los Angeles" ~ "Los Angeles-Union Station",
      Station == "Oakland Coliseum" ~ "Oakland-Coliseum/Airport Station",
      Station == "Oakland" ~ "Oakland-Jack London Square Station",
      Station == "Palm Springs" ~ "Palm Springs-Amtrak Station",
      Station == "Redding" ~ "Redding-Amtrak Station",
      Station == "Sacramento" ~ "Sacramento-Sacramento Valley Station",
      Station == "San Diego-Old Town" ~ "San Diego-Old Town Transportation Center",
      Station == "San Diego" ~ "San Diego-Santa Fe Depot",
      Station == "San Jose" ~ "San Jose-Diridon Station",
      Station == "San Luis Obispo" ~ "San Luis Obispo-Amtrak Station",
      Station == "Santa Barbara" ~ "Santa Barbara-Amtrak Station",
      Station == "Santa Clara (University)" ~ "Santa Clara-Transit Center",
      Station == "Stockton (Downtown)" ~ "Stockton-Robert J. Cabral Station",
      Station == "Victorville" ~ "Victorville-Amtrak Station",
      Station == "Lakeland" ~ "Lakeland",
      Station == "Worcester" ~ "Worcester-Union Station",
      Station == "Lamy (Santa Fe)" ~ "Lamy",
      Station == "Eugene" ~ "Eugene-Amtrak Station",
      Station == "Portland" & StateAbb == "OR" ~ "Portland-Union Station",
      Station == "Providence" ~ "Providence-Amtrak/MBTA Station",
      Station == "Richmond - Staples Mill" ~ "Richmond-Staples Mill Road Station",
      Station == "Seattle" ~ "Seattle-King Street Station",
      Station == "Wenatchee" ~ "Wenatchee-Amtrak Station",
      Station == "Denver" ~ "Denver-Union Station",
      Station == "Fremont" & StateAbb == "CA" ~ "Fremont-Amtrak/ACE Station",
      Station == "Houston" ~ "Houston-Amtrak Station",
      Station == "Salinas" ~ "Salinas-Amtrak Station",
      Station == "Tampa" ~ "Tampa-Union Station",
      Station == "Greensburg" ~ "Greensburg-Amtrak Station",
      Station == "Latrobe" ~ "Latrobe-Amtrak Station",
      Station == "Lewistown" & StateAbb == "PA" ~ "Lewistown-Amtrak Station",
      Station == "Tacoma" ~ "Tacoma-Amtrak Station",
      Station == "Chicago" ~ "Chicago-Union Station",
      Station == "Meridian" ~ "Meridian-Union Station",
      Station == "St. Louis" ~ "St. Louis-Gateway Station",
      Station == "Newark" ~ "Newark-Penn Station",
      Station == "Newark Airport" ~ "Newark",
      Station == "Rochester" & StateAbb == "NY" ~ "Rochester-Louise M. Slaughter Station",
      Station == "Raleigh" ~ "Raleigh-Union Station",
      Station == "Spokane" ~ "Spokane-Amtrak Station",
      Station == "Cumberland" ~ "Cumberland-Amtrak Station",
      Station == "Pittsburgh" ~ "Pittsburgh-Union Station",
      Station == "Leavenworth" & StateAbb == "WA" ~ "Leavenworth-Icicle Station",
      Station == "Lafayette" & StateAbb == "IN" ~ "Lafayette-Amtrak Station",
      Station == "Windsor" & StateAbb == "VT" ~ "Windsor-Mt. Ascutney",
      Station == "New Haven" ~ "New Haven-Union Station",
      Station == "Baltimore" ~ "Baltimore-Penn Station",
      Station == "Flagstaff" ~ "Flagstaff-Amtrak Station",
      Station == "New London" ~ "New London-Union Station",
      Station == "Kansas City" ~ "Kansas City-Union Station",
      Station == "Fort Edward" ~ "Fort Edward-Glens Falls",
      Station == "Milwaukee" ~ "Milwaukee-Downtown-Intermodal Station",
      Station == "New York City (Penn Station)" ~ "New York",
      Station == "Philadelphia William H. Gray III 30th Street" ~ "Philadelphia",
      Station == "Philadelphia-North" ~ "North Philadelphia",
      Station == "Connellsville" & StateAbb == "IN" ~ "Connersville",
      Station == "Albany-Rensselaer" & StateAbb == "IN" ~ "Rensselaer",
      TRUE ~ Station
    )) %>%
  stringdist_left_join(
    station_codes,
    by = c("Station", "StateAbb"),
    max_dist = 20,
    method = "qgram",
    distance_col = "dist") %>%
  filter(StateAbb.dist == 0) %>%
  group_by(Station.x, StateAbb.x) %>%
  filter(Station.dist == min(Station.dist)) %>%
  ungroup() %>%
  select(
    Station_Name,
    Station.x,
    StateAbb.x,
    Code,
    Station.y,
    StateAbb.y,
    Station.dist
  ) %>%
  transmute(
    station_name = Station_Name,
    station_abbr = ifelse(station_name == "Lakeland, Florida", "LAK/LKL", Code)
  ) %>%
  distinct()

# riders
riders_df <-
  riders_raw %>%
  select(-State, -Note) %>%
  pivot_longer(
    cols = `2021`:`2005`,
    names_to = "year",
    values_to = "riders") %>%
  filter(if_all(everything(), ~!is.na(.x))) %>%
  mutate(year = as.integer(year)) %>%
  complete(Station, year) %>%
  inner_join(
    riders_station, by = c("Station" = "station_name")
  ) %>%
  arrange(Station, year) %>%
  select(station_abbr, year, riders)

# save out
write_rds(riders_df, "data_out/riders.rds")

# Wiki scrape -------------------------------------------------------------

wiki_routes_html <-
  read_html("https://en.wikipedia.org/wiki/List_of_Amtrak_routes")
wiki_routes_urls <-
  wiki_routes_html %>%
  html_elements(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]/tbody/tr/td[1]') %>%
  html_elements("a") %>%
  html_attr("href")
wiki_routes_df <-
  wiki_routes_html %>%
  html_elements(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table() %>%
  pluck(1) %>%
  janitor::clean_names() %>%
  bind_cols(
    tibble(url = wiki_routes_urls)
  ) %>%
  mutate(name = str_replace(name, "Zephyrand", "Zephyr and"))
wiki_stations_html <-
  read_html("https://en.wikipedia.org/wiki/List_of_Amtrak_stations")
wiki_stations_urls <-
  wiki_stations_html %>%
  html_elements(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]/tbody/tr/td[1]') %>%
  html_elements("a") %>%
  html_attr("href")
wiki_stations_df <-
  wiki_stations_html %>%
  html_elements(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table() %>%
  pluck(1) %>%
  janitor::clean_names() %>%
  bind_cols(
    tibble(url = wiki_stations_urls)
  ) %>%
  mutate(route = map(
    route,
    ~{
      str_split(.x, pattern = "\\n") %>%
        pluck(1) %>%
        str_squish()
    }
  ))


# OSM Geoms ---------------------------------------------------------------

states_sf <-
  states(cb = T, progress_bar = F) %>%
  filter(
    STUSPS %in% c(state.abb, "DC"),
    !STUSPS %in% c("HI", "AK"))

osm_amtrak_df <- tibble(
  route = c(
    "Amtrak Berkshire Flyer: Pittsfield => New York",
    "Amtrak Palmetto: Savannah => New York",
    "Amtrak Silver Meteor: Miami => New York",
    "Amtrak Silver Star: Miami => New York",
    "Amtrak Valley Flyer: New Haven => Greenfield",
    "VIA Rail Maple Leaf: Niagara Falls => Toronto",
    "Amtrak Cascades: Vancouver => Seattle",
    "Amtrak Northeast Regional: Springfield => Washington"
  ),
  state = c(
    "MA",
    "GA",
    "FL",
    "FL",
    "CT",
    "NY",
    "WA",
    "MA"
  )
)

# amtrack
osm_amtrak_fcn <- function(route, state) {

  res <-
    opq(bbox = st_bbox(states_sf %>% filter(STUSPS == state)), timeout = 300) %>%
    add_osm_feature(key = 'type', value = 'route') %>%
    add_osm_feature(key = 'name', value = route, value_exact = T) %>%
    osmdata_sf() %>%
    pluck("osm_multilines")

  return(res)
}

osm_routes_raw <- map2_dfr(osm_amtrak_df$route, osm_amtrak_df$state, osm_amtrak_fcn)

osm_routes_prep <-
  osm_routes_raw %>%
  tibble() %>%
  filter(role == "(no role)") %>%
  transmute(
    name = str_extract(name, "[^:]+"),
    name = str_remove(name, "Amtrak|VIA Rail"),
    name = str_squish(name),
    name = ifelse(name == "Cascades", "Amtrak Cascades", name),
    geometry
  )


# more cleaning
trainweb_routes <-
  read_html("http://www.trainweb.org/usarail/stationlist3.htm") %>%
  html_elements(xpath = '//*[@id="AutoNumber7"]') %>%
  html_elements("a") %>%
  html_attr("href") %>%
  paste0("http://www.trainweb.org/usarail/", .) %>%
  head(n = 47) %>%
  unique() %>%
  enframe(name = "row", value = "url") %>%
  mutate(
    page = map(url, read_html)) %>%
  mutate(
    tables = map(
      page, ~html_elements(.x, "table")
    ),
    title = map(tables, ~html_table(pluck(.x, 1), convert = F)),
    train_data = map(
      tables, ~pluck(keep(.x %>% html_table(., convert = F),
                          function(x) as.character(x[1, 1]) == "Route")), 1),
    station_html = map(
      tables,
      ~pluck(keep(.x,
                  function(x) html_element(x, 'tr') %>%
                    html_element('td') %>%
                    html_text() %>% str_detect("Stops|Station|Stations")), 1)),
    station_data = map(
      station_html,
      ~{
        tbl_df <- html_table(.x, header = TRUE) %>%
          setNames(c("station", "state", "tz", "dist", "ele", "notes")) %>%
          mutate_all(str_squish)
        links <- html_elements(.x, 'a')
        links_df <- tibble(
          station = html_text(links),
          station_url = paste0(
            "http://www.trainweb.org/usarail/", html_attr(links, 'href'))
        ) %>%
          mutate_all(str_squish)
        tbl_df <-
          tbl_df %>%
          left_join(links_df, by = "station")
        return(tbl_df)
      }
    )
  ) %>%
  select(url, title, train_data, station_data) %>%
  mutate(
    title = map_chr(title, ~pull(.x, X1) %>% glue::glue_collapse(., ";"))) %>%
  unnest(train_data) %>%
  unnest(train_data) %>%
  filter(!str_detect(X1, "Route")) %>%
  select(-X3) %>%
  pivot_wider(names_from = X1, values_from = X2) %>%
  janitor::clean_names() %>%
  unnest(station_data) %>%
  janitor::remove_empty(which = "cols") %>%
  filter(!str_detect(notes, "Suspended") | is.na(notes)) %>%
  filter(!str_detect(station, "^\\(")) %>%
  mutate(across(where(is.character), str_squish)) %>%
  group_by(title) %>%
  mutate(stop_num = row_number()) %>%
  ungroup() %>%
  mutate(
    name = str_extract(title, "[^;]+"),
    name = str_to_title(str_extract(name, "[^(]+")),
    name = str_replace(name, "\\/", "and"),
    name = str_replace(name, " Of", " of "),
    name = str_replace(name, "Zephyrcarl", "Zephyr and Carl"),
    name = case_when(
      name == "Cascades" ~ "Amtrak Cascades",
      name == "Hiawatha" ~ "Hiawatha Service",
      name == "San Joaquin" ~ "San Joaquins",
      TRUE ~ name),
    name = str_squish(name))

# clean stations
trainweb_stations <-
  trainweb_routes %>%
  distinct(station, state, station_url) %>%
  filter(str_length(station) > 1) %>%
  mutate(station_url = case_when(
    station == "Boston - South" ~ "http://www.trainweb.org/usarail/boston_south.htm",
    station == "Boston - Back Bay" ~ "http://www.trainweb.org/usarail/boston_backbay.htm",
    station == "Boston - North" ~ "http://www.trainweb.org/usarail/boston_north.htm",
    station == "Santa Clara (Great America)" ~ "http://www.trainweb.org/usarail/santaclara.htm",
    station == "Santa Clara (SC University)" ~ "http://www.trainweb.org/usarail/santaclara_university.htm",
    station == "Stockton (San Joaquin St.)" ~ "http://www.trainweb.org/usarail/stockton.htm",
    station == "Stockton (ACE / Downtown)" ~ "http://www.trainweb.org/usarail/stocktonace.htm",
    station == "New Haven - State Street" ~ "http://www.trainweb.org/usarail/newhaven_state.htm",
    station == "New Haven - Union Station" ~ "http://www.trainweb.org/usarail/newhaven.htm",
    station == "Durham - UNH" ~ "http://www.trainweb.org/usarail/durhamnh.htm",
    station == "New York - Penn. Station" ~ "http://www.trainweb.org/usarail/newyork.htm",
    station == "Philadelphia - 30th Street" ~ "http://www.trainweb.org/usarail/philadelphia.htm",
    station == "Richmond - Staples Mill Rd." ~ "http://www.trainweb.org/usarail/richmondva.htm",
    station == "Richmond - Main Street" ~ "http://www.trainweb.org/usarail/richmondmain.htm",
    station == "Burlington Union Station" ~ "http://www.trainweb.org/usarail/burlingtonvt.htm",
    str_detect(station, "^Lakeland") & state == "FL" ~ "http://www.trainweb.org/usarail/lakeland.htm",
    station == "Bloomington" ~ "http://www.trainweb.org/usarail/normal.htm",
    station == "Springfield" & state == "IL" ~ "http://www.trainweb.org/usarail/springfield.htm",
    station == "Vancouver" & state == "WA" ~ "http://www.trainweb.org/usarail/vancouverWA.htm",
    station == "Vancouver" & state == "BC" ~ "http://www.trainweb.org/usarail/vancouverBC.htm",
    station == "Newark" & state == "NJ" ~ "http://www.trainweb.org/usarail/newarknj.htm",
    station == "Newark" & state == "DE" ~ "http://www.trainweb.org/usarail/newarkde.htm",
    station == "Niagara Falls" & state == "NY" ~ "http://www.trainweb.org/usarail/niagarafallsny.htm",
    station == "Niagara Falls" & state == "ON" ~ "http://www.trainweb.org/usarail/niagarafallson.htm",
    TRUE ~ station_url)
  ) %>%
  distinct() %>%
  mutate(
    page = map(station_url, read_html),
    page_df = map(
      page,
      ~{
        df <- html_element(.x, xpath = '/html/body/table') %>%
          html_table() %>%
          mutate(row = row_number()) %>%
          pivot_wider(values_from = X1, names_from = row) %>%
          setNames(c("station_name", "state_name", "station_abbr"))
        return(df)
      }
    )) %>%
  unnest(page_df) %>%
  mutate(station_abbr = str_remove_all(station_abbr, "[:punct:]"))

# clean routes
trainweb_df <-
  trainweb_routes %>%
  select(-station_url) %>%
  left_join(
    trainweb_stations %>%
      distinct(station, state, station_abbr),
    by = c("station", "state")
  ) %>%
  mutate(
    across(c(station, state), ~ifelse(.x == "", "BLANK", .x)),
    station_abbr = ifelse(station == "BLANK", "BLANK", station_abbr)) %>%
  filter(station_abbr != "MCI") %>%
  mutate(
    station_abbr = case_when(
      station == "Palatka" & state == "FL" ~ "PAK",
      station == "Old Orchard Beach" & state == "ME" ~ "ORB",
      station == "San Clemente Pier" & state == "CA" ~ "SNP",
      str_detect(station, "^Lakeland") & state == "FL" ~ "LAK/LKL",
      TRUE ~ station_abbr
    )
  )
# test <- trainweb_stations %>%
#   # slice(11) %>%
#   mutate(
#     tables = map(page, ~html_elements(.x, 'table')),
#     station_data_table = map(
#       tables, ~pluck(keep(.x %>% html_table(., convert = F),
#                           function(x) as.character(x[1, 1]) == "Address")), 1),
#     station_data = map(
#       station_data_table,
#       ~safely({
#         df <-
#           pluck(.x, 1) %>%
#           select(X1, X2) %>%
#           pivot_wider(names_from = X1, values_from = X2) %>%
#           janitor::clean_names()
#         return(df)
#       }
#       )
#     )
#   ) %>%
#   unnest_wider(station_data)
# trainweb_stations <-
#   read_html("http://www.trainweb.org/usarail/stationlist3.htm") %>%
#   html_elements(xpath = '//*[@id="AutoNumber3"]') %>%
#   html_elements('a') %>%
#   html_attr('href') %>%
#   paste0("http://www.trainweb.org/usarail/", .) %>%
#   map(
#     .,
#     ~{
#       tbl_html <- read_html(.x) %>% html_element('table')
#       tbl_df <- html_table(tbl_html, header = TRUE)
#       links <- tbl_html %>%
#         html_elements('a') %>%
#         html_attr('href') %>%
#         unique() %>%
#         paste0("http://www.trainweb.org/usarail/", .)
#       tbl_df <- tbl_df %>% bind_cols(tibble(urls = links))
#       return(tbl_df)
#     }
#   ) %>%
#   map(., ~setNames(.x, c("station", "state", "url"))) %>%
#   bind_rows() %>%
#   mutate_all(str_squish) %>%
#   mutate(
#     page = map(url, read_html),
#     page_df = map(
#       page,
#       ~{
#         df <- html_element(.x, xpath = '/html/body/table') %>%
#           html_table() %>%
#           mutate(row = row_number()) %>%
#           pivot_wider(values_from = X1, names_from = row) %>%
#           setNames(c("station_name", "state_name", "station_abbr"))
#         return(df)
#       }
#     )) %>%
#   unnest(page_df) %>%
#   mutate(station_abbr = str_remove_all(station_abbr, "[:punct:]"))
# safe_read_html <- safely(read_html)
# trainweb_stations <-
#   trainweb_pages %>%
#   mutate(links = map(page, ~html_elements(.x, 'a') %>% html_attr('href'))) %>%
#   unnest(links) %>%
#   filter(
#     str_detect(links, "htm$"),
#     !str_detect(links, "^https"),
#     !str_detect(links, "[:digit:]")
#   ) %>%
#   distinct(links) %>%
#   pull(links) %>%
#   paste0("http://www.trainweb.org/usarail/", .) %>%
#   map(., ~safe_read_html(.x)) %>%
#   map("result") %>%
#   compact()
# trainweb_stations_df <-
#   trainweb_stations %>%
#   map(., ~html_element(.x, xpath = '/html/body/table') %>%
#         html_table()) %>%
#   map(., ~mutate(.x, row = row_number()) %>%
#         pivot_wider(values_from = X1, names_from = row)) %>%
#   bind_rows() %>%
#   rename(
#     station = 1,
#     state = 2,
#     station_abbr = 3
#   ) %>%
#   mutate(station_abbr = str_remove_all(station_abbr, "[:punct:]"))


# Updates -----------------------------------------------------------------

amtrak_routes <-
  amtrak_rout_sf %>%
  filter(
    !NAME %in% c(
      "Hoosier State", # Hoosier State suspended in 2019
      "Brightline", # private service in Florida
      "Silver_Service/Palmetto") # multiple routes need separate geometry
  ) %>%
  mutate(
    name = case_when(
      NAME == "Cascades" ~ "Amtrak Cascades",
      NAME == "Wolverines (M-DOT)" ~ "Wolverine",
      NAME == "Wolverines (Michigan_Services)" ~ "Wolverine",
      NAME == "Chicago - St.Louis" ~ "Lincoln Service",
      NAME == "Hiawathas" ~ "Hiawatha Service",
      NAME == "The Downeaster" ~ "Downeaster",
      NAME == "Regional" ~ "Northeast Regional",
      NAME == "DC - Lynchburg - Roanoke" ~ "Northeast Regional",
      NAME == "DC - Norfolk" ~ "Northeast Regional",
      NAME == "DC - Newport News" ~ "Northeast Regional",
      NAME == "DC – Richmond" ~ "Northeast Regional",
      NAME == "NEC" ~ "Northeast Regional",
      NAME == "Illinois Zephyr" ~ "Illinois Zephyr and Carl Sandburg",
      NAME == "Illini (Illinois_Service)" ~ "Illini and Saluki",
      NAME == "Kansas City - St. Louis (Missouri River Runner)" ~ "Missouri River Runner",
      NAME == "New Haven - Springfield" ~ "Hartford Line",
      TRUE ~ NAME
    )
  ) %>%
  bind_rows(osm_routes_prep) %>%
  group_by(name) %>%
  summarize() %>%
  ungroup() %>%
  inner_join(wiki_routes_df, by = "name") %>%
  left_join(
    trainweb_routes %>%
      distinct(name, distance, operation, time, train_set, cars),
    by = "name"
  ) %>%
  relocate(geometry, .after = last_col()) %>%
  mutate(
    route = str_remove(route, "\\(through cars to Los Angeles on the Sunset Limited\\)"),
    distance = ifelse(name == "Berkshire Flyer", "190 miles", distance),
    operation = ifelse(name == "Berkshire Flyer", "2 times a week", operation),
    time = ifelse(name == "Berkshire Flyer", "4h 00m", time),
    train_set = ifelse(name == "Berkshire Flyer", "Amfleet", train_set),
    cars = ifelse(name == "Berkshire Flyer", "Coach, Cafe", cars),
    fy2021passengers_1 = ifelse(name == "Adirondack", NA_character_, fy2021passengers_1),
    route_miles = case_when(
      name == "Texas Eagle" ~ "1,306",
      name == "Lake Shore Limited" ~ "1,018 (Boston);959 (New York)",
      name == "Northeast Regional" ~ "644 (Newport News);679 (Norfolk);682 (Roanoke)",
      name == "San Joaquins" ~ "318 (Oakland);280 (Sacramento)",
      name == "Empire Builder" ~ "2,257 (Portland);2,206 (Seattle)",
      TRUE ~ route_miles
    ),
    time = str_remove(time, "\\(Westbound\\)|\\(Westbound Train\\)|\\(Southbound\\)|\\(Southbound Train\\)|\\(Northbound\\)|\\(Northbound Train\\)|\\(Fastest\\)|\\(Fastest Train\\)"),
    time = case_when(
      name == "Texas Eagle" ~ "32h 25m",
      name == "Lake Shore Limited" ~ "22h 50m (Boston);19h 0m (New York)",
      name == "Northeast Regional" ~ "14h 0m",
      name == "Amtrak Cascades" ~ "3h 55m (Vancouver);6h 10m (Eugene)",
      name == "Empire Builder" ~ "45h 00m (Portland);45h 10m (Seattle)",
      TRUE ~ time
    ),
    time = str_replace(time, " 00m", " 0m"),
    daily_round_trips = str_replace(daily_round_trips, "Sunday", "Sun"),
    daily_round_trips = str_replace(daily_round_trips, "Monday", "Mon"),
    daily_round_trips = str_replace(daily_round_trips, "Saturday", "Sat")
  ) %>%
  transmute(
    name,
    route,
    daily_round_trips,
    fy2021_passengers = parse_number(str_remove(fy2021passengers_1, "(2019)")),
    route_miles,
    time,
    url = paste0('https://en.wikipedia.org', url),
    cars
  ) %>%
  mutate(across(c(route_miles, time), ~str_replace_all(.x, "\\)", ");"))) %>%
  mutate(across(c(route_miles, time), ~str_replace_all(.x, "\\;{2}", ";"))) %>%
  mutate(across(c(route_miles, time), ~str_remove(.x, "\\;$"))) %>%
  mutate(across(c(route_miles, route, time), ~str_replace_all(.x, "[:space:]{0,1}\\–[:space:]{0,1}", "-"))) %>%
  arrange(desc(fy2021_passengers))

write_rds(amtrak_routes, "data_out/routes.rds")


# Prep Station Data -------------------------------------------------------

berkshire_df <-
  tibble(
    route = "Berkshire Flyer",
    station_abbr = c("PIT", "ALB", "HUD", "RHI", "POU", "CRT", "YNY", "NYP")
  ) %>%
  unnest(station_abbr) %>%
  mutate(stop_num = row_number()) %>%
  left_join(
    wiki_stations_df %>%
      select(
        station_wiki = station,
        station_abbr = station_code,
        location,
        state_or_province,
        station_routes = route,
        opened,
        rebuilt,
        url),
    by = "station_abbr"
  ) %>%
  left_join(
    amtrak_stat_sf %>%
      select(
        station_type = StaType,
        station_abbr = Code,
        station_fullname = StationNam,
        geometry
      ),
    by = "station_abbr"
  )
ethan_allen_stat <-
  tibble(
    station_type = rep("Station Building (with waiting room)", 3),
    station_abbr = c("MBY", "VRN", "BTN"),
    station_fullname = c("Middlebury, VT", "Ferrisburgh–Vergennes, VT", "Burlington, VT"),
    x = c(-73.1698, -73.2489, -73.219577),
    y = c(44.0177, 44.1809, 44.475709)
  ) %>%
  st_as_sf(., coords = c("x", "y"), crs = 4326)

stations_df <-
  trainweb_df %>%
  select(
    route = name,
    stop_num,
    station,
    state,
    station_abbr,
    dist) %>%
  left_join(
    wiki_stations_df %>%
      select(
        station_wiki = station,
        station_abbr = station_code,
        location,
        state_or_province,
        station_routes = route,
        opened,
        rebuilt,
        url),
    by = "station_abbr"
  ) %>%
  mutate(
    station_abbr = case_when(
      station_abbr == "LAK/LKL" & stop_num == 27 ~ "LAK",
      station_abbr == "LAK/LKL" & stop_num == 29 ~ "LKL",
      TRUE ~ station_abbr
    )
  ) %>%
  left_join(
    bind_rows(
      amtrak_stat_sf %>%
        select(
          station_type = StaType,
          station_abbr = Code,
          station_fullname = StationNam,
          geometry),
      ethan_allen_stat),
    by = "station_abbr"
  ) %>%
  bind_rows(berkshire_df)

stations_df <- st_set_geometry(stations_df, stations_df$geometry)

amtrak_stations <-
  stations_df %>%
  mutate(station = ifelse(is.na(station), station_wiki, station)) %>%
  filter(station != "BLANK") %>%
  filter(station_abbr != "ALX" | (station_abbr == "ALX" & stop_num <= 45)) %>%
  filter(station_abbr != "RVR" | (station_abbr == "RVR" & stop_num <= 61)) %>%
  filter(station_abbr != "SPK" | (station_abbr == "SPK" & stop_num <= 35)) %>%
  group_by(route, station_abbr) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(route, stop_num) %>%
  group_by(route) %>%
  mutate(
    stop_num = row_number(),
    stops = n()) %>%
  ungroup() %>%
  mutate(
    junction_type = case_when(
      route == "Empire Builder" & station_abbr == "SPK" ~ "1-2_split",
      route == "Empire Builder" & station_abbr == "SEA" ~ "end_w_bypass",
      route == "Empire Builder" & stop_num %in% 36:40 ~ "cont_w_bypass",
      route == "Empire Builder" & station_abbr == "PSC" ~ "cont_aft_bypass",
      route == "Lake Shore Limited" & station_abbr == "PIT" ~ "beg_bypass",
      route == "Lake Shore Limited" & station_abbr == "NYP" ~ "sta_w_bypass",
      route == "Lake Shore Limited" & stop_num %in% 8:9 ~ "cont_w_bypass",
      route == "Lake Shore Limited" & station_abbr == "ALB" ~ "2-1_comb",
      route == "San Joaquins" & station_abbr == "MOD" ~ "1-2_split_aft",
      route == "San Joaquins" & stop_num %in% 10:14 ~ "cont_w_bypass",
      route == "San Joaquins" & station_abbr == "OKJ" ~ "end_w_bypass",
      route == "San Joaquins" & station_abbr == "SKT" ~ "cont_aft_bypass",
      route == "Northeast Regional" & station_abbr == "OSB" ~ "beg_bypass",
      route == "Northeast Regional" & station_abbr == "SPG" ~ "sta_w_bypass",
      route == "Northeast Regional" & stop_num %in% 11:16 ~ "cont_w_bypass",
      route == "Northeast Regional" & station_abbr == "NHV" ~ "2-1_comb",
      route == "Northeast Regional" & station_abbr == "ALX" ~ "1-2_split",
      route == "Northeast Regional" & station_abbr == "RNK" ~ "end_w_bypass",
      route == "Northeast Regional" & stop_num %in% 39:43 ~ "cont_w_bypass",
      route == "Northeast Regional" & station_abbr == "WDB" ~ "cont_aft_bypass",
      route == "Northeast Regional" & station_abbr == "RVR" ~ "1-2_split",
      route == "Northeast Regional" & station_abbr == "NPN" ~ "end_w_bypass",
      route == "Northeast Regional" & stop_num %in% 50:51 ~ "cont_w_bypass",
      route == "Northeast Regional" & station_abbr == "PTB" ~ "cont_aft_bypass",
      stop_num == 1 ~ "sta",
      stop_num == stops ~ "end",
      TRUE ~ "cont"
    )
  ) %>%
  mutate(
    station_routes = map2(station_routes, route, ~str_subset(.x, .y, negate = T)),
    station_routes = modify_if(station_routes, ~length(.) == 0, ~NA_character_),
    other_routes = map_chr(station_routes, ~glue::glue_collapse(.x, sep = "; "))) %>%
  mutate(
    building_yrs = paste0(opened, ",", rebuilt),
    building_yrs = str_extract_all(building_yrs, "[:digit:]{4}"),
    building_yrs = modify_if(building_yrs, ~length(.) == 0, ~NA_character_),
    year_min = map_dbl(building_yrs, ~min(as.numeric(.x))),
    year_max = map_dbl(building_yrs, ~max(as.numeric(.x)))) %>%
  transmute(
    route,
    stop_num,
    junction_type,
    station_name = paste0(station_wiki, " (", station_abbr, ")"),
    station_abbr,
    state_or_province,
    country = ifelse(state_or_province %in% c("ON", "QC", "BC"), "CA", "US"),
    station_routes,
    other_routes,
    opened = year_min,
    station_type,
    url = paste0('https://en.wikipedia.org', url)
  )
write_rds(amtrak_stations, "data_out/stations.rds")
