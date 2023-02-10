# function for water year day
add_wyd <- function(date, start_mon = 10L){

  start_yr <- year(date) - (month(date) < start_mon)
  start_date <- make_date(start_yr, start_mon, 1L)
  wyd <- as.integer(date - start_date + 1L)
  # deal with leap year
  offsetyr <- ifelse(lubridate::leap_year(date), 1, 0) # Leap Year offset
  adj_wyd <- ifelse(offsetyr==1 & month(date) >= start_mon, wyd - 1, wyd)
  return(adj_wyd)
}

# function for water year week
add_wyweek <- function(wyday){
  wyw <- wyday %/% 7 + 1
  return(wyw)
}
