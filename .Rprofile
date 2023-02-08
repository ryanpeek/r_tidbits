# library(cowsay) # animals!
# library(glue)   # pasting things together
#
# # get vector of all animals
# animals <- names(cowsay::animals)
#
# # get pieces to make link
# repo <- "JakubPetriska/060958fd744ca34f099e947cd080b540"
# csv <- "raw/963b5a9355f04741239407320ac973a6096cd7b6/quotes.csv"
#
# # get dataframe of inspirational quotes
# quotes  <- readr::read_csv(glue("https://gist.githubusercontent.com/{repo}/{csv}"))
#
# # make full quote
# quotes$full_quote  <- glue("{quotes$Quote} - {quotes$Author}")
#
# # now use it!
# cowsay::say(sample(quotes$full_quote, 1), by = sample(animals, 1))
