
# install ffc

# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
library(tidyverse)

## Error: issue updating the CLI package
# C:\Users\RPeek\AppData\Local\R\win-library\4.2\00LOCK\cli\libs\x64\cli.dll

# login to eflows.ucdavis.edu, go to profile page and make sure logged in
# F12, go to console and type: localStorage.getItem('ff_jwt')
# library(usethis)
# edit_r_environ("user") # add EFLOWS_TOKEN

# set token
ffctoken <- set_token(Sys.getenv("EFLOWS_TOKEN", ""))


# start ffc processor
ffc <- FFCProcessor$new()
ffc$set_up(gage_id = 11427000,
           token = ffctoken)
           #plot_output_folder = "data_out/nfa_gage_alt")

# run it!
ffc$run()
ffc$predicted_percentiles  # predicted percentile data

# or run all
# look up COMID first
# ffcAPIClient::get_comid_for_lon_lat()

# If you have a gage and a token, you can get all results simply by running
ffcAPIClient::evaluate_gage_alteration(gage_id = 11427000,
                                       force_comid_lookup = TRUE,
                                       token = ffctoken,
                                       plot_output_folder = "data_out/nfa_gage_alteration")


# try just step one: ----------------------------

ffc <- FFCProcessor$new()
ffc$step_one_functional_flow_results(gage_id=11336000,
                                     token=ffctoken,
                                     output_folder = "C:/Users/RPeek/Downloads/ffc_test")
