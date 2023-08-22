#install.packages("MixSIAR", dependencies=TRUE)
library(MixSIAR)
library(R2WinBUGS)
library(splancs)

#load mix data (ie the consumers. always cite brian's paper about this package)

# using GLUE is a great way to make one switch to a root path
# and then all the other paths will work on anyone's computer
library(glue)
path_root <- "C:/Users/RPeek/Downloads"

thi_2020_fish<-read.csv(glue("{path_root}/source.csv"), header = TRUE, sep = ",", stringsAsFactors = TRUE)


thi_2020_fish$species<-as.numeric(thi_2020_fish$species)
thi_2020_fish$site<-as.numeric(thi_2020_fish$site)

# had to change the base data to "C" instead "c" and "N" instead of "n"
mix<-load_mix_data(filename = glue("{path_root}/source.csv"),
                   iso_names = c("d13C", "d15N"),
                   factors = c("species_model", "site_model"),
                   fac_random = c(FALSE, FALSE),
                   fac_nested = c(FALSE, FALSE),
                   cont_effects = NULL)

#load the source data:
# NOTE: `source()` is a protected function...if you just type source
# in the console, it will return the underlying function
# make sure to name as something other than "source", and use explicit
# argument for source = "object" throughout.
# usually they won't let you make an argument name that conflicts like this
# but here we are... :)


prey<- read.csv(glue("{path_root}/prey_model.csv"), header = TRUE, sep = ",", stringsAsFactors = TRUE)

prey$site<-as.numeric(prey$site)

prey<-load_source_data(filename = glue("{path_root}/prey_model.csv"),
                         source_factors = NULL,
                         conc_dep = FALSE,
                         data_type = "raw",
                         mix)



#load the fractionation data
discr<-read.csv(glue("{path_root}/tef_new.csv"))
discr<-load_discr_data(filename = glue("{path_root}/tef_new.csv"), mix)

mix$iso_names
mix$MU_names
mix$SIG_names

#make isospace plot

plot_data(filename = "figs/isospace_plot" , plot_save_pdf = FALSE, plot_save_png = TRUE, mix = mix, source = prey, discr = discr)

plot_prior(alpha.prior = c(1), prey) #currently having the source being weighed equally but might need to change based on anchovie populations

#informative prior

#plot_prior(alpha.prior = c(0.5, 0.5, 1), source)

#write the jags model

model_filename <- "data_out/MixSIAR_model.txt"
resid_err = TRUE
process_err <-TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, prey)

#run model

run<-list(chainLength = 1000, burn = 500, thin = 1, chains = 3, calcDIC = TRUE)

#run test first to make sure things are loaded correctly
#uninformed prior (should take a long time to run)

jags.1<- run_model(run = "test", mix, prey, discr, model_filename,
                   alpha.prior = 1, resid_err, process_err)

#informed prior
jags.1<- run_model(run = "test", mix, prey, discr, model_filename,
                   alpha.prior = c(0.5, 0.5, 1, 1), resid_err, process_err)

# RYAN: NOTE CHECK PRIORS HERE IT NEEDS 4, and wasn't sure what was needed...so added "1" at the end...



#what info do we want will be in the folder where the package is placed. If the variables in the gelman are above 1.05, that means we need to run the model longer to get convergence. want only a few above 1.05 to be confident in the results for interpration of the data


output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_nme = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

#then you can call output_JAGS to process diagnostics, sum stats, and create posterior density plots (last step) for a more thorough explantaiton of the output see the Wolves example in the mixsiar manual.

output_JAGS(jags.1, mix, prey, output_options)

