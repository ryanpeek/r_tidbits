# RangeShiftR
# https://rangeshifter.github.io/RangeshiftR-tutorials/overview_0.html

library(RangeShiftR)

s <- RSsim()

# create dirs we need
library(fs)

# make req dirs
fs::dir_create("RS")
fs::dir_create("RS/Inputs")
fs::dir_create("RS/Outputs")
fs::dir_create("RS/Output_Maps")


# Params ------------------------------------------------------------------

init <- Initialise(FreeType = 0,
                   NrCells = 2250,
                   InitDens = 2,
                   IndsHaCell = 3,
                   PropStages = c(0,0.7,0.3))
init

# Run Stuff ---------------------------------------------------------------

RunRS(s, "RS/")
s # see params


# Demography --------------------------------------------------------------

demo <- Demography(Rmax = 2.2, ReproductionType = 1, PropMales = 0.45)

stg <- StageStructure(Stages = 3,
                      TransMatrix = matrix(c(0,1,0,5.7,.5,.4,3.4,0,.9),nrow = 3),
                      FecDensDep = T,
                      SurvDensDep = T)
demo <- demo + stg

# simpler
demo <- Demography(StageStruct = stg, ReproductionType = 1, PropMales = 0.45)

plotProbs(stg)

# Dispersal ---------------------------------------------------------------

disp <-  Dispersal(Emigration = Emigration(EmigProb = 0.2),
                   Transfer   = DispersalKernel(Distances = 50),
                   Settlement = Settlement() )

plotProbs(DispersalKernel(Distances = matrix(c(0,1,2,70,50,30),nrow = 3), StageDep = T))

disp <-  disp + Settlement(SexDep = T,
                          Settle = matrix(c(0,1,1,0), nrow = 2))

dev.off()

# Landscapes -----------------

# can import from ASCII raster in inputs or simulate random map

land <- ImportedLandscape()
land <- ArtificialLandscape()

land <- ArtificialLandscape(Resolution = 10,  # in meters
                            K_or_DensDep = 1500,  # ~ 15 inds/cell
                            propSuit = 0.2,
                            dimX = 129, dimY = 257,
                            fractal = T, hurst = 0.3,
                            continuous = F)
land



# Simulation --------------------------------------------------------------

sim <- Simulation(Simulation = 2,
                  Years = 50,
                  Replicates = 2,
                  OutIntPop = 50)

gene <- Genetics(NLoci = 3, ProbMutn = .05)

s <- RSsim(simul = sim, land = land, demog = demo, dispersal = disp, gene = gene, init = init)

# validate
validateRSparams(s)


# Run ---------------------------------------------------------------------

RunRS(s, "RS/")


# Plot Results ------------------------------------------------------------

range_df <- readRange(s, "RS/")
par(mfrow=c(1,2))
plotAbundance(range_df)
plotOccupancy(range_df)

# read population output file into a dataframe
pop_df <- readPop(s, "RS/")

# Not all years have the same number of populated and thus listed cells. For stacking, we set a common extent with the values used in the landscape module:
ext <- c(0,1290,0,2570)
res <- 10

# Make stack of different raster layers for each year and for only one repetition (Rep==0):
pop_wide_rep0 <- reshape(subset(pop_df,Rep==0)[,c('Year','x','y','NInd')], timevar='Year', v.names=c('NInd'), idvar=c('x','y'), direction='wide')

library(raster)
stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
names(stack_years_rep0) <- c('Year.0', 'Year.50')
spplot(stack_years_rep0, zlim = c(0,7))
