# single season single spp occupancy

library(terra)
library(unmarked)

# data
bluebugs <- read.csv("data_raw/bluebug.csv")

# convert count to 1/0
y <- bluebugs[,4:9]
y[y>1] = 1
tail(y)

# Fix Dates ---------------------------------------------------------------

# Extract the data for the date covariate.
dates <- as.matrix(bluebugs[ ,10:15])

# Extract the data for minutes after noon.
mins <- as.matrix(bluebugs[ ,16:21])

# Extract data for woodpile location (0: within the forest, 1: on the edge of the forest)
siteCovs <- bluebugs[,c("forest_edge")]

# Save site data as a data frame.
site <- data.frame(siteCovs)

# Standardize (center) the dates
mean.date <- mean(dates, na.rm = TRUE)
sd.date <- sd(dates[!is.na(dates)])
DATES <- (dates-mean.date)/sd.date

DATES[is.na(DATES)] = 0

# Standardize the minutes, same procedure as dates
mean.mins <- mean(mins, na.rm = TRUE)
sd.mins <- sd(mins[!is.na(mins)])
MINS <- (mins - mean.mins)/sd.mins
MINS[is.na(MINS)] = 0

# Assign Covariates --------------------------------------------------

dates = as.matrix(DATES)
mins = as.matrix(MINS)
dates.x = (dates)^2
mins.x = (mins)^2
obs = list(date = dates, mins = mins, dates.2 = dates.x, mins.2 = mins.x)


# Setup Occupancy List in Unmarked ----------------------------------------

wt <- unmarkedFrameOccu(y = y, siteCovs = site, obsCovs = obs)
summary(wt)


# Example Models ----------------------------------------------------------

# No observation covariates, site covariates

## occu(~ 1 ~ siteCovs, data = wt)

# Observation covariates, no site covariates

## occu(~ observation covariates ~ 1, data = wt)

# Observation covariates and site covariates

## occu(~ observation covariates ~ site covariates, data = wt)

# Model 1: --------------------------------------------------

# look at effect of location of sites on occupancy and whether date and time of day has effect on detection probability. Include both site covariates & observation covariates

m1 <- occu(~date+dates.2+mins+mins.2~siteCovs,data = wt)
summary(m1)

# back transform to probabilities:
backTransform(linearComb(m1, coefficients = c(1, 0), "state"))
# Occupancy probability of sites within the a given state (forest here).
backTransform(linearComb(m1, coefficients = c(1, 1), "state"))
# Occupancy probability of sites on the forest edge

# Now look at covariates on detection
backTransform(linearComb(m1, coefficients = c(1, mean(dates), 0, 0, 0),"det")) #Probability set at mean date
backTransform(linearComb(m1, coefficients = c(1, 0, mean(dates.x), 0, 0),"det")) # Probability set at mean quadratic date
backTransform(linearComb(m1, coefficients = c(1, 0, 0, mean(mins), 0),"det")) # mean minutes after noon
backTransform(linearComb(m1, coefficients = c(1, 0, 0, 0, mean(mins.x)),"det"))  # Probability set at mean minutes after noon (quadratic)

## Get True Occupancy:
sum(bup(ranef(m1), stat="mode"))     # Sum of posterior modes

# Plotting habitat occupancy probability
newdata1 <- data.frame(siteCovs=c("Edge", "Interior"),date = 0,dates.2 = 0,mins = 0,mins.2 = 0) # Sim data for the predict function

Epsi <- predict(m1, type="state", newdata=newdata1)
with(Epsi, {
  plot(1:2, Predicted, xaxt="n", xlim=c(0.5, 2.5),ylim = c(0,1),
       xlab="Habitat",
       ylab=expression(paste("Probability of occurrence (", psi, ")")),
       cex.lab=1.2,
       pch=16, cex=1.5, main = "Probability of Occupancy for Each Habitat")
  axis(1, 1:2, c('Interior', 'Edge'))
  arrows(1:2, lower, 1:2, upper, angle=90, code=3, length=0.05)
})


## Plot 2
# Plot detection as a function of date and minutes afternoon using estimates produced by the model.
par(mfrow=c(2, 1), mar = c(5,5,5,5))

tdates = seq(0,60, length = 30) # Set up date data
tdatess = (tdates - mean.date)/sd.date # Standardize date data
tdatesss = tdatess^2 # set up your date quadratic

newdata2 <- data.frame(date=tdatess, dates.2 = tdatesss, mins = 0, mins.2 = 0) # Make data.frame out of data generated above

Ep <- predict(m1, type="det", newdata=newdata2, appendData=TRUE) #Use predict function to generated predictions for new data based off model estimates
Ep$dateOrig = seq(0,60, length = 30) # Set up x-axis dates
with(Ep,{plot(dateOrig, Predicted, type="l", lwd=2, ylim=c(0,1), #Plot the predicted values against dates
              xlab="Date (1 = July 1)",
              ylab=expression(paste("Detection probability (", italic(p), ")")), main = "Detection Probability by Date")
  lines(dateOrig,lower, col=gray(0.7))     # add confidence intervals
  lines(dateOrig,upper, col=gray(0.7))})

# Create Minutes Data frame to predict on
tmins = seq(180, 540, length.out = 30) # set up minute data
tminss = (tmins - mean.mins)/sd.mins # standardize minute data
tminsss = tminss^2 # make minutes quadratic
newdata3 = data.frame(date=0, dates.2 = 0, mins = tminss, mins.2 = tminsss) # make data.frame out of minutes data generated above

# predict using model
Ep1 <- predict(m1, type="det", newdata=newdata3, appendData=TRUE)
Ep1$minOrig = seq(180, 540, length.out = 30) # Setup vector for x-axis
with(Ep1,{plot(minOrig, Predicted, type="l", lwd=2, ylim=c(0,1), # Plot predicted values and minutes after noon.
               xlab="Minutes After Noon",
               ylab=expression(paste("Detection probability (", italic(p), ")")), main = "Detection Probability by Minutes After Noon")
  # add confidence intervals
  lines(minOrig,lower, col=gray(0.7))
  lines(minOrig,upper, col=gray(0.7))})



# Now w JAGS ----------------------------------------------------------


library("rjags")
library("jagsUI")
library("runjags")
library("coda")
library("MCMCvis")


## create jags txt file ----------------------------------------------------

cat(
  "
  model {

  ### Likelihood
  ### Estimating true state

  for(i in 1:R){            # Make a for- loop that loops through the number of sites surveyed
  z[i] ~ dbern(psi[i])                # The latent state z, whether animal was actually there or not is distributed by a bernoulli distribution
  psi[i] <- 1/(1 + exp(-lpsi.lim[i]))          # Calculate logit psi, probability of site i occupancy, on the logit scale
  lpsi.lim[i] <- min(999, max(-999,lpsi[i]))   # Truncate values on logit scale to prevent numeric overflow and underflow
  lpsi[i] <- alpha0.psi + alpha1.psi * edge[i]  # linear estimation of psi with different probability dependent on site (in woods, edge of woods)

  ### Modeling the observations
  for(j in 1:T){              # For loop iterating through number of surveys
  y[i,j] ~ dbern(mu.p[i,j])         #detected or not detected at site i and survey j
  mu.p[i,j] <- z[i] * p[i,j]           #Calculate probability of detection which = zi (whether site is occupied or not) * p (probability of detection at site i and survey j)
  p[i,j] <- 1/(1 + exp(-lp.lim[i,j]))          # Calculation of detection probability at site i and survey j on the logit scale
  lp.lim[i,j] <- min(999, max(-999, lp[i,j]))     # Truncate values to provent over/underflow
  lp[i,j] <- beta0.p + beta1.p*DATES[i,j] +           # Calculate detection probability as linear
             beta2.p * pow(DATES[i,j],2) +
             beta3.p * MINS[i,j] + beta4.p * pow(MINS[i,j],2)

   ### Bayes P-Value

   Presi[i,j] <- (y[i,j] - p[i,j])^2       # Calculate the squared residual error of the observed data
   y.new[i,j] ~ dbern(mu.p[i,j])                # Simulate observed data
   Presi.new[i,j] <- (y.new[i,j] - p[i,j])^2  # Calculate squared residual error of simulated data

  }
}
  SSEobs <- sum(Presi[,])     # Calculate the sum of squared residual errors for observed data
  SSEsim <- sum(Presi.new[,]) # Calculate the sum of squared residual error for the similuated data

  p.val <- step(SSEsim - SSEobs)

  ### Priors

  alpha0.psi ~ dnorm(0,1/2.25)
  alpha1.psi ~ dnorm(0,1/2.25)
  beta0.p ~ dnorm(0,1/2.25)
  beta1.p ~ dnorm(0,1/2.25)
  beta2.p ~ dnorm(0,1/2.25)
  beta3.p ~ dnorm(0,1/2.25)
  beta4.p ~ dnorm(0,1/2.25)

  occ.fs <- sum(z[])
  mean.p <- exp(beta0.p) / (1 + exp(beta0.p))

  }

  ", file="data_out/JagsOcc.txt"
)


# Make JAGS data list -----------------------------------------------------

#Gather Covariate data, edge will be 1 if it was on the edge and 0 if it is in the forest
edge <- bluebugs$forest_edge

datalist = list(
  y = y,         # Our response data
  R = nrow(y),   # Set our for-loop iterations. We want it to loop through the number of sites (rows)
  T = ncol(y),   # We want T to represent number of surveys (number of columns)
  edge = edge,   # Set our data for site covariates
  DATES = DATES, # Set our observation covariate data
  MINS = MINS
)

# Make initial values for the model

zst = apply(y,1,max,na.rm = TRUE) # Make a vector of zeros and ones for initial z-values
inits = function(){               # Make a function that returns a list of initial values
  list(
    z = zst,                      # Assign initial values for z
    alpha0.psi = runif(1,-3,3),    # Make initial value for alpha0.psi (occupancy)
    beta0.p = runif(1,-3,3)       # Make initial value for beta0.p (detection)
  )
}
inits()

# Tell JAGS what we want back:
paramsave = c("alpha0.psi", "alpha1.psi", "mean.p", "occ.fs", "beta0.p",
              "beta1.p", "beta2.p", "beta3.p", "beta4.p", "p.val", "SSEobs","SSEsim")



# Run JAGS Model ----------------------------------------------------------


# set.seed(2021)
out <- jags(data = datalist, parameters.to.save = paramsave, inits = inits,
           model.file = "JagsOcc.txt", n.chains = 3, n.adapt = 1000,
           n.iter = 30000, n.burnin = 20000,parallel = TRUE,n.thin = 10)

# convert and plot to check models
outMCMC <- out$samples #Convert output to MCMC object
outMCMCtrace <- MCMCtrace(outMCMC,params = c("alpha0.psi","alpha1.psi","beta0.p","beta1.p","beta2.p","beta3.p","beta4.p", "mean.p"), ISB = FALSE, pdf = F, exact = TRUE, post_zm = TRUE, type = 'trace', Rhat = TRUE, n.eff = TRUE)

# check convergence
outMCMCtrace <- MCMCtrace(outMCMC,params = c("alpha0.psi","alpha1.psi","beta0.p","beta1.p","beta2.p","beta3.p","beta4.p", "mean.p"), ISB = FALSE, pdf = F, exact = TRUE, post_zm = TRUE, type = 'density', Rhat = TRUE, n.eff = TRUE, ind = TRUE)

# use diagnostics
gelman.diag(outMCMC)

# check p-vals
plot(out$sims.list$SSEobs,out$sims.list$SSEsim,xlab="SSEobs",ylab="SSEsim")
abline(0,1,lwd = 2, col = "red")
out$mean$p.val #take the p-value out of the model summary

# get info out
out$summary[,c("mean","sd","2.5%","97.5%","Rhat")]

# estimate of the number of woodpiles likely to be occupied by species. We estimate occu.fs (total sites occupied based on latent state z produced by detection probability) in our jags model by summing the estimated Z values produced by each iteration. This in turn provides us with an estimate of how many of the sites surveyed likely were occupied. To look at the posterior distribution of the number of sites occupied, all you have to do is pull it out of the model output

# occu.fs = 14.55
hist(out$sims.list$occ.fs, nclass = 30, col = "red",
     xlab = "Number of Occupied Wood Piles", xlim = c(5,28), main = "Estimated Blue Bug Occupancy") # Plot out the posterior distribution of occu.fs.
abline(v = 10, lty = 2) # add an abline indicating how many piles were occupied based off surveys.

# compute a power statistic to determine how many surveys would be needed to improve detection and determine whether a site is occupied


# Calculate Power of Detection --------------------------------------------

# Create an empty storage array for storing detection probability
# Array should have same number of rows produced in MCMC model
# Array should have same number of cols as desired number of surveys
pstar = array(NA, dim = c(out$mcmc.info$n.samples,10))

# Set up x-values with same number of rows as your array (nrows of samples produced in the MCMC)
# These x-values will allow plotting of box values
x = cbind(rep(1, 3000), rep(2, 3000), rep(3, 3000),
          rep(4, 3000), rep(5, 3000), rep(6, 3000),
          rep(7, 3000), rep(8, 3000), rep(9, 3000),
          rep(10, 3000))

#Set up a for-loop that will run the same amount of iterations as the samples produced in the MCMC
for (i in 1:out$mcmc.info$n.samples) { # fills in data for each row
  for (j in 1:10){ # Fills in data for each column (i.e. number of columns)
    pstar[i,j] <- 1 - (1 - out$sims.list$mean.p[i])^j #Calculate estimated maximum detection probability for each survey using mean probability calculated in the MCMC
  }
}
#Plot the probability of detection for each survey, along with associated confidence intervals
boxplot(pstar ~ x, col = "red", las = 1, ylab = "Pstar",
        xlab = "Number of surveys", outline = FALSE, main = "Probability to Detect at least one Specimen")

#Add an abline that represents the 95% CI cutoff
abline(h = 0.95, lty = 2, lwd = 2)

# result: we want to survey each site at least six times to be 95% confident that site was unoccupied
hist(plogis(out$sims.list$alpha0.psi), nclass = 40, col = "green", main = "Forest interior", xlab = "Occupancy probability", xlim = c(0, 1))

hist(plogis(out$sims.list$alpha0.psi+ out$sims.list$alpha1.psi), nclass = 40, col = "chocolate", main = "Forest edge", xlab = "Occupancy probability", xlim = c(0, 1))


# Calculate Effect of Time & Date -----------------------------------------

# Predict effect of time of day with uncertainty
mcmc.sample = out$mcmc.info$n.samples

original.date.pred = seq(0, 60, length.out = 30)
original.mins.pred = seq(180, 540, length.out = 30)
date.pred = (original.date.pred - mean.date)/sd.date
mins.pred = (original.mins.pred - mean.mins)/sd.mins
p.pred.date = plogis(out$mean$beta0.p + out$mean$beta1.p * date.pred + out$mean$beta2.p * date.pred^2 )
p.pred.mins = plogis(out$mean$beta0.p + out$mean$beta3.p * mins.pred + out$mean$beta4.p * mins.pred^2 )

array.p.pred.mins <- array.p.pred.date <- array(NA, dim = c(length(mins.pred), mcmc.sample))
for (i in 1:mcmc.sample){
  array.p.pred.date[,i] = plogis(out$sims.list$beta0.p[i] + out$sims.list$beta1.p[i] * date.pred + out$sims.list$beta2.p[i] * date.pred^2)
  array.p.pred.mins[,i] = plogis(out$sims.list$beta0.p[i] + out$sims.list$beta3.p[i] * mins.pred + out$sims.list$beta4.p[i] * mins.pred^2)
}

# Plot for a subsample of MCMC draws for DATE
sub.set <- sort(sample(1:mcmc.sample, size = 200))

plot(original.date.pred, p.pred.date, main = "", ylab = "Detection probability", xlab = "Date (1 = 1 July)", ylim = c(0, 1), type = "l", lwd = 3, frame.plot = FALSE)
for (i in sub.set){
  lines(original.date.pred, array.p.pred.date[,i], type = "l", lwd = 1, col = "gray")
}
lines(original.date.pred, p.pred.date, type = "l", lwd = 3, col = "blue")

# PLOT FOR TIME
plot(original.mins.pred, p.pred.mins, main = "", ylab = "Detection probability", xlab = "Time of day (mins after noon)", ylim = c(0, 1), type = "l", lwd = 3, frame.plot = FALSE)
for (i in sub.set){
  lines(original.mins.pred, array.p.pred.mins[,i], type = "l", lwd = 1, col = "gray")
}
lines(original.mins.pred, p.pred.mins, type = "l", lwd = 3, col = "blue")
