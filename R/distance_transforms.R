# Various distance transforms [0,1] --> [0,1] to account for psychological effects of (especially)
# high proportions of matching mass when comparing the components

# The parameter CC is a priori not the same as the C-parameter in the kanjidist call!
# C is the cutoff from the (rtt / unbalanced Wasserstein) distance between components
# CC refers to the scaling function in the distance transform
# In the paper CC = b = 2^(1/p) * C = 0.4 was advocated as the reasonable choice, however,
# already the first kanjidist used actually CC=C=0.2 (for p=1). This was originally due to 
# a typo but turned out to work slightly better as distances were generally smaller than expected. 

# In newer versions of the kanjidist we choose CC freely, and rather try to ditch
# the asymmetry parameter (p0 or, in betacdf, b).

# Naming convection: anything with 01 at the end is the (inverse of the) classic link function given by
# the first part of the name, but defined on (0,1), by plugging in a logit-transform

# The original choice in the 2023 paper, for reproducibility (including the name logi2C)
logi2C <- logit01 <- function(q, a=2, p0=0.4, CC=0.2) {  
  ptemp <- q/CC
  p <- pmax(0,pmin(1,ptemp))
  # if (any(abs(p-ptemp) > 1e-6)) warning("q = ", q[abs(p-ptemp) > 1e-6], " is substantially out of range for logi2C") 
  1/(1+((p0/(1-p0))*(1-p)/p)^a)
}

betacdf <- function(q, a=4, b=a, CC=0.2) {
  ptemp <- q/CC
  p <- pmax(0,pmin(1,ptemp))  # not necessary here; just to be able to do the check in the next line
  # if (any(abs(p-ptemp) > 1e-6)) warning("q = ", q[abs(p-ptemp) > 1e-6], " is substantially out of range for betacdf") 
  pbeta(p, a, b)
}
# currently preferred
  
probit01 <- function(q, a=1, p0=0.5, CC=0.2) {  
  ptemp <- q/CC
  p <- pmax(0,pmin(1,ptemp))  
  # if (any(abs(p-ptemp) > 1e-6)) warning("q = ", q[abs(p-ptemp) > 1e-6], " is substantially out of range for probit01") 
  pnorm(a*log(((1-p0)/p0)*p/(1-p)))
}
# this is very close to beta-link if a=b approx 2.5; so probably not worth pursuing

cloglog01 <- function(q, a=1,  p0=0.5, CC=0.2) {  
  ptemp <- q/CC
  p <- pmax(0,pmin(1,ptemp))
  # if (any(abs(p-ptemp) > 1e-6)) warning("q = ", q[abs(p-ptemp) > 1e-6], " is substantially out of range for cloglog01") 
  exp(-((p0/(1-p0))*(1-p)/p)^a)
}
# asymmetric by nature
