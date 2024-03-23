# Has been moved to transport package (method wpp for generic unbalanced)
# keep a copy for time being
if (FALSE) {
unbalanced_wpp <- function(a, b, p = 1, C = 1/sqrt(2), output = c("dist", "all", "rawres"), threads=1) {
  stopifnot(is(a, "wpp") && is(b, "wpp"))
  if (a$dimension < 2) stop("dimension must be >=2")
  output <- match.arg(output)  # currently only "dist" functionality for everything 
  m <- a$N
  n <- b$N
  amass <- a$mass
  atotmass <- a$totmass
  bmass <- b$mass
  btotmass <- b$totmass
 
  if (m == 0 && n == 0) return(0)  # this line is not needed (if a is consistent), but makes things clearer
 
  if (m == 0) return(btotmass)  # atotmass = 0
    
  if (n == 0) return(atotmass)  # btotmass = 0
    
  costm <- transport:::gen_cost(a$coordinates, b$coordinates, threads=threads)^(p/2) 
  # could take pmin with 2*C^p, but not needed, let's see what is easier (since most probably the pmin is cheap)
  costm <- rbind(costm, C^p)  # we do *not* divide by two! (MSM not HKM)
  costm <- cbind(costm, C^p)
  costm[m+1, n+1] <- 0
  amassplus <- c(amass, btotmass)  
  bmassplus <- c(bmass, atotmass)

  rawres <- transport:::networkflow(matrix(amassplus), matrix(bmassplus), costm, threads=threads) # m x 1 and n x 1 matrices for the masses 
  # sanity check (not sure if already performed inside networkflow)
  primalcost <- sum(costm * rawres$plan)   
  dualcost <- sum(rawres$potential * c(amassplus, bmassplus))
  if (!isTRUE(all.equal(primalcost, dualcost)) || !isTRUE(all.equal(primalcost, rawres$dist))) {  # dist is cost (dist^p)
    warning("Primal-dual gap is ", rawres$dist - dualcost, "\n", "Primal cost: ", primalcost, 
            "; dual cost: ", dualcost, "; rawres$cost: ", rawres$dist)
  }
  # rawres
  
  return(rawres$dist^(1/p)) # recall that rawres$dist is actually the p-th power of the unbalanced Wasserstein dist
}
}


