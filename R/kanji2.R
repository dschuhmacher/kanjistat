# Functions in this file operate on pairs of kanji (typical example distance/similarity functions).


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Functions for kanjimat objects
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Compute the unbalanced or balanced Wasserstein distance between two kanjimat objects
#' 
#' This gives the dissimilarity of pixel-images of the kanji based on how far mass (or "ink") has to be
#' transported to transform one image into the other.
#'
#' @param k1,k2 two objects of type \code{kanjimat}.
#' @param p the order of the Wasserstein distance. All distances and a potential penalty are taken
#' to the \code{p}-th power (which is compensated by taking the \code{p}-th root after summation).
#' @param C the penalty for extra mass if \code{type="unbalanced"}, i.e. we add  \code{C^p} per unit
#' of extra mass (before applying the \code{p}-th root).
#' @param type the type of Wasserstein metric. \code{"unbalanced"} means the pixel values in the two images are
#' interpreted as mass. The total masses can be very different. Extra mass can be disposed of at cost \code{C^p}
#' per unit. \code{"balanced"} means the pixel values are normalized so that both images have the same total
#' mass 1. Everything has to be transported, i.e. disposal of mass is not allowed.
#' @param output the requested output. See return value below.
#'
#' @return If \code{output = "dist"}, a single non-negative number: the unbalanced or balanced Wasserstein
#' distance between the kanji. If \code{output = "all"} a list with detailed information on the transport plan
#' and the disposal of pixel mass. See \code{\link[transport]{unbalanced}} for details. 
#' @export
#'
#' @seealso \code{\link{kmatdistmat}}, \code{\link{kanjidist}}
#'
#' @examples
#' res <- kmatdist(fivetrees1[[1]], fivetrees1[[5]], p=1, C=0.1, output="all")
#' plot(res, what="plan", angle=20, lwd=1.5) 
#' plot(res, what="trans")
#' plot(res, what="extra")
#' plot(res, what="inplace")
#' 
kmatdist <- function(k1, k2, p=1, C=0.2, type=c("unbalanced", "balanced"), output=c("dist","all")) {
  stopifnot(is(k1, "kanjimat") && is(k2, "kanjimat"))
  type <- match.arg(type)
  output <- match.arg(output)
  
  if (type == "unbalanced") {
    a <- transport::pgrid(k1$matrix)
    b <- transport::pgrid(k2$matrix)
    res <- transport::unbalanced(a, b, p=p, C=C, output=output)
  } else {
    a <- transport::pgrid(k1$matrix/sum(k1$matrix))
    b <- transport::pgrid(k2$matrix/sum(k2$matrix))
    res <- transport::unbalanced(a, b, p=p, output=output)
       # essentially the same as transport::transport.pgrid, but the latter does not directly return the dist
       # and the output is in a bit a different format
  }
  return(res)
}

#' Compute distance matrix for lists of kanjimat objects 
#' 
#' Apply \code{\link{kmatdist}} to every pair of \code{\link{kanjimat}} objects to compute
#' the unbalanced or balanced Wasserstein distance.
#'
#' @param klist a list of \code{\link{kanjimat}} objects.
#' @param klist2 an optional second list of \code{\link{kanjimat}} objects.
#' @param p,C,type the same as for the function \code{\link{kmatdist}}.
#'
#' @return A matrix of dimension \code{length(klist)} x \code{length(klist2)} having
#' as its \eqn{(i,j)}-th entry the distance between \code{klist[[i]]} and
#' \code{klist2[[j]]}. If \code{klist2} is not provided it is assumed to be equal to
#' \code{klist}, but the computation is more efficient as only the upper triangular part
#' is computed and then symmetrized with diagonal zero.
#' @export
#'
#' @seealso \code{\link{kmatdist}}, \code{\link{kanjidistmat}}
#'
#' @examples
#' kmatdistmat(fivetrees1)
#' \donttest{kmatdistmat(fivetrees1, fivetrees1)  # same result but slower
#' kmatdistmat(fivetrees1, fivetrees2)  # note the smaller values on the diagonal
#' }
#' 
kmatdistmat <- function(klist, klist2=NULL, p=1, C=0.2, type=c("unbalanced", "balanced")) {
  stopifnot( is.list(klist) )
  stopifnot( all(sapply(klist, \(x) is(x, "kanjimat"))) )
  type <- match.arg(type)
  n <- length(klist)
  
  if (is.null(klist2)) {
    temp <- combn(n,2)
    ll <- lapply(1:dim(temp)[2], \(x) {temp[,x]})
    dd <- sapply(ll, \(x) {kmatdist(klist[[x[1]]], klist[[x[2]]], p=p, C=C, type=type, output="dist")})
    dmat <- matrix(0, n, n)
    dmat[t(temp)] <- dd
    dmat <- dmat + t(dmat)
  } else {
    stopifnot( is.list(klist2) )
    stopifnot( all(sapply(klist2, \(x) is(x, "kanjimat"))) )
    n2 <- length(klist2)
    N <- n*n2
    temp <- arrayInd(1:N, c(n, n2))
    ll <- lapply(1:N, \(x) {temp[x,]})
    dd <- sapply(ll, \(x) {kmatdist(klist[[x[1]]], klist2[[x[2]]], p=p, C=C, type=type, output="dist")})
    dmat <- matrix(dd, n, n2)
  }
  
  return(dmat)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Functions for kanjivec objects
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#' Compute distance between two kanjivec objects based on hierarchical optimal transport
#'
#' The kanji distance is based on matching hierarchical component structures in a
#' nesting-free way across all levels. The cost for matching individual components is a
#' cost for registering the components (i.e. alligning there position, scale and aspect
#' ratio) plus the (relative unbalanced) Wasserstein distance between the registered components.
#'
#' @param k1,k2 two objects of type \code{kanjivec}. 
#' @param compo_seg_depth1,compo_seg_depth2 two integers \eqn{\geq 1}. Specifies for each kanji the 
#' deepest level included for component matching. If 1, only the kanji itself is used.
#' @param p the order of the Wasserstein distance used for matching components. All distances and
#' the penalty (if any) are taken
#' to the \code{p}-th power (which is compensated by taking the \code{p}-th root after summation).
#' @param C the penalty for extra mass if \code{type} is \code{"rtt"} or \code{"unbalanced"}, i.e.
#' we add  \code{C^p} per unit of extra mass (before applying the \code{p}-th root).
#' @param approx what kind of approximation is used for matching components. If this is \code{"grid"}, 
#' a bitmap (raster image) is used, otherwise lines are approximated by more freely spaced points.
#' For \code{"pc"} (point cloud) each point has the same weight and points are placed in a (more or
#' less) equidistant way. For \code{"pcweighted"} points are further apart along straight lines and
#' around the center of the Bezier curves that describe the strokes. The weights of the points are
#' then (more or less) proportional to the amount of ink (stroke length) they represent.
#' @param type the type of Wasserstein distance used for matching components based on the grid or
#' point cloud approximation chosen. \code{"unbalanced"} means the weights (pixel values
#' if `approx = "grid`) are interpreted as mass. The total masses in two components be very different.
#' Extra mass can be disposed of at cost \code{C^p} per unit. \code{"rtt"} is computationally the same,
#' but the final distance is divided by the maximum of the total ink (sum of weights) in each component
#' to the 1/p. \code{"balanced"} means the weights are normalized so that both images have the same
#' total mass 1. Everything has to be transported, i.e.\ disposal of mass is not allowed. 
#' @param size side length of the bitmaps used for matching components.
#' @param lwd  linewidth for drawing the components in these bitmaps.
#' @param verbose logical. Whether to print detailed information on the cost for all pairs of
#' components and the final matching.  
#'
#' @details For the precise definition and details see the reference below. Parameter \code{C}
#' corresponds to \eqn{b/2^{1/p}} in the paper.
#'
#' @section Warning:
#'
#' `r lifecycle::badge("experimental")`\cr
#' The interface and details of this function will change in the future. Currently only a minimal
#' set of parameters can be passed. The other parameters are fixed exactly as in the
#' "prototype distance" (4.1) of the reference below for better or worse.\cr 
#' There is a certain
#' tendency that exact matches of components are rather strongly favored (if the KanjiVG elements
#' agree this can overrule the unbalanced Wasserstein distance) and the penalties for
#' translation/scaling/distortion of components are somewhat mild.\cr
#' The computation time is rather high (depending on the settings and kanji up to several
#' seconds per kanji pair). This can be alleviated somewhat by keeping the \code{compo_seg_depth}
#' parameters at 3 or lower and setting \code{size = 32} (which goes well with \code{lwd=1.8}).\cr
#' Future versions will use a much faster line base optimal transport algorithm and further 
#' speed-ups.
#' 
#' @references Dominic Schuhmacher (2023).\cr
#'             Distance maps between Japanese kanji characters based on hierarchical optimal transport.\cr
#'             ArXiv, \doi{10.48550/arXiv.2304.02493}
#'
#' @return The kanji distance, a non-negative number.
#' @export
#' 
#' @seealso \code{\link{kanjidistmat}}, \code{\link{kmatdist}}
#'
#' @examples
#' if (requireNamespace("ROI.plugin.glpk")) {
#'   kanjidist(fivebetas[[4]], fivebetas[[5]])
#'   \donttest{kanjidist(fivebetas[[4]], fivebetas[[5]], verbose=TRUE)}
#'   # faster and similar:
#'   kanjidist(fivebetas[[4]], fivebetas[[5]], compo_seg_depth1=2, compo_seg_depth2=2, 
#'             size=32, lwd=1.8, verbose=TRUE) 
#'   # slower and similar:
#'   \donttest{kanjidist(fivebetas[[4]], fivebetas[[5]], size=64, lwd=3.2, verbose=TRUE)}
#' } 
kanjidist <- function(k1, k2, compo_seg_depth1=3, compo_seg_depth2=3, p=1, C=0.2,
                      approx=c("grid", "pc", "pcweighted"), type=c("rtt", "unbalanced", "balanced"),
                      size=48, lwd=2.5, verbose=FALSE) {
  stopifnot(is(k1, "kanjivec") && is(k2, "kanjivec"))
  type <- match.arg(type)
  if (k1$char == k2$char) return(0) 
  
  logi2C <- function(q, a=2, p0=0.5, CC=0.2) {  
    ptemp <- q/CC
    p <- pmax(0,pmin(1,ptemp))
    if (any(abs(p-ptemp) > 1e-6)) warning("q = ", q[abs(p-ptemp) > 1e-6], " is substantially out of range for logi2C") 
    1/(1+((p0/(1-p0))*(1-p)/p)^a)
  }

  level0fact <- 1    # fudge factor for the optimal transport on the toplevel (probably not needed anymore)
  useele <- TRUE     # if true the element attribute in kanjivec is used and dist is set to 1e-6 if it is a match. 
    # Currently this uses only a strict comparison, e.g. the "left and right betas" are not the same.
  exmatchdist <- 0.06  # dist below this value (for 2 components) is considered "excellent".
    # Currently the only effect is a warning if the kanjiVG elements match, but the rtt distance is larger than
    # exmatchdist (if it is much larger there is usually a good reason to take the warning seriously).
    # rtt distance = the unbalanced bounded Wasserstein distance *without* division by b resp. CC,
  unmatchedcost <- 0.25 # weight mass that is not matched contributes this to overall cost (a in the paper)
  trickleloss <- 0.02   # in [0,1), epsilon in the paper.
  distfact <- 0.8       # lambda_0 in the paper
  transfact <- 0.1      # lambda_1
  scalefact <- 0.05     # lambda_2
  distortfact <- 0.05   # lambda_3
  # values for the lambdas are pretty ad hoc for now and should be ultimately estimated based 
  # on data that is suitable for the task we want kanjidist to fulfill.
  stopifnot(isTRUE(all.equal(distfact+transfact+scalefact+distortfact,1)))
  
  allcosts <- all_compcosts(k1=k1, k2=k2,
                            compo_seg_depth1=compo_seg_depth1, compo_seg_depth2=compo_seg_depth2,
                            p=p, C=C, approx=approx, type=type,
                            size=size, lwd=lwd, precompute=FALSE)

  lseq1 <- seq_len(min(compo_seg_depth1, length(k1$ncompos)))
  lseq2 <- seq_len(min(compo_seg_depth2, length(k2$ncompos)))
  
  ncompos1 <- k1$ncompos[lseq1]
  ncompos2 <- k2$ncompos[lseq2]
  compos1 <- k1$components[lseq1]
  compos2 <- k2$components[lseq2]
  
  # compos1, compos2 contains "unreal" components, which are components copied from higher levels
  # that do not decompose further (in the language of the paper these are just components as well)
  # Here we don't want them, as they do not contribute anything to the component matching
  # (there higher-level original creates the same matches, that usually help even more due to trickleloss),
  # so we have to find the coordinates of the real components
  get_real <- attr_getter("real")
  realminor1 <- lapply(compos1, \(x) which(sapply(x, get_real)))
  realmajor1 <- lapply(seq_along(realminor1), \(x) {rep(x, length(realminor1[[x]]))})
  realminor2 <- lapply(compos2, \(x) which(sapply(x, get_real)))
  realmajor2 <- lapply(seq_along(realminor2), \(x) {rep(x, length(realminor2[[x]]))})
  
  # first determine weights for the whole component list (including unreal components)
  # then pick only real compos
  weights1 <- compoweights_ink(k1, relative = TRUE, trickleloss = trickleloss)$compos  
  weights1 <- weights1[lseq1]
  w1 <- lapply(seq_along(weights1), \(x) {weights1[[x]][realminor1[[x]]]})
  w1 <- unlist(w1)
  weights2 <- compoweights_ink(k2, relative = TRUE, trickleloss = trickleloss)$compos
  weights2 <- weights2[lseq2]
  w2 <- lapply(seq_along(weights2), \(x) {weights2[[x]][realminor2[[x]]]})
  w2 <- unlist(w2)
  
  flatcompos1 <- lapply(seq_along(compos1), \(x) {compos1[[x]][realminor1[[x]]]})
  flatcompos1 <- list_flatten(flatcompos1)
  flatcompos2 <- lapply(seq_along(compos2), \(x) {compos2[[x]][realminor2[[x]]]})
  flatcompos2 <- list_flatten(flatcompos2)
  n1 <- length(flatcompos1)
  n2 <- length(flatcompos2)
  stopifnot(n1 == length(allcosts))
  stopifnot(all(n2 == sapply(allcosts, "length")))
  
  flatminor1 <- unlist(realminor1)
  flatmajor1 <- unlist(realmajor1)
  flatminor2 <- unlist(realminor2)
  flatmajor2 <- unlist(realmajor2)
  
  compores <- list() # saves for each combination of components all sorts of information
                     # for the computation of totcost (this is for diagnostic purposes, displayed if verbose=TRUE)
  costmat <- matrix(NA, n1, n2)  # the totcost for each combination
  for (r1 in seq_len(n1)) {
  for (r2 in seq_len(n2)) {
    l1 <- flatmajor1[r1]
    i1 <- flatminor1[r1]
    l2 <- flatmajor2[r2]
    i2 <- flatminor2[r2]
    dist <- chuck(allcosts, r1, r2, "dist")
    ele1 <- chuck(allcosts, r1, r2, "elements", 1)  
    ele1 <- str_sub(ele1, 1, 1)
    ele2 <- chuck(allcosts, r1, r2, "elements", 2)
    ele2 <- str_sub(ele2, 1, 1)
    if (useele && ele1 == ele2 && ele1 != "g" && ele1 != "") { # not sure any more if == in the last one may happen
      if (dist > exmatchdist) warning("elements in kanjiVG data are the same, but dist of bitmaps is substanial for ", ele1, " and ", ele2)
      dist <- 0.000001  
    }
        
    dist <- min(C, dist) # recall that dist is only guaranteed to be <= 2^(1/p)*C
                         # practically it is even for p=1 only in relatively extreme cases >C
        
    reltrans <- chuck(allcosts, r1, r2, "reltrans")
    scar <- chuck(allcosts, r1, r2, "scar")
    distort <- chuck(allcosts, r1, r2, "distort")
    transerror <- sqrt(reltrans[1]^2 + reltrans[2]^2)
    scaleerror <- sqrt(scar[1] * scar[2])   # geom. mean
    distorterror <- distort[1]/distort[2]   # since we take abs log below the order of numerator and denom. is not important
    if (r1 == 1 && r2 == 1) {
      totcost <- logi2C(dist, a=2, p0=0.4, CC=C)  
    } else {
      totcost <- distfact * logi2C(dist, a=2, p0=0.4, CC=C) + transfact * transerror + scalefact * abs(log(scaleerror)) + distortfact * abs(log(distorterror))
    }
    # Formula (4.1) in the paper corresponds exactly to what we are doing here.
    # However:
    # If we follow (3.2) in the paper, we should have C=b/2^(1/p) and CC=b, i.e. CC = 2^(1/p) * C.
    # I missed this when changing b^p to b^p/2 in (3.1), so we still use CC = C here.
    # Since this give logi2C (psi in the paper) overall a somewhat nicer shape and since
    # all the results in the paper have been computed like this, I leave it for now
    # (also for general p, where the effect is smaller).
    
    costmat[r1,r2] <- totcost
    pluck(compores, r1, r2) <- c(flatind=c(r1,r2), level1=l1, ind1=i1, level2=l2, ind2=i2, totcost=totcost, dist=dist, weight1=w1[r1], weight2=w2[r2],
                                      transx=reltrans[1], transy=reltrans[2], scalex=scar[1], scaley=scar[2],
                                      distortx=distort[1], distorty=distort[2])
    attr(pluck(compores, r1, r2), "elements") <- c(ele1, ele2)
  }
  }
  
  # veins in terms of unlisted real components
  skele <- lapply(compos1, \(x) {lapply(x, \(y) {1})})
  flatreal <- sapply(list_flatten(compos1), get_real)
  nreal <- sum(flatreal)
  flatind <- flatreal
  flatind[flatreal] <- seq_len(nreal)
  flatind[!flatreal] <- NA
  indlist1 <- relist(flatind, skele)
  #
  skele <- lapply(compos2, \(x) {lapply(x, \(y) {1})})
  flatreal <- sapply(list_flatten(compos2), get_real)
  nreal <- sum(flatreal)
  flatind <- flatreal
  flatind[flatreal] <- seq_len(nreal)
  flatind[!flatreal] <- NA
  indlist2 <- relist(flatind, skele)
  #
  vein2flatind <- function(v, indlist) {
    temp <- lapply(seq_len(nrow(v)), \(x) v[x,])
    sapply(temp, \(x) chuck(indlist, !!! x))
  }
  # due to cutting of via lseq1 (or even before?) we might have several copies of the same (shortened) veins
  veins1 <- unique(lapply(k1$veins, \(x) {x[intersect(lseq1, seq_len(nrow(x))),,drop=FALSE]}))  
  veins2 <- unique(lapply(k2$veins, \(x) {x[intersect(lseq2, seq_len(nrow(x))),,drop=FALSE]})) 
  flatveins1 <- lapply(veins1, vein2flatind, indlist1)
  flatveins2 <- lapply(veins2, vein2flatind, indlist2)
  
  wmat <- outer(w1, w2, pmin)
  # wmat <- outer(w1, w2, \(X,Y) {2/(1/X + 1/Y)})  # harmonic mean instead of min
  # wmat <- outer(w1, w2, \(X,Y) {sqrt(X*Y)})  # geometric mean instead of min
  # wmat <- outer(w1, w2, \(X,Y) {(X+Y)/2})    # arithmetic mean instead of min
  # under arithmetic mean total sum of weights is 1 again, but arithm. mean seems
  # be too large for very unbalanced matches:
  # One compo barely visible, the other the full kanji --> weigth approx 1/2

  res <- fullmin_transport(wmat, costmat, flatveins1, flatveins2, unmatchedcost)
  
  solind <- which(res$solution==1, arr.ind = TRUE)
  solfrom <- cbind(flatmajor1[solind[,1]], flatminor1[solind[,1]])
  solto <- cbind(flatmajor2[solind[,2]], flatminor2[solind[,2]])
  labfrom <- sapply(seq_len(nrow(solfrom)), \(x) pluck(compos1, !!!solfrom[x,])$label)
  labto <- sapply(seq_len(nrow(solto)), \(x) pluck(compos2, !!!solto[x,])$label)
  costs <- costmat[solind]
  masses <- wmat[solind] 
  masses1 <- w1[solind[,1]]
  masses2 <- w2[solind[,2]]
  restmass <- (1-sum(masses))
  checkdist <- sum(costs*masses) + unmatchedcost*restmass
  
  if (verbose) {
    print(compores)
    cat("Overview of matches:\n\n")
    print(data.frame(labfrom, labto, costs, masses, masses1, masses2))
    cat("Unmatched at cost ", unmatchedcost, ": ", restmass, "\n\n", sep="")
    message("Directly computed total cost based on this overview: ", checkdist)
  }
  
  retval <- res$objval
  attr(retval, "solution") <- res$solution
  return(res$objval)
}



#' Compute distance matrix based on hierarchical optimal transport for lists of kanjivec objects 
#' 
#' Individual distances are based on \code{\link{kanjidist}}.
#' 
#' @param klist a list of \code{\link{kanjimat}} objects.
#' @param klist2 an optional second list of \code{\link{kanjimat}} objects.
#' @param compo_seg_depth integer \eqn{\geq 1}. Specifies for all kanji the 
#' deepest level included for component matching. If 1, only the kanji itself is used.
#' @param p,C,type,approx,size,lwd,verbose the same as for the function \code{\link{kanjidist}}.
#'
#' @section Warning:
#'
#' `r lifecycle::badge("experimental")`\cr
#' The same precautions apply as for \code{\link{kanjidist}}.
#'
#' @return A matrix of dimension \code{length(klist)} x \code{length(klist2)} having
#' as its \eqn{(i,j)}-th entry the distance between \code{klist[[i]]} and
#' \code{klist2[[j]]}. If \code{klist2} is not provided it is assumed to be equal to
#' \code{klist}, but computation is more efficient as only the upper triangular part
#' is computed and then symmetrized with diagonal zero.
#' @export
#'
#' @seealso \code{\link{kanjidist}}, \code{\link{kmatdistmat}}
#'
#' @examples
#' \donttest{
#' kanjidistmat(fivebetas)
#' }
kanjidistmat <- function(klist, klist2=NULL, compo_seg_depth=3, p=1, C=0.2,
                  approx=c("grid", "pc", "pcweighted"), type=c("rtt", "unbalanced", "balanced"),
                  size=48, lwd=2.5, verbose=FALSE) {
  stopifnot( is.list(klist) )
  stopifnot( all(sapply(klist, \(x) is(x, "kanjivec"))) )
  type <- match.arg(type)
  n <- length(klist)
  
  if (is.null(klist2)) {
    temp <- combn(n,2)
    ll <- lapply(1:dim(temp)[2], \(x) {temp[,x]})
    dd <- sapply(ll, \(x) {kanjidist(klist[[x[1]]], klist[[x[2]]],
                                     compo_seg_depth1=compo_seg_depth, compo_seg_depth2=compo_seg_depth,
                                     p=p, C=C, type=type, size=size, lwd=lwd, verbose=verbose)})
    dmat <- matrix(0, n, n)
    dmat[t(temp)] <- dd
    dmat <- dmat + t(dmat)
  } else {
    stopifnot( is.list(klist2) )
    stopifnot( all(sapply(klist2, \(x) is(x, "kanjivec"))) )
    n2 <- length(klist2)
    N <- n*n2
    temp <- arrayInd(1:N, c(n, n2))
    ll <- lapply(1:N, \(x) {temp[x,]})
    dd <- sapply(ll, \(x) {kanjidist(klist[[x[1]]], klist2[[x[2]]],
                                     compo_seg_depth1=compo_seg_depth, compo_seg_depth2=compo_seg_depth,
                                     p=p, C=C, approx=approx, type=type, size=size, lwd=lwd, verbose=verbose)})
    dmat <- matrix(dd, n, n2)
  }
  
  return(dmat)
}



# Use of glpk here may be inconvenient to some users as they have to install it first.
# Check if we can use lpsolve or something similar (typically binary via integer with 0-1 bounds)
fullmin_transport <- function(wmat, costmat, flatveins1, flatveins2, unmatchedcost) {
  if (!requireNamespace("ROI.plugin.glpk", quietly = TRUE)) {
    stop("package 'ROI.plugin.glpk' is not available. This function requires an installation of 
          the GNU Linear Programming Kit, as well as the R packages 'Rglpk' and 'ROI.plugin.glpk'.")
  }
  glpk_signature <- ROI::ROI_solver_signature("glpk")
  
  m <- nrow(wmat)
  n <- ncol(wmat)
  stopifnot(all(dim(costmat) == c(m,n)))
  vm <- length(flatveins1)
  vn <- length(flatveins2)
  # since it goes better with the col.major order of R, we use the linearized real nodes of compo1
  # as secondary(!) and the linearized real nodes of compo2 as primary(!) order criterion
  constr1 <- sapply(flatveins1, \(x) {constraint1 <- rep(0, m)
                                      constraint1[x] <- 1
                                      rep(constraint1, times=n)
                                     })
  constr2 <- sapply(flatveins2, \(x) {constraint2 <- rep(0, n)
                                      constraint2[x] <- 1
                                      rep(constraint2, each=m)
                                     })
  A <- t(cbind(constr1, constr2))
  b <- rep(1, vm+vn)
  # ub <- as.numeric(outer(w1, w2, pmin))
  
  lp <- ROI::OP(objective = as.numeric((costmat-unmatchedcost) * wmat),  # row major (matches the order in cols of A)
                constraints = ROI::L_constraint(L = A, dir = rep("<=", vm+vn), rhs = b),
                types = rep("B", m*n), 
                # bounds = V_bound(lb = rep(0, m*n), ub = ub),
                maximum = FALSE)
  lp_sol <- ROI::ROI_solve(lp, "glpk")
  
  if (lp_sol$status$code != 0) {
    warning("solver reports problems")
    print(lp_sol$status)
  }
  
  return(list(objval=lp_sol$objval+unmatchedcost, solution=matrix(lp_sol$solution, m, n)))
}


# Compute component weights based on the the total stroke length
# trickle loss (tl) specifies the ratio of weight that is lost from one compo level to the next, 
# i.e. sum of weights will be 1, 1, 1-tl, (1-tl)^2, a.s.o. on
# (no loss from kanji to first compo level since kanji are treated specially anyway)
# (needed for kanjidist)
compoweights_ink <- function(kanji, compo_seg_depth=4, relative=TRUE, trickleloss=0) {
  ncompos <- kanji$ncompos
  compos <- kanji$components
  get_real <- attr_getter("strokenums")
  
  all_strokes <- get_strokes_compo(kanji)
  all_lengths <- sapply(all_strokes, strokelength)
  totlength <- sum(all_lengths)
  nstrokes <- kanji$nstrokes
  
  divisor <- ifelse(relative, totlength, 1)
  
  compo_lengths <- list()
  for (l in seq_len(min(compo_seg_depth, length(ncompos)))) {
    tll <- ifelse(l==1, 1, (1-trickleloss)^(l-2))
    compo_lengths[[l]] <- list()
    all_strokenums <- vector("list", ncompos[l])
    for (i in seq_len(ncompos[l])) {
      compo_strokes <- get_strokes_compo(kanji, which = c(l,i))
      compo_length <- sum(sapply(compo_strokes, strokelength))/divisor * tll
      pluck(compo_lengths, l, i) <- compo_length
      strokenums <- as.numeric(names(compo_strokes))
      all_strokenums[[i]] <- strokenums
      attr( pluck(compo_lengths, l, i), "strokes" ) <- strokenums
    }
    counttab <- as.numeric(table(factor(unlist(all_strokenums), levels=seq_len(nstrokes))))
    
    if (!isTRUE(all.equal(counttab, rep(1, nstrokes)))) {
      if (relative && any(counttab == 0)) {
        warning("Stroke(s) ", paste(which(counttab == 0)), " missing from component decomposition at level ", l,
                " (where 1 = full kanji) for kanji ", kanji$char, ". As a cheap workaround the sum of the ",
                "remaining stroke weights is renormalized to 1. This will be dealt with more judiciously ",
                "in a future version of the package.")
        considered_strokes <- which(counttab != 0)
        renorm_fact <- sum(all_lengths[considered_strokes])/divisor * tll
        for (i in seq_len(ncompos[l])) {
          pluck(compo_lengths, l, i) <- pluck(compo_lengths, l, i) / renorm_fact
          # attr( pluck(compo_lengths, l, i), "strokes" ) <- all_strokenums[[i]]
        }
      } else {
        for (i in seq_len(ncompos[l])) {
          strokelength_split <- all_lengths[ all_strokenums[[i]] ] / counttab[ all_strokenums[[i]] ]
          pluck(compo_lengths, l, i) <- sum(strokelength_split)/divisor * tll
          attr( pluck(compo_lengths, l, i), "strokes" ) <- all_strokenums[[i]]
        }
      }
    }
  }
  
  return(list(compos=compo_lengths, leaves=all_lengths/divisor))
}


# Compute all relevant subcosts for all pairwise comparisons of components.
# The parameter precomputed was introduced, because at some point there was hope that it makes things faster
# If precompute=TRUE, all the bitmaps and the underlying translation/scaling/distortion data are precomputed
# (otherwise bitmaps are regenerated for each pairwise comparison). However, the gain from this is tiny and
# it creates other problems (essentially because translation/scaling/distortion cannot be done in a
# dedicated way for each comparison anymore). The parameter precompute and the corresponding code will 
# probably be removed in a future version.
# In the long run, by far the best thing to speed up computation, would be to move away from the pixel grid
# optimal transport, and use a weighted point pattern based optimal transport (based directly on the
# stroke matrices in the kanjivec objects). The simplest way would be to assign weights according to the
# segment length each point stands for (i.e. dist from midpoint with previous and next point).
# IF the endpoints get somewhat more weight, this would not be undesirable. Also e.g. how the match
# of points is different from identity could be relevant (components that are essentially the same, would
# have same stroke order *and* all the points of drawing it would be in the same order; i.e. if assignment
# is not close to identity, there could be an additional penalty, say)
all_compcosts <- function(k1, k2, compo_seg_depth1=4, compo_seg_depth2=4, p=1, C=0.2,
                          approx=c("grid", "pc", "pcweighted"), type=c("rtt", "unbalanced", "balanced"), 
                          size=48, lwd=2.5, precompute=FALSE) {
  #recommended combinations:
  # size=32, lwd=1.8 (noticable loss in quality, but still ok)
  # size=48, lwd=2.5
  # size=64, lwd=3.2
  # size=128, lwd=5.6 (if you must; this is very slow)
  approx <- match.arg(approx)
  type <- match.arg(type)

  ncompos1 <- k1$ncompos
  ncompos2 <- k2$ncompos
  compos1 <- k1$components
  compos2 <- k2$components
  get_real <- attr_getter("real")
  get_original <- attr_getter("original")

  if (precompute) { 
    bitstats1 <- kcompos_to_bitmaps(k1, compo_seg_depth=compo_seg_depth1, size=size, lwd=lwd)
    bitstats2 <- kcompos_to_bitmaps(k2, compo_seg_depth=compo_seg_depth2, size=size, lwd=lwd)
  }
    
  res <- list()  # two level list; first level corresponds to first kanji, second to second kanji
                 # enumeration of components is breadth first (as in $components)
  
  l1max <- min(length(ncompos1), compo_seg_depth1)
  l2max <- min(length(ncompos2), compo_seg_depth2)
  r1 <- 0
  for (l1 in seq_len(l1max)) {
  for (i1 in seq_len(ncompos1[l1])) {
  if (chuck(compos1, l1, i1, get_real)) { 
    r1 <- r1 + 1
    res[[r1]] <- list()
    r2 <- 0
    for (l2 in seq_len(l2max)) {
    for (i2 in seq_len(ncompos2[l2])) {
    if (chuck(compos2, l2, i2, get_real)) {
      r2 <- r2 + 1 
      if (precompute) {
        temp <- component_cost_prec(bitstats1, bitstats2, which1=c(l1, i1), which2=c(l2, i2), p=p, C=C, type=type, output = "distplus")
      } else {
        temp <- component_cost(k1, k2, which1=c(l1, i1), which2=c(l2, i2), size=size, lwd=lwd,
                               p=p, C=C, approx=approx, type=type, output = "distplus")
      }
      pluck(res, r1, r2) <- temp
    }
    }
    }
  }
  }
  }

  return(res)
}
#transport::matimage(strokelist_to_bitmap(get_strokes(kvec[[1779]]), size=32, lwd=1.8))
#transport::matimage(strokelist_to_bitmap(get_strokes(kvec[[1779]]), size=48, lwd=2.5))
#transport::matimage(strokelist_to_bitmap(get_strokes(kvec[[1779]]), size=64, lwd=3.2))
#transport::matimage(strokelist_to_bitmap(get_strokes(kvec[[1779]]), size=128, lwd=5.6))
# strokes will be 0.75*lwd pixels thick (always), so if we look at the different sizes using
# a fixed-size window (e.g. matimage-plot of the matrix), the lwds should be exactly proportional
# to the size parameters. This is not what I propose above, because (within reason) thicker font
# helps a bit to preserve features in bad resolutions (i.e. small sizes)
# [for this question unrelated: on most devices (including png) lwd=1 corresponds 1/96 inch]



# the plus in distplus is for total ink of the two kanji and their shift vector, scaling and
# distortion rates.
component_cost <- function(k1, k2, which1=c(1,1), which2=c(1,1), size=48, lwd=2.5, p=1, C=0.2,
                           approx=c("grid", "pc", "pcweighted"), type=c("rtt", "unbalanced", "balanced"),
                           output=c("distplus","all")) {
  approx <- match.arg(approx)
  type <- match.arg(type)
  output <- match.arg(output)
  
  compos1 <- k1$components
  compos2 <- k2$components

  s1 <- get_strokes_compo(k1, which1)
  s2 <- get_strokes_compo(k2, which2)
  
  if (approx=="pc" || approx=="pcweighted") {
    svg_strings1 <- sapply(s1, function(x) attr(x, "d"))
    svg_strings2 <- sapply(s2, function(x) attr(x, "d"))
    
    # This simply uses the precomputed points
    points1 = do.call(rbind, s1)
    points2 = do.call(rbind, s2)
    
    min_x <- min(points1[, 1])
    max_x <- max(points1[, 1])
    min_y <- min(points1[, 2])
    max_y <- max(points1[, 2])
    min1 <- c(min_x, min_y)
    max1 <- c(max_x, max_y)
    min_x <- min(points2[, 1])
    max_x <- max(points2[, 1])
    min_y <- min(points2[, 2])
    max_y <- max(points2[, 2])
    min2 <- c(min_x, min_y)
    max2 <- c(max_x, max_y)    
    
    cen1 <- (min1 + max1) / 2
    sca1 <- max1-min1           # 2D extension
    cen2 <- (min2 + max2) / 2
    sca2 <- max2-min2
    
    meansca <- sqrt(sca1*sca2)
    upfact <- max(meansca)
    
    fact1 <- sqrt(sca2/sca1)/upfact  
    fact2 <- sqrt(sca1/sca2)/upfact
    '
    rescaled_points <- matrix(nrow = nrow(points1), ncol = ncol(points1))
    rescaled_points[, 1] <- (points1[, 1] - min1[1]) * fact1[1] # Rescale x
    rescaled_points[, 2] <- (points1[, 2] - min1[2]) * fact1[2] # Rescale y
    points1 <- rescaled_points
    
    rescaled_points <- matrix(nrow = nrow(points2), ncol = ncol(points2))
    rescaled_points[, 1] <- (points2[, 1] - min2[1]) * fact2[1] 
    rescaled_points[, 2] <- (points2[, 2] - min2[2]) * fact2[2]
    points2 <- rescaled_points'
    
    points1 <- matrix(0, 0, 2)
    points2 <- matrix(0, 0, 2)
    mass1 <- numeric()
    mass2 <- numeric()
    # In case we want to control the number of points, we reconstruct from SVG like so:
    for (svg_string in svg_strings1) {
      new_points <- points_from_svg(svg_string, 50, spaced=TRUE, offset=-min1, factor=fact1)
      points1 <- rbind(points1, new_points)
      if (approx == "pcweighted") # Here, we are weighing points by the nearest neighbors within the SVG command:
        mass1 <- c(mass1, average_distances(new_points))
      else
        mass1 <- rep(1, length(points1)/2)
    }
    for (svg_string in svg_strings2) {
      new_points <- points_from_svg(svg_string, 50, spaced=TRUE, offset=-min2, factor=fact2)
      points2 <- rbind(points2, new_points)
      if (approx == "pcweighted")
        mass2 <- c(mass2, average_distances(new_points))
      else
        mass2 <- rep(1, length(points2)/2)
    }
    # Instead we could also use global nearest neighbors:
    # nn_dists1 <- nn2(points1, points1, k=2)$nn.dists[,2]
    # nn_dists2 <- nn2(points2, points2, k=2)$nn.dists[,2]

    '
    massa <- mass1
    massb <- mass2
    massa <- massa/sum(massa)
    massb <- massb/sum(massb)
    # We transform to a weighted transport::points object.
    a <- transport::wpp(matrix(points1, length(points1)/2), massa)
    b <- transport::wpp(matrix(points2, length(points2)/2), massb)
    ink1 <- length(points1)
    ink2 <- length(points2)
    res <- transport::transport(a,b, fullreturn=TRUE, method="networkflow")'
    
    ink1 <- sum(mass1)
    ink2 <- sum(mass2)
    output <- ifelse(output=="distplus", "dist", output)
    
    if (type == "unbalanced") {
      a <- transport::wpp(points1, mass1)
      b <- transport::wpp(points2, mass2)
      res <- as.list(unbalanced_wpp(a, b, p=p, C=C, output=output))   
      # argument output does not do anything yet
    } else if (type == "rtt") {
      a <- transport::wpp(points1, mass1)
      b <- transport::wpp(points2, mass2)
      res <- as.list(unbalanced_wpp(a, b, p=p, C=C, output=output))
      res[[1]] <- res[[1]]/max(ink1, ink2)^(1/p) # instead we could just divide massa, massb above by max(ink1, ink2)^(1/p) (same result, not clear which is preferable)
    } else if (type == "balanced") {
      a <- transport::wpp(points1, mass1/ink1)
      b <- transport::wpp(points2, mass2/ink2)
      res <- as.list(unbalanced_wpp(a, b, p=p, output=output))
      # essentially the same as transport::transport.wpp, but the latter does not directly return the dist
      # and the output is in a bit a different format
      # @file-acomplaint: resulting distance coincides with commented-out 
      # transport::transport above (currently line 706)
    }
    
    # For debugging, we might want to have a look at the point clouds:
    # plot(points1, cex=0.5*massa*length(points1), asp=1)
    # plot(points2, cex=0.5*massb*length(points2), asp=1)
    # DS: cex proportional to sqrt(massa), sqrt(massb) is more appropriate
    # human brains usually judge importance by area not ba diameter
    # the following command does this (among other things)
    
    # @file-acomplaint: the following only works (i.e. shows the transport plan) for
    # if res was obtained by transport::transport.wpp 
    'transport::plot.wpp(a,b,res$default)
    title(res$cost)
    res <- as.list(res$cost)'
  } else {
    # Here, bitmaps are used for optimal transport:
    
    # location and scale based on min/max dimension of stroke info is extreme but
    # not considerably worse than anything else and much more efficient than 
    # creating a bitmap first points of all strokes in a single matrix
    mat1 <- do.call(rbind, s1)
    mat2 <- do.call(rbind, s2)
    min1 <- apply(mat1, 2, min)   
    max1 <- apply(mat1, 2, max)
    min2 <- apply(mat2, 2, min)
    max2 <- apply(mat2, 2, max)
    cen1 <- (min1 + max1) / 2
    sca1 <- max1-min1           # 2D extension
    cen2 <- (min2 + max2) / 2
    sca2 <- max2-min2
    meansca <- sqrt(sca1*sca2)  # this is still 2D of course.
    # The geometric mean is nicer here because going from ele1 to ele2 (the elements given
    # by which) we can multiply twice by sqrt(sca2/sca1) and arrive after the first multipl. at the mean
    # and after the second multipl. at ele2. In contrast to, say, the arithmetic mean this operation
    # is consistent with the natural penalty of sca2/sca1 we return below.
    upfact <- max(meansca) # this is one way of doing it meaning if the aspect ratio is not "the same
                           # way round" for both kanji we do not make the kanji the same aspect ratio in the end
                           # the other way would be instead of fact1, fact2 below to set both directions to size 1
    fact1 <- sqrt(sca2/sca1)/upfact  
    fact2 <- sqrt(sca1/sca2)/upfact  
    
    s1_scaled <- lapply(s1, \(x) { (x - matrix(cen1, nrow(x), 2, byrow = TRUE))*matrix(fact1, nrow(x), 2, byrow = TRUE)*0.95 + matrix(0.5, nrow(x), 2) })
    s2_scaled <- lapply(s2, \(x) { (x - matrix(cen2, nrow(x), 2, byrow = TRUE))*matrix(fact2, nrow(x), 2, byrow = TRUE)*0.95 + matrix(0.5, nrow(x), 2) })
    bm1 <- strokelist_to_bitmap(s1_scaled, size=size, lwd=lwd)
    bm2 <- strokelist_to_bitmap(s2_scaled, size=size, lwd=lwd)
    # transport::matimage(bm1, asp=1) # For debugging, we can print the matrices
    # transport::matimage(bm2, asp=1)
    
    ink1 <- sum(bm1)
    ink2 <- sum(bm2)
    output <- ifelse(output=="distplus", "dist", output)
    
    if (type == "unbalanced") {
      a <- transport::pgrid(bm1)
      b <- transport::pgrid(bm2)
      res <- as.list(transport::unbalanced(a, b, p=p, C=C, output=output))   # for output="all" it is already a list
                                                                             # for output="dist" we will start a new list 
    } else if (type == "rtt") {
      a <- transport::pgrid(bm1)
      b <- transport::pgrid(bm2)
      res <- as.list(transport::unbalanced(a, b, p=p, C=C, output=output))
      res[[1]] <- res[[1]]/max(ink1, ink2)^(1/p) # instead we could just divide bm1, bm2 above by max sum (same result, not clear which is preferable)
    } else if (type == "balanced") {
      a <- transport::pgrid(bm1/ink1)
      b <- transport::pgrid(bm2/ink2)
      res <- as.list(transport::unbalanced(a, b, p=p, output=output))
      # essentially the same as transport::transport.pgrid, but the latter does not directly return the dist
      # and the output is in a bit a different format
    }
  }
  
  names(res)[1] <- "dist"  # was already the case if output="all"
  
  # browser()
  ele1 <- pluck(compos1, !!! which1, 1)
  ele2 <- pluck(compos2, !!! which2, 1)
  description <- list(elements=c(ele1,ele2), which=c(paste(which1, collapse=""), paste(which2, collapse="")))
  
  cursca1 <- sca1/max(sca1)
  cursca2 <- sca2/max(sca2)
  res <- c(description, res, list(ink1=ink1, ink2=ink2, reltrans=cen2-cen1, scar=sca2/sca1, distort=cursca2/cursca1))
  # this is ink after up-scaling; not much good, since we scale differently in x and y direction differently.
  # It is not possible to get a more or less exact number for the ink before scaling unless we do another plot, which
  # seems overkill)
  # reltrans = relative translation (of ele2 minus ele1)
  # scar = scale ratio (of ele2 to ele1)
  # distort is an intermediate result, in the end we do nothing else than
  # (abs log of) (sca2[2]/sca2[1]) / (sca1[2]/sca1[1])  for the distortion penalty in kanjidist
  
  return(res)
}


strokelist_to_bitmap <- function(strokelist, size=NULL, lwd=2.5, ...){
  ll <- list(...)
  if (!is.null(ll$col)) {
    warning("argument col is ignored for generating grayscale bitmap")
  }
  
  if (is.null(size)) {
    default_size <- get_kanjistat_option("default_bitmap_size")
    if (is.null(default_size)) size <- 48 else size <- default_size
  }
  
  fname <- tempfile("subtree", fileext=".png")
  png(filename=fname, width = size, height = size, res=72, ...)
  # type = c("cairo", "Xlib", "quartz"), at least on my system (cairo I pressume)
  # gray-antialiasing is the standard (I guess subpixel aa (if possible for png),
  # would make things difficult if we extract (only) the grayscales)
  # saving as grayscale png would save memory (and time presumably), but
  # I do not seem to get antialiasing to work in that case:
  # bitmap(file=fname, type="pnggray", width=size, height=size, res=72, pointsize=12, units="px", taa=4)
  
  oldpar <- par(mai=rep(0,4))
  on.exit(par(oldpar))
  plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=FALSE, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
  lapply(strokelist, lines, col=1, lwd=lwd, ...)
  dev.off()
  
  temp<- png::readPNG(fname)
  # on my system identical(res[,,1], res[,,2]) and identical(res[,,1], res[,,3])
  # return true, but you never know so we take an unweighted average
  # (note that rgb to grayscale algos would usually take a weighted average with more than
  # 0.5 weight on green, e.g. opencv 0.299*R + 0.587*G + 0.114*B)
  resmat <- 1-apply(temp[,,1:3], c(1,2), mean)
  # we invert the image (1-... above) mainly for the mass transport algorithm
  # and of course for a cool darkmode style :-)
  unlink(fname)

  return(resmat)
}


# =============================================================
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   everything from here on is for precompute = TRUE 
#   (its use is not recommended)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# advantage: we can precompute everything per kanji
# (this could have made the computation of the distance much more efficient, but it did not ...)
# another disadvantage: we cannot compute transport after correcting for a distortion
# compo_seg_depth is seg_depth + 1
kcompos_to_bitmaps <- function(kanji, compo_seg_depth=4, size=c(64, 64, 48, 48), lwd=c(3.2, 3.2, 2.5, 2.5), ...) {
  ll <- list(...)
  if (!is.null(ll$col)) {
    warning("argument col is ignored for generating grayscale bitmap")
  }
  
  if (compo_seg_depth > length(kanji$ncompos)) {
    warning("No inner nodes at compo_seg_depth ", compo_seg_depth, ". Using maximal compo_seg_depth of ", length(kanji$ncompos))
    compo_seg_depth <- length(kanji$ncompos)
  }
  
  if (is.null(size)) {
    default_size <- get_kanjistat_option("default_bitmap_size")
    lwd <- 3.2
    # if (is.null(default_size)) size <- 64 else size <- default_size
  }
  
  if (length(size) == 1) {
    size <- rep(size, compo_seg_depth)
  }
  if (length(lwd) == 1) {
    lwd <- rep(lwd, compo_seg_depth)
  }
  stopifnot(length(size) == compo_seg_depth)
  stopifnot(length(lwd) == compo_seg_depth)
  copyunreal <- length(unique(size)) == 1 && length(unique(lwd)) == 1
  # whether we can just copy the unreal components (or whether they might have 
  # different sizes/lwds than there originals)
  
  ncompos <- kanji$ncompos[seq_len(compo_seg_depth)]
  compos <- kanji$components[seq_len(compo_seg_depth)]
  get_real <- attr_getter("real")
  get_original <- attr_getter("original")
  
  res_bitmaps <- list()
  res_stats <- list()
  for (l in seq_along(ncompos)) {
    for (i in seq_len(ncompos[l])) {
      if (copyunreal && !pluck(compos, l, i, get_real)) {
        copyfrom <- pluck(compos, l, i, get_original)
        # a coordinate that is already computed
        pluck(res_bitmaps, l, i) <- pluck(res_bitmaps, !!! copyfrom)
        pluck(res_stats, l, i) <- pluck(res_stats, !!! copyfrom) 
      } else { # plot and create matrix 
        strokes <- get_strokes_compo(kanji, which = c(l, i)) 
        
        # first we compute the individual trans and scale values 
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        smat <- do.call(rbind, strokes)
        smin <- apply(smat, 2, min) 
        smax <- apply(smat, 2, max)
        scen <- (smin + smax) / 2
        ssca <- smax-smin # 2-d extension
        fact <- 0.95/max(ssca) 
        strokes_trafo <- lapply(strokes, \(x) { (x - matrix(scen, nrow(x), 2, byrow = TRUE)) *
            matrix(fact, nrow(x), 2, byrow = TRUE) + matrix(0.5, nrow(x), 2) })
        
        # now we plot the translated and scaled components
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        fname <- tempfile("subtree", fileext=".png")
        png(filename=fname, width = size[l], height = size[l], res=72, ...)
        oldpar <- par(mai=rep(0,4))
        on.exit(par(oldpar))
        plot(0.5, 0.5, xlim=c(0,1), ylim=c(0,1), axes=FALSE, type="n", asp=1, xaxs="i", yaxs="i", xlab="", ylab="")
        lapply(strokes_trafo, lines, col=1, lwd=lwd[l], ...)
        dev.off()
        
        temp<- png::readPNG(fname)
        resmat <- 1-apply(temp[,,1:3], c(1,2), mean)
        unlink(fname)
        
        pluck(res_bitmaps, l, i) <- resmat
        
        ele <- ifelse(l == 1, kanji$char, pluck(compos, l, i, 1))
        ink <- sum(resmat)
        pluck(res_stats, l, i) <- list(element=ele, ink=ink, trans=scen, scale=ssca)
      }
    }
  }
  
  return(list(bitmaps=res_bitmaps, stats=res_stats))
}


# same as component_cost but with precomputed components
# there is hardly any advantage of this: plotting the pictures is no major bottleneck (not even a minor one)
# all the time lies in the OT and in fact 10-20 % in the distance computation on the grid (!??), which could be done
# once and for all
# disadvantage is that we cannot nicely scale in an adapted way
# careful: which is in terms of + 1 increased levels
component_cost_prec <- function(bitstats1, bitstats2, which1=c(1,1), which2=c(1,1), p=1, C=0.2,
                                type=c("rtt", "unbalanced", "balanced"), output=c("distplus","all")) {
  type <- match.arg(type)
  output <- match.arg(output)
  
  cmap1 <- chuck(bitstats1, 1, !!!which1)
  cmap2 <- chuck(bitstats2, 1, !!!which2)
  cstats1 <- chuck(bitstats1, 2, !!!which1)
  cstats2 <- chuck(bitstats2, 2, !!!which2)
  sca1 <- cstats1$scale
  sca2 <- cstats2$scale
  
  cursca1 <- sca1/max(sca1)
  cursca2 <- sca2/max(sca2)
  fact <- sqrt(cursca2/cursca1)
  if (!isTRUE(all.equal(fact[1], 1))) {
    # keep lwd 3.2 for now because it is our standard
    cmap1 <- scale_erodilate_plus(cmap1, factor=fact[1], horizontal=TRUE, erodilate=TRUE)#, lwd=3.2)
    cmap2 <- scale_erodilate_plus(cmap2, factor=1/fact[1], horizontal=TRUE, erodilate=TRUE)#, lwd=3.2)
  }
  if (!isTRUE(all.equal(fact[2], 1))) {
    # keep lwd 3.2 for now because it is our standard
    cmap1 <- scale_erodilate_plus(cmap1, factor=fact[2], horizontal=TRUE, erodilate=TRUE)#, lwd=3.2)
    cmap2 <- scale_erodilate_plus(cmap2, factor=1/fact[2], horizontal=TRUE, erodilate=TRUE)#, lwd=3.2)
  }
  # transport::matimage(cmap1)
  # transport::matimage(cmap2)
  
  output <- ifelse(output=="distplus", "dist", output)
  ink1 <- sum(cmap1)
  ink2 <- sum(cmap2)
  
  if (type == "unbalanced") {  # not recommended (for one reason since not bounded by 1)
    a <- transport::pgrid(cmap1)
    b <- transport::pgrid(cmap2)
    res <- as.list(transport::unbalanced(a, b, p=p, C=C, output=output))   # for output="all" it is already a list
    # for output="dist" we will start a new list 
  } else if (type == "rtt") {  # recommended from the point of view of the global construction of the distance map
    a <- transport::pgrid(cmap1)
    b <- transport::pgrid(cmap2)
    res <- as.list(transport::unbalanced(a, b, p=p, C=C, output=output))
    res[[1]] <- res[[1]]/max(ink1, ink2) # instead we could just divide bm1, bm2 above by max sum (same result, not clear which is preferable)
  } else if (type == "balanced") { 
    a <- transport::pgrid(cmap1/ink1)
    b <- transport::pgrid(cmap2/ink2)
    res <- as.list(transport::unbalanced(a, b, p=p, output=output))
    # essentially the same as transport::transport.pgrid, but the latter does not directly return the dist
    # and the output is in a bit a different format
  }
  names(res)[1] <- "dist"  # was already the case if output="all"
  
  description <- list(elements=c(cstats1$element,cstats2$element),
                      which=c(paste(which1, collapse=""), paste(which2, collapse="")))
  
  res <- c(description, res, list(ink1=ink1, ink2=ink2, reltrans=cstats2$trans-cstats1$trans, scar=sca2/sca1, distort=fact^2))
  # ink is ink after scaling and erodilating; here we have direct access to the previous values
  # reltrans = relativ translation (of ele2 minus ele1)
  # scar = scale ratio (of ele2 to ele1)
  # distort = distortion factor (after scaling up ele1 and ele2 by /max(sca1) and /max(sca2), respectively)
  return(res)
}


# does a simple scaling in one direction (horizontal or vertical) using linear interpolation
# and returns the central clipping of the same size (i.e. image dimensions are not changed) 
scale_erodilate <- function(mat, factor=1.5, horizontal=TRUE, erodilate=TRUE) {  # currently assumed that factor >= 1
  if (horizontal) mat <- t(mat)
  m <- dim(mat)[[1]]
  n <- dim(mat)[[2]]  # number of pixels n in the (now always vertical) scaling direction is assumed to be even,
                      # otherwise there might be a slight distortion
  scale_erodi_line <- function(j) {  # j = col number
    rescol <- mat[,j]
    if (erodilate && factor < 0.9) {
      rescol <- mapply(\(x, y, z) {ifelse( (x==1 & y>0) | (y>0 & z==1), 1, y) },
                       c(0, rescol[1:(m-1)]), rescol[1:m], c(rescol[2:m], 0) )
    }
    rescol <- approx( x = seq(0.5, length.out=n) * factor, y = rescol, 
                      xout = ((factor-1)*m/2 + 0.5):((factor+1)*m/2 - 0.5), rule = 2 )$y
                      # rule = 2 should in all of our situations lead to extrapolations by zeros (for factor < 1)
    if (erodilate && factor > 1.1) {
      rescol <- mapply(\(x, y, z) {ifelse( (x==0 & y>0 & z>0) | (x>0 & y>0 & z==0), 0, y) },
                       c(0, rescol[1:(m-1)]), rescol[1:m], c(rescol[2:m], 0) )
    } 
    return(rescol)  
  }
  
  res <- sapply(1:n, scale_erodi_line)  # note that apply always fills in vector function values as columns
  
  if (horizontal) {
    return(t(res))
  } else {
    return(res)
  }
}
  

# improved version of erodilate; still not perfect but much better
# ahaha, microbenchmark gives that scale_erodilate_plus with factor > 1 is even quite a bit faster (times 0.75)
# possibly ifelse is surprisingly slow (so this could be fixed above)
# however scale_erodilate_plus with factor > 1 and erodilate=FALSE is of course much faster (a bit less than times 0.2)
scale_erodilate_plus <- function(mat, factor=1.5, horizontal=TRUE, erodilate=TRUE, lwd=3.2) {  
  # if ever lpix is smaller than 2 (i.e. lwd smaller than 8/3), scaling down (factor < 1) 
  # will not work well (erodilate does not necessarily recognize vertical(ish) lines)
  if (horizontal) mat <- t(mat)
  m <- dim(mat)[[1]]
  n <- dim(mat)[[2]]  # number of pixels n in the (now always vertical) scaling direction is assumed to be even,
                      # otherwise there might be a slight distortion
  
  # ~~~~~~~~~~ linewise scaling function plus kind-of-dilation or erosion (depending on factor) ~~~~
  scale_erodi_line <- function(j) {  # j = col number
    rescol <- mat[,j]
    
    if (erodilate && factor < 0.9) { # we do the kind-of-dilation (if necessary) before the scaling because it is easier
                                     # to see the true vertical(ish) strokes. The disadvantage is that we seem to
                                     # systematically undercorrect the (total mass of the) thickness (and tend to loose the 1 values in the middle)
                                     # (the erosion below is probably easier to understand)
      to_add <- 0.5*(1-factor)*0.75*lwd
      rescol <- mapply(\(z1, z2, z3, z4) {
                         if (z2<=z3 & z3<1 & z4==1) { # the = in <= is for the unlikely case that z1=0, z2==z3, z4=0 (has to belong to one of the two cases)
                           r <- z3 + to_add
                           return( c( min(1, z2+max(0,r-1)), min(1,r) ) )
                         }
                         if (z1==1 & z2<1 & z3<z2) {
                           r <- z2 + to_add
                           return( c( min(1,r), min(1, z3+max(0,r-1)) ) )
                         }
                         return( c(z2, z3) )
                       },
                       c(0, rescol[seq(2,(m-2),2)]), rescol[seq(1,m-1,2)], 
                       rescol[seq(2,m,2)], c(rescol[seq(3,m-1,2)], 0) )
    }
    
    rescol <- approx( x = seq(0.5, length.out=n) * factor, y = rescol, 
                      xout = ((factor-1)*m/2 + 0.5):((factor+1)*m/2 - 0.5), rule = 2 )$y
    # rule = 2 should in all of our situations lead to extrapolations by zeros (for factor < 1)
    
    if (erodilate && factor > 1.1) { # kind-of-erosion
      to_remove <- 0.5*(factor-1)*0.75*lwd
      rescol <- mapply(\(z1, z2, z3, z4) {
                         if (z1==0 & z2>0 & z3>=z2) { # the = in >= is for the unlikely case that z1=0, z2==z3, z4=0 (has to belong to one of the two cases)
                           r <- z2 - to_remove
                           return( c( max(0,r), max(0, z3-max(0,-r)) ) )
                         }
                         if (z2>z3 & z3>0 & z4==0) {
                           r <- z3 - to_remove
                           return( c( max(0, z2-max(0,-r)), max(0,r) ) )
                         }
                         return( c(z2, z3) )
                       },
                    c(0, rescol[seq(2,(m-2),2)]), rescol[seq(1,m-1,2)], 
                    rescol[seq(2,m,2)], c(rescol[seq(3,m-1,2)], 0) )
      # linewidth in pixels is (almost exactly) lpix = lwd*0.75 (we need to know the original linewidth with which we drew;
      # default is lwd=3.2 so 2.4 pixels, reality is 2.40something for some reason)
      # so after scaling with factor, we can/should remove (lpix/2) * (factor -1) to the left and right of vertical strokes
      # and it does not hurt to do this for anything. Because we sample only a small number of points this seems hard to do
      # adequately. We could oversample in the commands above, then do a horizontal grayscale erosion (min) on the right scale, (interpolate and)
      # sample the final m values, but this seems hardly an advantage compared to drawing the component with the correct distortion
      # in the longer version. For these reasons, we simply (binarily) erode horizontally the pixel adjacent to 0. Due to
      # the linear interpolation it is almost never supposed to be there.
    } 
    
    return(rescol)   
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  res <- sapply(1:n, scale_erodi_line)  # note that apply always fills in vector function values as columns
  
  if (horizontal) {
    return(t(res))
  } else {
    return(res)
  }
}
