#' PPS Sampling using data.table
#'
#' @keywords internal
#' @noRd
ppsDT<-function(data, clu="cluster", n, sizevar="HH_count"){
  data_in<-data
  if(!data.table::is.data.table(data)) 
    data_in<-data.table::data.table(data)
  
  data.table::setkeyv(data_in, clu)
  #print(key(data_in))
  ##  Preparing the data
  int<-sum(data_in[[sizevar]])/n
  data_in[,p1:=n*(data_in[[sizevar]]/sum(data_in[[sizevar]]))]
  ##  Creating Random Order
  data_in[sample(.N)]
  ##  Cumulative sum
  data_in[,cumul:=cumsum(data_in[[sizevar]])]
  ##  Random start
  rn<-runif(1, 0, int)
  ##  Creat intervall groups and select firs per group
  data_in[,int_group:=cut(cumul, seq(rn, (n+1)*int, by=int), labels=FALSE)]
  data_out<-data_in[int_group>1]
  data_out<-data_out[,.SD[1], by=int_group]
  data_out<-data_out[,c("cumul", "int_group"):=NULL]
  data_out<-data.table::setkeyv(data_out, clu)
  return(data_out) 
}

#' Allocation for Stratified Sampling
#'
#' @param n.tot Total sample size
#' @param Nh Vector of stratum population sizes
#' @param Sh Vector of stratum standard deviations
#' @param cost Total budget / cost
#' @param ch Vector of stratum unit costs
#' @param alloc Allocation type: "neyman", "totcost", or "prop"
#' @return A list with component `nh` containing stratum sample sizes
#' @keywords internal
#' @noRd
strAlloc <- function(n.tot = NULL, Nh = NULL, Sh = NULL, cost = NULL, ch = NULL, 
                     alloc = c("neyman", "totcost", "prop")) {
  alloc <- match.arg(alloc)
  if (is.null(Nh)) stop("Nh cannot be NULL.")
  N <- sum(Nh)
  Wh <- Nh / N
  
  if (alloc == "prop") {
    if (is.null(n.tot)) stop("n.tot is required for proportional allocation.")
    nh <- n.tot * Wh
  } else if (alloc == "neyman") {
    if (is.null(n.tot)) stop("n.tot is required for Neyman allocation.")
    if (is.null(Sh)) stop("Sh cannot be NULL for Neyman allocation.")
    denom <- sum(Wh * Sh)
    if (denom == 0) {
      nh <- rep(n.tot / length(Nh), length(Nh))
    } else {
      nh <- n.tot * Wh * Sh / denom
    }
  } else if (alloc == "totcost") {
    if (is.null(cost)) stop("cost must be specified for totcost allocation.")
    if (is.null(Sh)) stop("Sh cannot be NULL for totcost allocation.")
    if (is.null(ch)) stop("ch cannot be NULL for totcost allocation.")
    d1 <- sum(Wh * Sh / sqrt(ch))
    if (d1 == 0) {
      ph.cost <- rep(1 / length(Nh), length(Nh))
    } else {
      ph.cost <- (Wh * Sh / sqrt(ch)) / d1
    }
    denom2 <- sum(Wh * Sh * sqrt(ch))
    if (denom2 == 0) {
      n.cost <- cost
    } else {
      n.cost <- cost * d1 / denom2
    }
    nh <- n.cost * ph.cost
  }
  
  list(allocation = alloc, Nh = Nh, Sh = Sh, nh = nh)
}

