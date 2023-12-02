#' Synthetic population data for Ethiopia
#'
#'
#' Synthetic population data for Ethiopia
#'
#'
#' @format A tibble with synthetic population data for Ethiopia at the person level
#'
#'
#'
#' @source generated with the synthpop package
'population'

#' Synthetic population data for Ethiopia aggregated for the household level
#'
#'
#' Synthetic population data for Ethiopia
#'
#'
#' @format A tibble with synthetic population data for Ethiopia at the household level
#'
#'
#'
#' @source generated with the synthpop package
'population.hh'

# h <- mapPop()
# CHECK<<-h
# # source("helpers.R")
# h[ ,count:=.N, by = .(hhidg, lon)]
# # h <- h %>%
# #   group_by(hhidg, lon) %>%
# #   mutate(count = n())
# # h <- data.table(h)
# population.hh <- create.pop.hh(h)
# population.hh <- data.table(population.hh, key = "hhidg")
# ## drop na observations at lon
# population.hh <- population.hh[!is.na(population.hh$lon), ]
# ## Calculate costs as distance, create cost strata
# population.hh[, dist := mapply(hav_dist, lon.hh, lat.hh)]
# population.hh <- data.table(population.hh %>% group_by(stratum) %>% mutate(distCat = median(dist)),
#                             key = "hhidg"
# )
# population.hh[, distCat := cut(dist, breaks = 3, labels = c("l", "m", "h"))] #<-factor(population.hh$distCat, labels = c("Low"))"Medium", "High"
# population.hh[, stratum := as.factor(stratum)]
# population.hh[, cluster := as.factor(cluster)]

#' GADM 3.6 Ethiopia
#'
#'
#' Gadm level 1 data for Ethiopia
#'
#'
#' @format A sf object with GADM level 1 data for Ethiopia
#'
#'
#'
#' @source https://gadm.org/download_country_v3.html
'eth.shp'
