#' Late spring frost
#' 
#' Compute late spring frost, which is a freezing event occurring after
#'  a substantial accumulation of warmth
#' 
#' @family GDD functions
#' @inheritParams temperature
#' @inheritParams GDD
#' @param tfrost an integer for the freezing threshold
#' @details 
#' Additional arguments:
#' 
#' \code{equation}: character to specify the equation to be used, \code{"variant_a"} 
#'  is set by default. See GDD() 
#' 
#' \code{last.day}: an object (optional to \var{span}) of class \code{Date} or
#'  any other object that can be coerced to \code{Date} (e.g. integer, character 
#'  YYYY-MM-DD) for the last day of the time series
#'  
#' \code{span}: an integer (optional to \var{last.day}) or a vector with 
#'  integers (optional if \var{last.day} is given) for the length of 
#'  the time series to be captured
#' @return a data.frame with the late frost events
#' \item{id}{the \var{object} id}
#' \item{date}{the first day of the frost event}
#' \item{gdd}{the growing degree-days accumulated before the frost event}
#' \item{duration}{the number of days the frost event spanned}
#' @source 
#' Trnka et al. (2014). Nature Climate Change 4(7):637–43.
#' \cr\url{https://doi.org/10.1038/nclimate2242}
#' 
#' Zohner et al. (2020). PNAS.
#' \cr\url{https://doi.org/10.1073/pnas.1920816117}
#' 
#' @export
late_frost <- function(object, ..., base = 5, tfrost = -2) {
  UseMethod("late_frost")
}

# ts <- get_timeseries(lonlatsf,
#                      day.one = "2019-01-01",
#                      last.day = "2019-06-30",
#                      pars = c("T2M_MAX","T2M_MIN"))
# 
# 
# ts
# 
# object <- ts
# base <- 5
# tfrost <- -2
# equation = "variant_a"
# 
# obj <- cbind(ts[[1]], tmin = ts[[2]]$value)
# names(obj)[names(obj) == "value"] <- "tmax"
# 
# .late_frost(obj, base, tfrost, equation)
# 
# .late_frost <- function(obj, base, tfrost, equation = "variant_a") {
# 
#   obj <- split(obj, obj$id)
# 
#   result <- lapply(obj, function(temp){
#     # get tmin
#     tmin <- temp$tmin
# 
#     # calculate GDD
#     g <- climatrends:::.gdd(temp,
#               base = base,
#               equation = equation,
#               return.as = "gdd")$gdd
# 
#     # get a vector where 1 are the freeze events and 0 are the non-freeze
#     freeze <- as.integer(tmin <= tfrost) & g == 0
# 
#     # apply rle function to find the lengths of
#     # each event (freeze and non-freeze)
#     r <- rle(freeze)
# 
#     # rep the lengths to create an id for each event
#     rids <- r$lengths
#     rids <- rep(1:length(rids), rids)
# 
#     # get only the freeze events which are the 1's
#     isfreeze <- which(r$values == 1)
# 
#     # create a data frame with this data
#     dat <- data.frame(date = temp$date,
#                       gdd = g,
#                       freeze_id = rids,
#                       freeze = freeze,
#                       stringsAsFactors = FALSE)
# 
#     # and split it to get the summaries of each event
#     dat <- split(dat, dat$freeze_id)
# 
#     dat <- lapply(dat, function(x){
#       data.frame(date = x$date[1],
#                  freeze_id = x$freeze_id[1],
#                  gdd = sum(x$gdd, na.rm = TRUE),
#                  duration = length(x$freeze_id))
#     })
# 
#     dat <- do.call("rbind", dat)
# 
#     # here we should place the accumalated gdd in following frost event
#     # so it will show how many gdd was accumulated before the frost event started
#     dat[isfreeze[-1], "gdd"] <- dat[isfreeze -1, "gdd"]
# 
#     # keep only the values for the frost events plut the last row wich shows how
#     # the season continued accumulating gdd
#     dat <- dat[c(isfreeze, nrow(dat)), ]
# 
#     # put NA in the last row as it do not represent a frost event
#     dat[!dat$freeze_id %in% isfreeze, "duration"] <- NA
# 
#     # put the id for the row
#     dat <- cbind(id = temp$id[1], dat)
# 
#   })
# 
#   result <- do.call("rbind", result)
# 
#   result <- result[result$gdd != 0, -which(names(result) == "freeze_id")]
# 
#   rownames(result) <- 1:dim(result)[[1]]
# 
#   class(result) <- union("clima_df", class(result))
# 
#   return(result)
# 
# 
# }
# 
