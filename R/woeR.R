
#' @title Weight of Evidence based segmentation of a variable
#'
#' @description Create heterogeneous segmentations of a numeric variable based on a dependent variable using Weight of Evidence approach
#'
#' @param df  A data frame containing input arguments - variable & dv
#'
#' @param variable character string specifying the column name of the variable you want to bin. Currently, the code supports only numeric and integer classes
#'
#' @param dv character string specifying the column name of the binary dependent variable(0,1) (NAs are ignored). Dependent variable should be either of integer or numeric class
#'
#' @param min_perc Minimum percentage of records in each segment. If the percentage of records in a segment falls below this threshold it is merged with other segments. Acceptable values are in the range 0.01-0.2
#'
#' @param initial_bins No of segments of the variable to be created in the 1st iteration. Default value = 50(2 percent) for sample size > 1500. Acceptable values are in the range 5-100
#'
#' @param woe_cutoff Thereshold of the absolute difference in woe values between consecutive segments. If the difference is less than this threshold segments are merged. Acceptable values are in the range 0-0.2
#'
#' @return Output is a list containing the following elements : \cr
#' a) variable - value of the input argument 'variable' \cr
#' b) dv - value of the input argument 'dv' \cr
#' c) breaks - vector specifying cut-off values for each segment. Pass it to 'breaks' argument of cut function to create segments of the variable \cr
#' d) woe - woe table for the final iteration \cr
#' e) IV - Information Value for the final iteration
#'
#' @details Weight of Evidence represents the natural log of the ratio of percent of 0's in the segment to percent of 1's in the segment. It is a proxy for how far the dv rate for a segment is from the sample dv rate (# of 1s/# of observations).
#'
#' @examples library(smbinning)
#' data("chileancredit")
#' woe_binning(chileancredit, "cbs1", "fgood", initial_bins = 10)
#'
#' @export woe_binning
#'
#' @import "dplyr"

woe_binning <- function(df, variable, dv, min_perc = 0.02, initial_bins = 50, woe_cutoff = 0.1){
  #print(woe_cutoff)
  # Check input arguments have been supplied
  if (missing(df) == TRUE || missing(variable) == TRUE || missing(dv) == TRUE) {
    stop("One of the following input parameters is missing with no default: df/variable/dv.")
  }
  # Check if df is a data.frame
  if (!is.data.frame(df)) {
  return("Data not a data.frame")
  }
  # Check input arguments are not numeric
  if (is.numeric(variable) | is.numeric(dv)) {
    return("Parameter variable or dv not a string")
  }
  # Checking the arguments are present in the dataset
  if (is.na(which(names(df) == variable)[1]) | is.na(which(names(df) == dv)[1]) ) {
    stop("Parameters variable/dv not found in the datatset.")
  }
  # Converting tibbles/data tables to data frame & removing NA's from dv
  # Check if df has rows after removing NA from dv
  df <- df[!is.na(df[, dv]), ] %>% data.frame
  if (nrow(df) == 0) {
    stop("Parameter dv has only NAs.")
  }
  # Boundary checks on dv
  if (!class(df[, dv]) %in% c("numeric", "integer")) {
    stop("Incorrect parameter specification. Argument dv is not numeric/integer.")
  }
  # Check dv has two distinct values
  if (!(length(unique(df[, dv])) == 2)) {
    stop("Incorrect parameter specification. Dependent variable must have two distinct values excluding NAs.")
  }
  if (!(max(df[, dv], na.rm = T) == 1 & min(df[, dv], na.rm = T) == 0)) {
    stop("Incorrect parameter specification. Argument dv can only contain 0,1 values excluding NA.")
  }
  # Boundary checks on woe_cutoff
  if (woe_cutoff < 0 | woe_cutoff > 0.2 | !is.numeric(woe_cutoff)) {
    warning("Incorrect parameter specification; accepted woe_cutoff parameter range is 0-0.2. Parameter was set to 0.1.")
    woe_cutoff = 0.1
  }
  # Boundary checks on initial_bins
  if (nrow(df) <= 1500) {
    initial_bins <- max(5, floor(nrow(df)/30))
    print(paste0("Dataset is small (<= 1500 rows). Parameter initial_bins was set to ", initial_bins, "."))
  }
  if (initial_bins < 5 | initial_bins > 100 | !is.numeric(initial_bins)) {
    warning("Incorrect parameter specification; accepted initial_bins parameter range is 5-100. Parameter was set to 50.")
    initial_bins <- 50
  }
  # Boundary checks on min_perc
  if (min_perc < 0.01 | min_perc > 0.2 | !is.numeric(min_perc)) {
    warning("Incorrect parameter specification; accepted min_perc parameter range is 0.01-0.2. Parameter was set to 0.05.")
    min_perc = 0.05
  }

  if (class(df[, variable]) %in% c("numeric", "integer")) {
    return(woe_binning1(df, variable, dv, min_perc, initial_bins, woe_cutoff))
  } else {
    return("Function handles only integer/numeric classes")
  }
}

#woe_binning.default <- function(df, variable, dv, min_perc = 0.02, initial_bins = 50, woe_cutoff = 0.05){
#  stop("Function handles only interger/numeric class.")
#      }

woe_binning1 <- function(df, variable, dv, min_perc = 0.02, initial_bins = 50, woe_cutoff = 0.1){

  # Boundary checks on Variable distribution
  check <- boundary_checks(df, variable)
  if (!is.null(check)) {
    return(check)
  }
  # Computing breaks
  #print(initial_bins)
  breaks <- unique(stats::quantile(df[, variable] , probs = seq(0,1,1/initial_bins) , na.rm=T))
  breaks[initial_bins + 1] <- Inf
  breaks[1] <- -Inf
  # Compute woe & IV table
  woe <- compute_woe(df = df, variable = variable, dv = dv, breaks = breaks)
  #print(dim(woe))
  # Separate out NA values from the data
  woe_na <- woe[which(is.na(woe$rank)),]
  woe <- woe[which(!is.na(woe$rank)),]
  df <- df[which(!is.na(df[, variable])), ]
  ## Merge infinity bins
  inf_rows <- woe$rank[which(woe$woe %in% c(Inf, -Inf))]
  ## Take max and do below
  if (length(inf_rows)){
    #print(inf_rows)
    # Merging bins with WOE = Inf
    x <- infinity_check(df, woe, inf_rows, breaks, dv, variable)
    woe <- x$woe
    breaks <- x$breaks
    rm(x)
  }
  ## Call recursive binning function
  breaks <- recursive_woe(df , variable , dv , woe , breaks , woe_cutoff = woe_cutoff , min_perc = min_perc)
  df$rank <- cut(df[, variable], breaks = breaks, include.lowest = T)
  df$del <- df[, dv]
  woe <-
    df %>%
      group_by(rank) %>%
      summarise(bad = sum(del) , total = n()) %>%
      ungroup %>%
      data.frame
  woe <- rbind(woe, woe_na[,c("rank", "bad", "total")]) %>%
      mutate(good = total - bad , bad_percent = bad/sum(bad) , good_percent = good/sum(good),
             percent = total/sum(total), woe = log(good_percent/bad_percent) ) %>%
      ungroup() %>%
      mutate(IV = (good_percent - bad_percent)*woe ) %>%
      select(-c(good)) %>%
      as.data.frame()
  colnames(woe) <- c("bin", "# of 1s", "# of obs.", "percent of 1s", "percent of 0s", "% obs", "woe", "IV")
  df$del <- NULL
  #df[, dv1] <- NULL
  return(
    list(
      variable = variable,
      dv = dv,
      breaks = breaks,
      woe = woe ,#%>% setNames(c("bin", "# of 1s", "# of obs.", "percent of 1s", "percent of 0s", "% obs", "woe", "IV")),
      IV = sum(woe$IV)
    )
  )
}
