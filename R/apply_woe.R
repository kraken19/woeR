#' @title Weight of Evidence based segmentation of a variable
#'
#' @description This function applies the binning generated from woe_binning to new data
#'
#' @param df A data frame. The variable names and types need to be identical to the ones passed to woe_binning
#'
#' @param woe_object Output object from woe_binning function
#'
#' @return Input data frame is returned with two new columns - bin & woe
#'
#' @examples library(smbinning)
#' data("chileancredit")
#' \dontrun{woe_object <- woe_binning(chileancredit, "CuDDAmtAvg12M", "FlagGB", initial_bins = 10)
#' out <- apply_woe(chileancredit, woe_object)
#' #Above example to create and apply woe segmentation }
#'
#' @export apply_woe
#'

# breaks <- quantile(chileancredit$CuDDAmtAvg12M, probs = seq(0,1,0.1), na.rm = T)
# compute_woe(chileancredit[which(!is.na(chileancredit$FlagGB)),], "CuDDAmtAvg12M", "FlagGB", breaks)

apply_woe <- function(df, woe_object){
  ### Do name check of list, list should have 5 elements. Then check df has those variables and are of the same class & values,
  # Check df is a data frame
  if (!is.data.frame(df)) {
  return("Data not a data.frame")
  }
  if (!identical(names(woe_object), c("variable", "dv", "breaks", "woe", "IV"))) {
    stop("Incorrect parameter specification. Parameter woe_object should be generated from woe_binning function")
  }
  # Check variables are present in the data frame
  if (is.na(which(names(df) == woe_object$variable)[1]) | is.na(which(names(df) == woe_object$dv)[1]) ) {
    stop("Parameters variable/dv not found in the datatset.")
  }
  # Check class of input arguments
  if (!class(df[, woe_object$variable]) %in% c("numeric", "integer")) {
    stop(paste0("Incorrect parameter specification. Column ", woe_object$dv, " is not numeric/integer.") )
  }
  if (!class(df[, woe_object$dv]) %in% c("numeric", "integer")) {
    stop(paste0("Incorrect parameter specification. Column ", woe_object$variable, " is not numeric/integer.") )
  }
  # Check dv has two distinct values
  if (!(length(unique(df[, woe_object$dv][!is.na(df[, woe_object$dv])])) == 2)) {
    stop(paste0("Incorrect parameter specification. Column ", woe_object$dv, " must have two distinct values excluding NAs."))
  }
  if (!(max(df[, woe_object$dv], na.rm = T) == 1 & min(df[, woe_object$dv], na.rm = T) == 0)) {
    stop(paste0("Incorrect parameter specification. Column ", woe_object$dv, " can only contain 0,1 values excluding NA."))
  }
  #df$del <- df[, woe_object$dv]
  df$bin <-
    as.character(cut(df[, woe_object$variable], breaks = woe_object$breaks, include.lowest = T))
  df <- merge(df, woe_object$woe[, c("bin", "woe")], by = "bin")
  #df$del <- NULL
  return(df)
}
