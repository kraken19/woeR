
boundary_checks <- function(df, variable)
  {
      nvars <- unique(df[,variable])
      if ( length(nvars) < 4) {
        print("Variable has < 4 distinct values. WOE binning not required, returning the unique values")
        ## display error message & output WOE table based on the distinct bins
        return(nvars)
        }
      ## Computing the %age for the mode of the variable
      mode.var = -1*sort( -table( df[,variable], useNA = "ifany") )[1]
      percent = mode.var/nrow(df)

      if ( percent >= .95) {
        print("Mode of the variable accounts for 95% of the data . WOE binning will be redundant, returning the mode of the variable.")
        print("Sugestion: You can create a binary for this variable")
        return(mode.var) ## Return woe table?
        }
        return()
  }
