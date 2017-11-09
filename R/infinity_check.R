
# Checking infinity cases
infinity_check <- function(woe, inf_rows, breaks, dv, variable){
    woe_merged <- list()
    ## Handle cases when rows to merge is = 1 or last row - remove these rows and check if output has 1 or last row -1
    boundary_cases <- is.na(
                            c(match(1, inf_rows),
                              match( length(breaks)-1, inf_rows)
                              )
                            )
    if (!boundary_cases[1] ){
        woe_merged <- list( woe_merged,
                            woe[1:2,] %>%
                            summarise(good_percent = sum(good_percent),
                                      bad_percent = sum(bad_percent) ) %>%
                            mutate(rank = 1,
                                   woe = log(good_percent/bad_percent),
                                   IV = (good_percent - bad_percent)*woe )
                          )
        inf_rows <- inf_rows[-1]
    }
    if (!boundary_cases[2] ){
        woe_merged <- list( woe_merged,
                            woe[c(length(breaks)-1, length(breaks)-2), ] %>%
                            summarise(good_percent = sum(good_percent),
                                      bad_percent = sum(bad_percent) ) %>%
                            mutate(rank = 1,
                                   woe = log(good_percent/bad_percent),
                                   IV = (good_percent - bad_percent)*woe )
                          )
        inf_rows <- inf_rows[-length(inf_rows)]
    }
    rm(boundary_cases)
    # Merge bins with woe = infinity
    woe_merged <- list(woe_merged,
                       lapply(inf_rows, function(z, woe) return(max_iv( woe[c(z-1,z, z+1), ] ) ), woe = woe )
                      )
    woe_merged <-
      do.call(rbind, lapply(woe_merged[[-1]], data.frame)) %>% unique %>% select(rank)
    #print(woe_merged$rank)
    # What if consecutive rows are Inf? How to handle such cases - Modifying breaks for it
    breaks <- breaks[-(woe_merged$rank+1)]
    # Re-computing woe table
    woe <- compute_woe(df = df, variable = variable, dv = dv, breaks = breaks)
    #print(0)
    # Re-computing inf_rows
    inf_rows <-
      as.numeric(woe$rank[which(woe$woe %in% c(Inf, -Inf))])
    #print(inf_rows)
    # If inf_rows are not null return else call function again
    if (!length(inf_rows)){
        #print(2)
        return(list(woe = woe, breaks = breaks))
      }else {
          #print(3)
          if(max(inf_rows) == (length(breaks)-1) ) inf_rows[which.max(inf_rows)] <- inf_rows[which.max(inf_rows)] - 1
          return(infinity_check(woe, inf_rows, breaks, dv, variable))
        }
}
