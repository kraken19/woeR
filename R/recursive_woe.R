
# 1b.
mark_bins <- function(woe, woe_cutoff = 0.1, min_perc)
  {
    #print(woe_cutoff)
    woe <- woe %>%
            mutate(woe_next = lead(woe),
                   woe_change = abs((woe_next-woe)),
                   max_cum = cummax(woe),
                   check = max_cum != woe
                   )
    #woe$percent < 0.05 ; removed this condition since initial_bins = no of rows/50
    woe$conditions <- ifelse(woe$woe_change < woe_cutoff | woe$check | woe$percent < min_perc, 1, 0)
    woe$conditions[is.na(woe$conditions)] <- 0
    return(woe$conditions)
  }

recursive_woe <- function(df, variable, dv, woe , breaks , woe_cutoff = 0.1 , min_perc) {
  # Mark bins to merge
  woe$conditions <- mark_bins(woe, woe_cutoff = woe_cutoff, min_perc = min_perc)
  if(nrow(woe) == 2 | sum(woe$conditions) == 0) return(breaks)
  else{
    num <- as.numeric(min(woe[which(woe$conditions==1),"rank"]))
    #print(num)
    #print(breaks)
    if(num == (length(breaks)-1)) {num <- num - 1
    #print(num)
    }
    ###### Can change below else condition to min(abs(woe_change))
    else {
      if(num > 1) {
        #replace <- max_iv(woe[c(num-1, num+1, num)])
        num <- ifelse(abs(woe$woe[num+1] - woe$woe[num]) > abs(woe$woe[num] - woe$woe[num-1]) , num-1 , num)
        }
      }
    #print(num)
    return(recursive_woe(df,
                         variable,
                         dv,
                         compute_woe(df , variable , dv, breaks[-(num+1)] ),
                         breaks[-(num+1)],
                         woe_cutoff = woe_cutoff,
                         min_perc = min_perc
                         )
            )
    }
}
