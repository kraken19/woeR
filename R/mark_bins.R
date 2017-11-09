# 1b.
mark_bins <- function(woe, woe_cutoff = 0.05, min_perc)
  {
    woe <- woe %>%
            mutate(woe_next = lead(woe),
                   woe_change = abs((woe_next-woe)),
                   max_cum = cummax(woe),
                   check = max_cum != woe
                   )
    #woe$percent < 0.05 ; removed this condition since initial_bins = no of rows/50
    woe$conditions <- ifelse(woe$woe_change < woe_cutoff | woe$check ,1,0) #| woe$percent < min_perc
    woe$conditions[is.na(woe$conditions)] <- 0
    return(woe$conditions)
  }
