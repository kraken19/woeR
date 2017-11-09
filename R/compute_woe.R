
compute_woe <- function(df, variable, dv, breaks){
    df$del <- df[, dv]
    df$rank <-
      as.numeric( as.character(cut(df[, variable], breaks = breaks, include.lowest = T, labels = 1:(length(breaks)-1))))
    woe <-
      df %>%
        group_by(rank) %>%
        summarise(bad = sum(del) , total = n()) %>%
        mutate(good = total - bad , bad_percent = bad/sum(bad) , good_percent = good/sum(good), percent = total/sum(total),
               woe = log(good_percent/bad_percent) , woe_next = lead(woe) ,woe_change = abs((woe_next-woe)) ) %>%
        ungroup() %>%
        mutate(max_cum = cummax(woe), check = max_cum != woe, IV = (good_percent - bad_percent)*woe ) %>%
        select(-c(good)) %>%
        as.data.frame()
    #woe$conditions <- mark_bins(woe, woe_cutoff = woe_cutoff)
    #print(woe)
    #print(breaks)
    return(woe[,c(1:7, 12)])
    #| woe$good_percent < minBins | woe$bad_percent < minBins
}
