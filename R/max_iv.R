
max_iv <- function(df){
  # df contains input variables from woe dataframe - good_percent, bad_percent, rank & IV
  # df contains 3 rows. Aim : determine optimum binning out of 1,2/2,3
  iv <-
  data.frame(
    bad_percent = c( sum(df$bad_percent[1:2]), sum(df$bad_percent[2:3]) ),
    good_percent = c( sum(df$good_percent[1:2]), sum(df$good_percent[2:3]) ),
    rank = c( df$rank[1], df$rank[2]),
    IV2 = c( df$IV[3], df$IDV[1])
    ) %>%
    mutate(woe = log(good_percent/bad_percent),
           IV1 = (good_percent - bad_percent)*woe,
           IV = IV1 + IV2
          ) %>%
    select(-c(IV1, IV2)) %>%
    slice(which.max(IV)) %>%
    #select(rank) %>%
    as.data.frame %>%
    return
}
