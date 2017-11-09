
merge_bins <- function(replace, df){
  # df contains two rows from woe that need to be merged.
  # replace has the replacement row info
  # Output will be a single row
  data.frame(
    rank = replace$rank,
    bad = sum(df$bad),
    total = sum(df$total),
    bad_percent = replace$bad_percent,
    good_percent = replace$good_percent,
    percent = sum(df$percent),
    woe = replace$woe,
    IV = replace$IV
    ) %>%
    return
}
