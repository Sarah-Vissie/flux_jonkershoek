climatology_pred_sub = function(input_df, climatology_pred_df) {
  return(climatology_pred_df %>%
    subset(doy %in% unique(filtered_input_df$doy)))
}
