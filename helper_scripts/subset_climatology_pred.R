climatology_pred_sub = function(filtered_input_df, climatology_pred_df) {
  climatology_pred_df %>%
    subset(doy %in% unique(filtered_input_df$doy))
}