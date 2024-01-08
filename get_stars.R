get_stars <-  function(pval, thresholds = c(0.05, 0.01, 0.001)) {
  
  if (is.null(thresholds)) 
    thresholds <- c(.05, .01, .001)
  
  dplyr::case_when(
    is.na(pval) ~ "",
    pval < thresholds[3] ~ "***",
    pval < thresholds[2] ~ "**",
    pval < thresholds[1] ~ "*",
    TRUE ~ ""
  )
}
