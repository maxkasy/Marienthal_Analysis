trimmed_responses = function(outcome, control_variables, data, upper = T){
  # Missingness indicator
  S = !is.na(data[[outcome]])
  D = data[["town"]] == "Gramatneusiedl"
  Y = data[[outcome]]

  # Fit selection models separately by treatment
  form = as.formula(paste("S ~", paste(control_variables, collapse = " + ")))
  fit1 = glm(form, 
    cbind(S=S, data)[D==1,], family = binomial())
  # fit0 = glm(form, 
  #   cbind(S=S, data)[D==0,], family = binomial())
  # Predict ŝ1(x) and ŝ0(x)
  s1hat = predict(fit1, data, type = "response")
  # s0hat = predict(fit0, data, type = "response")
  # ratio = s1hat/s0hat

  # Create bins on s1hat among all units
  qs = unique(quantile(s1hat, probs = seq(0, 1, length.out = 11), na.rm = TRUE))
  if (length(qs) < 2L) {
    # all values identical
    bin = rep(1L, length(s1hat))
  } else {
    bin = cut(s1hat, breaks = qs, include.lowest = TRUE, labels = FALSE)
  }
  
  # indicator which observations to keep in trimmed sample
  keep = rep(F, nrow(data))
  keep[D==0] = 1
  # loop over bins, trim number of treate observations to be proportional to number of untreated obs
  for (b in unique(bin)) {
    s1bin = mean(S[bin == b & D==1])
    s0bin = mean(S[bin == b & D==0])

    n1_bin = sum(bin==b & D==1)
    n1_drop_bin = max(floor(n1_bin * (1- s0bin/s1bin)), 1)

    y1_bin_treated = Y[bin == b & D == 1 & S==1]
    y1_bin_sorted = sort(y1_bin_treated)

    if (upper) {
      cutoff = y1_bin_sorted[n1_drop_bin]
      keep[bin == b & D==1 & S == 1] = (y1_bin_treated >= cutoff)
    } else {
      cutoff = rev(y1_bin_sorted)[n1_drop_bin]
      keep[bin == b & D==1 & S == 1] = (y1_bin_treated <= cutoff)
    }
  }

  data |> filter(keep == T & S == T)
}


