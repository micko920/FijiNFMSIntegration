aaboot <- function( # A data.frame. The original accuracy assessment sample. First
                   # column gives the predicted class, second column gives the observed
                   # ('true') class.
                   aa_sample = NULL,
                   # A data.frame. First column gives the class codes, second column
                   # gives the total area mapped of the class.
                   areas_mapped = NULL,
                   # Number of bootstrap runs (i.e., iterations)
                   iterations = 1000,
                   # Progress bar?
                   progress_bar = TRUE) {
  # Check if all necessary data are provided. If not, stop!
  if (is.null(aa_sample)) {
    stop("Accuracy assessment sample is missing.")
  }
  if (is.null(areas_mapped)) {
    stop("Total areas missing (provide 'areas_mapped').")
  }

  # AA sample size in strata (strata = change class)
  n1 <- table(aa_sample[, 1])
  # Sort AA sample by change class (mapped class = predicted)
  names(aa_sample) <- c("predicted", "observed")
  aa_sample <- with(aa_sample, aa_sample[order(predicted), ])

  # Total area mapped and area weight of class i
  names(areas_mapped) <- c("lcc_code", "area_mapped")
  areas_mapped <- with(areas_mapped, areas_mapped[order(lcc_code), ])
  total_area_mapped <- sum(areas_mapped[, 2])
  weight_class_i <- areas_mapped[, 2] / total_area_mapped


  # Create data.frame that collects the results of the bootstrap runs
  Ais1 <- rep(0, length(unique(aa_sample[, 1])))
  names(Ais1) <- paste0("class_", unique(aa_sample[, 1]))

  # Vector to select rows from the AA sample (see 'n1' above)
  ns <- c(rbind(c(1, cumsum(n1) + 1)[-(length(n1) + 1)], cumsum(n1)))

  # Progress bar for simulation
  if (progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = iterations, style = 3)
  }

  # Start bootstrap...
  for (i in 1:iterations) {
    for (k in 1:length(n1)) {
      # Simple random sample with replacement from stratum k
      if (k == 1) {
        rsi <- aa_sample[sample(ns[k]:ns[k + 1], n1[k], replace = TRUE), ]
      } else {
        # Simple random sample with replacement from stratum k + 1
        rsi <- rbind(
          rsi,
          aa_sample[sample(ns[k + k - 1]:ns[k + k], n1[k],
            replace = TRUE
          ), ]
        )
      }
    }

    # Compute error matrix
    emi <- table(rsi[, 1], rsi[, 2])

    # Compute error matrix with estimated area proportions
    empi <- rep(weight_class_i, length.out = length(weight_class_i)^2) *
      (emi / rowSums(emi))

    # Estimate bias-adjusted areas for run i
    Aisi <- total_area_mapped * colSums(empi, na.rm = TRUE)

    # Bind results together
    Ais1 <- rbind(Ais1, Aisi)

    if (progress_bar == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }

  if (progress_bar == TRUE) {
    close(pb)
  }

  Ais1 <- Ais1[-1, ] # Remove first dummy row
  aab <- data.frame(Ais1) # Rename to aab
  row.names(aab) <- 1:nrow(aab) # Change row names (starting at 1)
  names(aab) <- areas_mapped[, 1]
  return(aab) # Return data frame
}
