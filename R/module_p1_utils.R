#' Function to generate normal distribution ggplot
#'
#' @keywords internal
#'
#' @noRd
#'

plot_normal_distribution_with_ci <- function(mu, sigma, confidence_interval = 0.95) {
  # Define the z-values for the specified confidence intervals
  ci_values <- c(`90` = 1.645, `95` = 1.96, `99` = 2.576)

  # Check if the specified confidence interval is valid
  if (!(confidence_interval %in% c(0.90, 0.95, 0.99))) {
    stop("Invalid confidence interval. Choose either 0.90, 0.95, or 0.99.")
  }

  z <- ci_values[as.character(confidence_interval * 100)]

  # Calculate the range for the confidence interval
  ci_lower <- mu - z * sigma
  ci_upper <- mu + z * sigma
  max_density <- dnorm(mu, mean = mu, sd = sigma)

  # Create a sequence of values around the mean
  x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 1000)

  # Create a data frame containing the values and their corresponding densities
  data <- data.frame(
    value = x,
    density = dnorm(x, mean = mu, sd = sigma)
  )

  # Generate the ggplot
  ggplot(data, aes(x = value, y = density)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = density),
                data = subset(data, value >= ci_lower & value <= ci_upper),
                fill = "red", alpha = 0.4) +
    geom_segment(aes(x = mu, y = 0, xend = mu, yend = max_density),
                 color = "blue", size = 1) +
    annotate("text", x = mu, y = max(data$density), label = paste("Mean:", round(mu, 2)),
             vjust = -0.5, color = "blue") +
    annotate("text", x = ci_lower, y = 0, label = paste("CI Lower:", round(ci_lower, 2)),
             vjust = 1.5, hjust = 0, color = "red") +
    annotate("text", x = ci_upper, y = 0, label = paste("CI Upper:", round(ci_upper, 2)),
             vjust = 1.5, hjust = 1, color = "red") +
    ggtitle(paste("Normal Distribution with mean =", mu,
                  ", SD =", sigma,
                  "and", confidence_interval * 100, "% CI")) +
    xlab("Value") +
    ylab("Density") +
    theme_minimal()
}

# Example usage:
# plot_normal_distribution_with_ci(2397, 239.7, 0.95)
