library(dplyr)
library(ggplot2)
library(readr)


dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "figures",
  recursive = TRUE,
  showWarnings = FALSE
)



set.seed(6400)

true_mean <- 25
true_sd <- 6

sample_size <- 100

n_boot <- 2000


sample_wage <- rnorm(
  n = sample_size,
  mean = true_mean,
  sd = true_sd
)

sample_mean <- mean(sample_wage)

bootstrap_means <- replicate(
  
  n_boot,
  
  mean(
    sample(
      sample_wage,
      size = sample_size,
      replace = TRUE
    )
  )
)


bootstrap_df <- data.frame(
  bootstrap_mean = bootstrap_means
)

bootstrap_ci <- quantile(
  bootstrap_means,
  probs = c(
    0.025,
    0.975
  )
)

ci_lower <- bootstrap_ci[1]
ci_upper <- bootstrap_ci[2]


cat(
  "\nSample mean:",
  round(sample_mean, 3),
  "\n"
)

cat(
  "95% Bootstrap CI:",
  round(ci_lower, 3),
  "to",
  round(ci_upper, 3),
  "\n"
)

cat(
  "True population mean:",
  true_mean,
  "\n"
)


write_csv(
  bootstrap_df,
  "data/processed/bootstrap_distribution.csv"
)

bootstrap_summary <- data.frame(
  sample_mean = sample_mean,
  ci_lower = ci_lower,
  ci_upper = ci_upper,
  true_mean = true_mean
)

write_csv(
  bootstrap_summary,
  "data/processed/bootstrap_summary.csv"
)


fig1 <- ggplot(
  bootstrap_df,
  aes(x = bootstrap_mean)
) +
  
  geom_histogram(
    bins = 40
  ) +
  
  geom_vline(
    xintercept = ci_lower,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  
  geom_vline(
    xintercept = ci_upper,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  
  geom_vline(
    xintercept = sample_mean,
    linewidth = 1
  ) +
  
  labs(
    title = "Bootstrap Distribution of the Sample Mean",
    
    subtitle = paste0(
      "2,000 bootstrap samples, n = ",
      sample_size
    ),
    
    x = "Bootstrap Mean ($ per hour)",
    
    y = "Number of Bootstrap Samples",
    
    caption = paste0(
      "Dashed lines show the 95% bootstrap CI: ",
      round(ci_lower, 2),
      " to ",
      round(ci_upper, 2)
    )
  ) +
  
  theme_minimal(
    base_size = 13
  )


ggsave(
  "figures/bootstrap_distribution.png",
  fig1,
  width = 9,
  height = 5.5,
  dpi = 300
)

print(fig1)



set.seed(6401)

n_simulations <- 500

bootstrap_per_sim <- 1000


simulate_one_ci <- function() {
  
  
  x <- rnorm(
    n = sample_size,
    mean = true_mean,
    sd = true_sd
  )
  
  
  boot_means <- replicate(
    
    bootstrap_per_sim,
    
    mean(
      sample(
        x,
        size = sample_size,
        replace = TRUE
      )
    )
  )
  
  
  ci <- quantile(
    boot_means,
    probs = c(
      0.025,
      0.975
    )
  )
  
  
  data.frame(
    sample_mean = mean(x),
    
    ci_lower = ci[1],
    
    ci_upper = ci[2],
    
    covers_true_mean =
      ci[1] <= true_mean &
      ci[2] >= true_mean
  )
}

coverage_results <- bind_rows(
  
  replicate(
    n_simulations,
    simulate_one_ci(),
    simplify = FALSE
  )
)


coverage_results <- coverage_results |>
  mutate(
    simulation = row_number()
  )


coverage_rate <- mean(
  coverage_results$covers_true_mean
)


cat(
  "\n======================================\n"
)

cat(
  "CONFIDENCE INTERVAL COVERAGE\n"
)

cat(
  "======================================\n"
)

cat(
  "Number of simulations:",
  n_simulations,
  "\n"
)

cat(
  "Intervals containing true mean:",
  sum(coverage_results$covers_true_mean),
  "\n"
)

cat(
  "Coverage rate:",
  round(
    coverage_rate * 100,
    2
  ),
  "%\n"
)


write_csv(
  coverage_results,
  "data/processed/coverage_simulation.csv"
)


plot_coverage <- coverage_results |>
  slice_head(
    n = 100
  )


fig2 <- ggplot(
  plot_coverage,
  aes(
    x = simulation,
    y = sample_mean
  )
) +
  
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper,
      linetype = covers_true_mean
    ),
    width = 0
  ) +
  
  geom_point(
    aes(
      shape = covers_true_mean
    ),
    size = 1.8
  ) +
  
  geom_hline(
    yintercept = true_mean,
    linewidth = 1
  ) +
  
  scale_linetype_manual(
    values = c(
      "TRUE" = "solid",
      "FALSE" = "dashed"
    ),
    labels = c(
      "TRUE" = "Contains true mean",
      "FALSE" = "Misses true mean"
    ),
    name = "95% CI"
  ) +
  
  scale_shape_manual(
    values = c(
      "TRUE" = 16,
      "FALSE" = 4
    ),
    labels = c(
      "TRUE" = "Contains true mean",
      "FALSE" = "Misses true mean"
    ),
    name = "95% CI"
  ) +
  
  labs(
    title = "95% Confidence Intervals Across Repeated Samples",
    
    subtitle = paste0(
      "First 100 of ",
      n_simulations,
      " simulations; overall coverage = ",
      round(
        coverage_rate * 100,
        1
      ),
      "%"
    ),
    
    x = "Simulation",
    
    y = "Estimated Mean Hourly Wage ($)",
    
    caption =
      "Horizontal line shows the true population mean of $25."
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    legend.position = "bottom"
  )


ggsave(
  "figures/ci_coverage.png",
  fig2,
  width = 10,
  height = 6,
  dpi = 300
)

print(fig2)

cat(
  "\n======================================\n"
)

cat(
  "BOOTSTRAP ANALYSIS COMPLETE\n"
)

cat(
  "======================================\n"
)

cat(
  "Figure 1: figures/bootstrap_distribution.png\n"
)

cat(
  "Figure 2: figures/ci_coverage.png\n"
)

cat(
  "Coverage rate:",
  round(
    coverage_rate * 100,
    2
  ),
  "%\n"
)