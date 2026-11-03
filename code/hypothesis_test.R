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


set.seed(6402)

null_mean <- 25
population_sd <- 6

sample_size <- 100

n_simulations <- 10000

alpha <- 0.05



calculate_t <- function(x, mu0) {
  
  sample_mean <- mean(x)
  
  sample_sd <- sd(x)
  
  standard_error <-
    sample_sd / sqrt(length(x))
  
  t_stat <-
    (sample_mean - mu0) /
    standard_error
  
  return(t_stat)
}



null_t_stats <- replicate(
  
  n_simulations,
  
  {
    
    x <- rnorm(
      n = sample_size,
      mean = null_mean,
      sd = population_sd
    )
    
    calculate_t(
      x,
      null_mean
    )
  }
)


null_df <- data.frame(
  t_stat = null_t_stats
)



critical_value <-
  qt(
    1 - alpha / 2,
    df = sample_size - 1
  )


cat(
  "\nCritical values:",
  round(-critical_value, 3),
  "and",
  round(critical_value, 3),
  "\n"
)



null_df <- null_df |>
  mutate(
    
    reject_region =
      abs(t_stat) >
      critical_value
    
  )


rejection_rate <- mean(
  null_df$reject_region
)


cat(
  "Share of simulated null statistics in rejection region:",
  round(
    rejection_rate * 100,
    2
  ),
  "%\n"
)


write_csv(
  null_df,
  "data/processed/null_t_distribution.csv"
)



fig3 <- ggplot(
  null_df,
  aes(
    x = t_stat,
    fill = reject_region
  )
) +
  
  geom_histogram(
    bins = 60,
    boundary = 0
  ) +
  
  geom_vline(
    xintercept = -critical_value,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  
  geom_vline(
    xintercept = critical_value,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  
  scale_fill_manual(
    values = c(
      "FALSE" = "grey70",
      "TRUE" = "grey30"
    ),
    labels = c(
      "FALSE" = "Do not reject H0",
      "TRUE" = "Reject H0"
    ),
    name = ""
  ) +
  
  labs(
    
    title =
      "Distribution of the t-Statistic Under the Null Hypothesis",
    
    subtitle =
      "H0: Mean hourly wage = $25",
    
    x =
      "Simulated t-statistic",
    
    y =
      "Number of Simulations",
    
    caption = paste0(
      "Dashed lines show the 5% two-sided rejection boundaries at ±",
      round(
        critical_value,
        2
      ),
      "."
    )
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    legend.position = "bottom"
  )


ggsave(
  "figures/null_distribution.png",
  fig3,
  width = 9,
  height = 5.5,
  dpi = 300
)


print(fig3)


# ============================================================


set.seed(6403)

observed_sample <- rnorm(
  n = sample_size,
  mean = 26.4,
  sd = population_sd
)


observed_mean <- mean(
  observed_sample
)


observed_t <- calculate_t(
  observed_sample,
  null_mean
)


cat(
  "\n======================================\n"
)

cat(
  "OBSERVED SAMPLE\n"
)

cat(
  "======================================\n"
)

cat(
  "Observed sample mean:",
  round(
    observed_mean,
    3
  ),
  "\n"
)

cat(
  "Observed t-statistic:",
  round(
    observed_t,
    3
  ),
  "\n"
)


p_value_sim <- mean(
  abs(null_t_stats) >=
    abs(observed_t)
)


cat(
  "Simulation-based p-value:",
  round(
    p_value_sim,
    4
  ),
  "\n"
)


reject_h0 <-
  p_value_sim < alpha


cat(
  "Reject H0 at 5% level:",
  reject_h0,
  "\n"
)


test_summary <- data.frame(
  
  null_mean = null_mean,
  
  observed_mean = observed_mean,
  
  observed_t = observed_t,
  
  p_value = p_value_sim,
  
  alpha = alpha,
  
  reject_h0 = reject_h0
  
)


write_csv(
  test_summary,
  "data/processed/hypothesis_test_summary.csv"
)



pvalue_df <- null_df |>
  mutate(
    
    at_least_as_extreme =
      abs(t_stat) >=
      abs(observed_t)
    
  )



fig4 <- ggplot(
  pvalue_df,
  aes(
    x = t_stat,
    fill = at_least_as_extreme
  )
) +
  
  geom_histogram(
    bins = 60,
    boundary = 0
  ) +
  
  geom_vline(
    xintercept = observed_t,
    linewidth = 1
  ) +
  
  geom_vline(
    xintercept = -observed_t,
    linewidth = 1
  ) +
  
  scale_fill_manual(
    values = c(
      "FALSE" = "grey75",
      "TRUE" = "grey25"
    ),
    labels = c(
      "FALSE" = "Less extreme",
      "TRUE" = "At least as extreme"
    ),
    name = ""
  ) +
  
  labs(
    
    title =
      "A p-Value as Tail Probability Under the Null",
    
    subtitle = paste0(
      "Observed t = ",
      round(
        observed_t,
        2
      ),
      "; simulation p-value = ",
      round(
        p_value_sim,
        3
      )
    ),
    
    x =
      "t-statistic under H0",
    
    y =
      "Number of Simulations",
    
    caption =
      "Dark regions show simulated test statistics at least as extreme as the observed statistic."
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    legend.position = "bottom"
  )


ggsave(
  "figures/p_value.png",
  fig4,
  width = 9,
  height = 5.5,
  dpi = 300
)


print(fig4)


cat(
  "\n======================================\n"
)

cat(
  "HYPOTHESIS TESTING COMPLETE\n"
)

cat(
  "======================================\n"
)

cat(
  "Figure 3: figures/null_distribution.png\n"
)

cat(
  "Figure 4: figures/p_value.png\n"
)

cat(
  "Observed t-statistic:",
  round(
    observed_t,
    3
  ),
  "\n"
)

cat(
  "Simulation p-value:",
  round(
    p_value_sim,
    4
  ),
  "\n"
)

cat(
  "Reject H0 at 5%:",
  reject_h0,
  "\n"
)