# Blog Post 8: Simulation and Statistical Inference

This project uses simulation to explain **bootstrap confidence intervals, confidence interval coverage, hypothesis testing, rejection regions, and p-values**.

The example assumes a simulated population with:

- Mean hourly wage = **$25**
- Standard deviation = **$6**

## Analysis

`bootstrap_ci.R`:

- Draws a random sample
- Constructs a bootstrap distribution
- Creates a 95% bootstrap confidence interval
- Uses repeated simulation to examine confidence interval coverage

`hypothesis_test.R`:

- Simulates the distribution of a t-statistic under the null hypothesis
- Shows the 5% rejection regions
- Calculates a simulation-based p-value
- Demonstrates when the null hypothesis should or should not be rejected


## Main Takeaway

Simulation makes statistical inference easier to understand by showing how confidence intervals and hypothesis tests behave over repeated samples.

A 95% confidence interval refers to the long-run coverage of the procedure, while a p-value measures how unusual the observed test statistic would be if the null hypothesis were true.
