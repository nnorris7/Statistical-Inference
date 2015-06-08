## This R script generates the answers for the Statistical Inference Course Project - Part 1

## Load packages
library(ggplot2)

## Set the seed value so the results are reproducible
set.seed(7)

## Exponential distribution sample parameters
num_sims <- 1000
lambda <- 0.2
sample_size <- 40

## Calculate the theoretical mean, standard deviation, and variance
## Theoretical mean = 1 / lambda
theoretical_mean = 1 / lambda

## Theoretical standard deviation = theoretical mean / sqrt(sample_size)
theoretical_sd = theoretical_mean / sqrt(sample_size)

## Theoretical variance = theoretical standard deviation ^ 2
theoretical_variance = theoretical_sd ^ 2

## Create a matrix of the theoretical values which will be added to a table for printout
theoretical_values <- matrix(c(theoretical_mean, round(theoretical_sd, 3), round(theoretical_variance, 3)))

## Creates a matrix (1000 rows, 40 columns)
## Each row represents an exponential distribution with a sample size of 40 and lambda of 0.2
## The exponential distribution is simulated 1000 times, 1 in each row
exponentials <- matrix(rexp(num_sims * sample_size, rate = lambda), num_sims, sample_size)

## The mean of each exponential/row is calculated and stored in sample_means (1000 rows by 1 column)
sample_means <- rowMeans(exponentials)

## Calculate the sample mean, standard deviation, and variance
sample_rowMean <- mean(sample_means)
sample_rowSD <- sd(sample_means)
sample_rowVariance <- var(sample_means)

## Create a matrix of the sample values which will be added to a table for printout
sample_values <- matrix(c(round(sample_rowMean, 3), round(sample_rowSD, 3), round(sample_rowVariance, 3)))

## Create a matrix of the row names which will be added to a table for printout
variable_names <- matrix(c("Mean", "Standard Deviation", "Variance"))

## Create a summary table (data frame) to print out theoretical vs sample data
summary_table <- data.frame(cbind(variable_names, theoretical_values, sample_values))
colnames(summary_table) <- c("Variable", "Theoretical", "Sample")
print(summary_table)

## Create a histogram of the means of the sample distributions
## Adds lines for the distribution of sample means, the normal distribution, the sample mean, and theoretical mean
## Customizes colours, axes labels, x-axis scale, and adds a legend
means.df <- data.frame(Means = sample_means)

## Open the png device
png(file = "./plot1.png", width = 800, height = 600)

g1 <- ggplot(means.df, aes(x = Means)) +
    geom_histogram(aes(y = ..density..), fill = "yellow", binwidth = 1/6, color = "darkgrey", alpha = 1/3) +
    geom_density(aes(color = "Means distribution"), size = 1, show_guide = FALSE) +
    stat_function(fun = dnorm, arg = list(mean = theoretical_mean, sd = theoretical_sd), aes(color = "Normal distribution"), size = 1) +
    geom_vline(aes(xintercept = sample_rowMean, colour = "Sample mean"), size = 1) +
    geom_vline(aes(xintercept = theoretical_mean, colour = "Theoretical mean"), size = 1, linetype = "twodash") +
    theme(legend.justification = c(1.15,-1.4), legend.position = c(1,0.5)) + 
    labs(title = "1000 Sample Means Distribution", x = "Means of 40 exponential distributions (lambda = 0.2)", y = "Density") +
    scale_x_continuous(limits = c(1, 9), breaks = 1:9) +
    scale_color_discrete(name = "Annotations")

print(g1)

## Close device
dev.off()

## Create Q-Q Plot, which plots sample means quantiles vs theoretical/normal quantiles
## Adds (red) line where sample and theoretical quantiles are equal
## Open the png device
png(file = "./plot2.png", width = 800, height = 600)

g2 <- ggplot(means.df, aes(sample = Means)) + stat_qq(color = "blue", alpha = 1) +
    geom_abline(intercept = mean(means.df$Means), slope = sd(means.df$Means), color = "red") +
    labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Means Quantiles")
    
print(g2)

## Close device
dev.off()

## End of file