## This R script generates the answers for the Statistical Inference Course Project - Part 2

## Load packages
library(datasets)
library(ggplot2)

## Load the dataset
data(ToothGrowth)

## Basic exploratory analysis of the data
str(ToothGrowth)

## Convert the dose to a Factor, as it only takes on 3 discrete values
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

## Summarize the data
summary(ToothGrowth)

## Define a labeller function that will return "nice" names for the facet labels
fips_labeller <- function(variable, value) {
    return(fips_names[value])
}

## Define the facet labels in a list (used by the above function)
fips_names <- list(
    'OJ' = "Orange Juice",
    'VC' = "Vitamin C")

## Create a boxplot of tooth length vs dose, faceting by suppliment type
g <- ggplot(ToothGrowth, aes(x = dose, y = len))

p1 <- g + geom_boxplot(aes(fill = dose)) + facet_grid(. ~ supp, labeller = fips_labeller) +
    labs(title = "Boxplot of Tooth Length by Suppliment and Dose") +
    labs(x = "Dose (mg/day)", y = "Length (microns)") +
    guides(fill = F)

print(p1)

## Use T-test to determine if dose affects tooth length (regardless of suppliment)
## Subset data and then compare dose of 0.5 mg/day to 1.0 mg/day
dose_05v10 <- subset(ToothGrowth, dose %in% c(0.5, 1))
t.test(len ~ dose, paired = F, var.equal = F, data = dose_05v10)

## Subset data and then compare dose of 1.0 mg/day to 2.0 mg/day\
dose_10v20 <- subset(ToothGrowth, dose %in% c(1, 2))
t.test(len ~ dose, paired = F, var.equal = F, data = dose_10v20)

## Use T-test to determine if suppliment type affects tooth length (hold dose constant)
## Compare suppliment type for dose = 0.5 mg/day
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 0.5, ])

## Compare suppliment type for dose = 1.0 mg/day
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 1.0, ])

## Compare suppliment type for dose = 2.0 mg/day
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 2.0, ])




## End of File