library(car)  # For Levene's test
library(dplyr)
library(multcomp) # For Tukey's HSD

# Load dataset
data <- read.csv("C:/Users/sgoyen/Desktop/R_R_Stats.csv")

# Ensure Treatment is a factor
data$Treatment <- as.factor(data$Treatment)

# Define response variables
variables <- c("T0_unadjusted", "T0_adjusted", "TMID_unadjusted", "TMID_adjusted", "TEND_unadjusted", "TEND_adjusted")

# Function to perform ANOVA with Tukey's HSD or Welch's t-test
analyze_variable <- function(var) {
  cat("\nAnalyzing:", var, "\n")
  
  # Shapiro-Wilk test for normality
  shapiro_results <- shapiro.test(data[[var]])
  cat("Shapiro-Wilk p-value:", shapiro_results$p.value, "\n")
  
  # Levene's test for homogeneity of variances
  levene_results <- leveneTest(as.formula(paste(var, "~ Treatment")), data = data)
  cat("Levene's Test p-value:", levene_results$"Pr(>F)"[1], "\n")
  
  # If p-values > 0.05, assume normality and homogeneity, run ANOVA with Tukey's HSD
  if (shapiro_results$p.value > 0.05 & levene_results$"Pr(>F)"[1] > 0.05) {
    cat("Using ANOVA:\n")
    anova_model <- aov(as.formula(paste(var, "~ Treatment")), data = data)
    
    # Extract ANOVA table
    anova_table <- as.data.frame(anova(anova_model))
    colnames(anova_table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    
    cat("\nANOVA Results (Sum Sq, Mean Sq, Significance Levels):\n")
    print(anova_table)
    
    # Tukey's HSD post-hoc test
    cat("\nTukey's HSD Test:\n")
    tukey_results <- TukeyHSD(anova_model)
    print(tukey_results)
    
  } else {
    cat("Assumptions not met, using Welch's t-test:\n")
    pairwise_results <- pairwise.t.test(data[[var]], data$Treatment, p.adjust.method = "bonferroni", pool.sd = FALSE)
    print(pairwise_results)
  }
}

# Run analysis for each variable
for (var in variables) {
  analyze_variable(var)
}
