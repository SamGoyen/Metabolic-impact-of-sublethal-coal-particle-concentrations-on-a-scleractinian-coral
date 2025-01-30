library(dplyr)
library(tidyr)
library(openxlsx)  # For Excel output
df <- read.csv("C:/Users/sgoyen/Desktop/Water_Quality_Stats_Sam.csv")
# Rename the first column to 'Concentration' if needed
colnames(df)[1] <- "Concentration"

# Convert Concentration to a factor
df$Concentration <- as.factor(df$Concentration)

# Extract element columns (excluding concentration)
element_columns <- colnames(df)[-1]

# Convert element columns to numeric (handling potential non-numeric values)
df[element_columns] <- lapply(df[element_columns], function(x) as.numeric(as.character(x)))

# Create an Excel workbook
wb <- createWorkbook()

# Initialize a dataframe to store Kruskal-Wallis p-values
kruskal_results <- data.frame(Element = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Perform Kruskal-Wallis test for each element
for (element in element_columns) {
  df[[element]] <- as.numeric(as.character(df[[element]]))  # Ensure numeric
  
  unique_vals <- unique(na.omit(df[[element]]))  # Get unique non-NA values
  
  if (length(unique_vals) > 1) {
    test_result <- kruskal.test(df[[element]] ~ df$Concentration)
    kruskal_results <- rbind(kruskal_results, data.frame(Element = element, P_Value = test_result$p.value))
  } else {
    kruskal_results <- rbind(kruskal_results, data.frame(Element = element, P_Value = NA))
  }
}

# Adjust Kruskal-Wallis p-values using Benjamini-Hochberg
adjusted_p_values <- p.adjust(kruskal_results$P_Value[!is.na(kruskal_results$P_Value)], method = "BH")
# Reinsert NA values in their original positions
kruskal_results$Adjusted_P_Value <- NA
kruskal_results$Adjusted_P_Value[!is.na(kruskal_results$P_Value)] <- adjusted_p_values

# Save Kruskal-Wallis results
write.csv(kruskal_results, "Kruskal_Wallis_Adjusted.csv", row.names = FALSE)

# Loop through all elements (significant or not) to save them in Excel
for (element in element_columns) {
  df[[element]] <- as.numeric(as.character(df[[element]]))  # Ensure numeric
  
  unique_vals <- unique(na.omit(df[[element]]))  # Get unique values
  
  # Create a new sheet for the element
  addWorksheet(wb, element)
  
  # Write Kruskal-Wallis results
  writeData(wb, element, "Kuskal-Wallis", startRow = 1, startCol = 1)
  writeData(wb, element, data.frame(chi_squared = ifelse(length(unique_vals) > 1, kruskal.test(df[[element]] ~ df$Concentration)$statistic, NA), 
                                    df = ifelse(length(unique_vals) > 1, kruskal.test(df[[element]] ~ df$Concentration)$parameter, NA), 
                                    p_value = kruskal_results$Adjusted_P_Value[kruskal_results$Element == element]), 
            startRow = 3, startCol = 1, colNames = TRUE)
  
  # If the element has enough variation, run Pairwise Wilcoxon test
  if (length(unique_vals) > 1) {
    pairwise_test <- pairwise.wilcox.test(df[[element]], df$Concentration, p.adjust.method = "none", exact = FALSE)
    
    # Write Pairwise Wilcoxon test results
    writeData(wb, element, "Pairwise Wilcox-test", startRow = 6, startCol = 1)
    
    # Convert the p-value matrix to a dataframe
    pairwise_df <- as.data.frame(as.table(pairwise_test$p.value))
    colnames(pairwise_df) <- c("Group1", "Group2", "P_Value")
    
    # Convert the pairwise results to wide format (matrix-style)
    pairwise_matrix <- spread(pairwise_df, key = Group2, value = P_Value)
    
    # Fill in diagonal with "NA" for better formatting
    pairwise_matrix[is.na(pairwise_matrix)] <- "NA"
    
    # Write pairwise matrix
    writeData(wb, element, pairwise_matrix, startRow = 8, startCol = 1, colNames = TRUE)
  } else {
    writeData(wb, element, "Not enough variation for pairwise Wilcoxon test", startRow = 8, startCol = 1)
  }
}

# Save the Excel file
saveWorkbook(wb, "Water_Quality_Analysis.xlsx", overwrite = TRUE)

print("Analysis complete! Results saved in 'Water_Quality_Analysis.xlsx' and 'Kruskal_Wallis_Adjusted.csv'")