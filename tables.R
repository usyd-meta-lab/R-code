# Install required packages if not already installed
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")

# Load libraries
library(Hmisc)
library(knitr)
library(kableExtra)

# Function to create APA-style correlation table with descriptive statistics
create_correlation_table <- function(data) {
  # Calculate correlation matrix with p-values
  correlation_matrix <- rcorr(as.matrix(data), type = "pearson")
  
  # Calculate means and SDs, ignoring NA values
  means <- round(colMeans(data, na.rm = TRUE), 2)
  sds <- round(apply(data, 2, sd, na.rm = TRUE), 2)
  desc_stats <- paste0(means, " (", sds, ")")
  
  # Round correlations to 2 decimal places
  rounded_correlations <- round(correlation_matrix$r, 2)
  
  # Create empty matrix for formatted output
  n <- ncol(data)
  formatted_matrix <- matrix("", nrow = n, ncol = n)
  
  # Fill the correlation matrix (upper triangle only)
  for(i in 1:n) {
    for(j in i:n) {
      if(i == j) {
        formatted_matrix[i,j] <- "—"  # Diagonal elements
      } else {
        correlation <- rounded_correlations[i,j]
        p_value <- correlation_matrix$P[i,j]
        
        # Handle NA values in correlation output
        if(is.na(correlation)) {
          formatted_matrix[i,j] <- "NA"
        } else {
          # Add significance stars
          stars <- case_when(
            p_value < .001 ~ "***",
            p_value < .01 ~ "**",
            p_value < .05 ~ "*",
            TRUE ~ ""
          )
          
          formatted_matrix[i,j] <- paste0(correlation, stars)
        }
      }
    }
  }
  
  # Fill lower triangle with blank spaces
  for(i in 1:n) {
    for(j in 1:(i-1)) {
      formatted_matrix[i,j] <- ""
    }
  }
  
  # Convert to data frame
  formatted_table <- as.data.frame(formatted_matrix)
  
  # Add the M (SD) column as the first column
  formatted_table <- cbind('M (SD)' = desc_stats, formatted_table)
  
  # Add row/column names
  colnames(formatted_table)[-1] <- colnames(data)  # Skip the M (SD) column
  rownames(formatted_table) <- colnames(data)
  
  # Create table with kableExtra
  kable_table <- kable(formatted_table, format = "markdown", 
                       caption = "Means, Standard Deviations, and Correlations") %>%
    kable_styling(full_width = FALSE)
  
  # Print the table
  print(kable_table)
  
  # Add note about significance levels
  cat("\nNote: * p < .05, ** p < .01, *** p < .001")
  cat("\nMissing values were handled using pairwise deletion.")
  
  # Return the formatted table invisibly
  invisible(formatted_table)
}

# Example usage:
# my_data <- data.frame(
#   var1 = c(1, 2, NA, 4, 5),
#   var2 = c(2, 3, 4, NA, 6),
#   var3 = c(3, 4, 5, 6, NA)
# )
# results <- create_correlation_table(my_data)