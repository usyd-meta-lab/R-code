# Install required packages if not already installed
if (!require("Hmisc")) install.packages("Hmisc")
if (!require("knitr")) install.packages("knitr")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("dplyr")) install.packages("dplyr")

# Load libraries
library(Hmisc)
library(knitr)
library(kableExtra)
library(dplyr)

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






# Function to calculate means, SDs, and t-tests by group
calculate_group_descriptives <- function(data, vars, group_var) {
  # Initialize results list
  results <- list()
  
  # Calculate descriptives for each variable
  for (var in vars) {
    # Split data by group
    group_stats <- tapply(data[[var]], data[[group_var]], function(x) {
      c(mean = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        n = sum(!is.na(x)))
    })
    
    # Format results for each group
    formatted_stats <- sapply(group_stats, function(x) {
      paste0(round(x["mean"], 2),
             " (",
             round(x["sd"], 2),
             ")")
    })
    
    # Perform t-test
    t_test <- t.test(data[[var]] ~ data[[group_var]])
    
    # Combine results
    row_result <- c(
      formatted_stats,
      `p-value` = format.pval(t_test$p.value, digits = 3),
      `t-statistic` = round(t_test$statistic, 2)
    )
    
    results[[var]] <- row_result
  }
  
  # Convert to data frame
  result_df <- do.call(rbind, results)
  result_df <- as.data.frame(result_df)
  
  # Add variable names as row names
  rownames(result_df) <- vars
  
  return(result_df)
}

# Example usage:
# my_data <- data.frame(
#   group = rep(c("Control", "Treatment"), each = 5),
#   age = c(25, 30, 35, 40, 45, 28, 32, 37, 42, 47),
#   height = c(170, 175, 168, 172, 169, 171, 176, 169, 173, 170),
#   weight = c(70, 75, 68, 73, 71, 72, 77, 70, 75, 73)
# )
# 
# vars_to_analyze <- c("age", "height", "weight")
# results_table <- calculate_group_descriptives(my_data, vars_to_analyze, "group")
# print(results_table)






independent_t_test_apa <- function(formula, data, var.equal = TRUE) {
  # Create a model frame from the formula and data
  mf <- model.frame(formula, data = data)
  
  # Check that the model frame has exactly 2 columns: outcome and grouping variable
  if (ncol(mf) != 2) {
    stop("The formula should be of the form 'outcome ~ group'")
  }
  
  # Extract outcome and group; ensure group is a factor
  outcome <- mf[[1]]
  group <- mf[[2]]
  if (!is.factor(group)) {
    group <- factor(group)
  }
  
  # Ensure there are exactly 2 groups
  if (length(levels(group)) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }
  
  # Split the outcome variable by group
  split_data <- split(outcome, group)
  group1 <- split_data[[1]]
  group2 <- split_data[[2]]
  
  # Calculate means and standard deviations for each group
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  sd1 <- sd(group1, na.rm = TRUE)
  sd2 <- sd(group2, na.rm = TRUE)
  
  # Perform the t-test using the formula interface
  ttest <- t.test(formula, data = data, var.equal = var.equal)
  t_stat <- ttest$statistic
  p_value <- ttest$p.value
  df_val <- ttest$parameter
  
  # Compute Cohen's d
  if (var.equal) {
    n1 <- length(group1)
    n2 <- length(group2)
    pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    cohen_d <- (mean1 - mean2) / pooled_sd
  } else {
    # When variances are not assumed equal, one common approach is to average the variances
    cohen_d <- (mean1 - mean2) / sqrt((sd1^2 + sd2^2) / 2)
  }
  
  # Format the p-value for APA reporting
  if (p_value < .001) {
    p_str <- "< .001"
  } else {
    p_str <- paste0("= ", format(round(p_value, 3), nsmall = 3))
  }
  
  # Format degrees of freedom: round if Welch's test is used
  if (var.equal) {
    df_str <- as.character(df_val)
  } else {
    df_str <- as.character(round(df_val, 2))
  }
  
  # Determine wording based on significance
  significance <- ifelse(p_value < 0.05, "significantly", "non-significantly")
  
  # Create an APA formatted result string (rounding values for clarity)
  result_str <- sprintf("An independent-samples t-test was conducted to compare the groups. %s (M = %.2f, SD = %.2f) and %s (M = %.2f, SD = %.2f) differed %s, t(%s) = %.2f, p %s, Cohen's d = %.2f.",
                        levels(group)[1], mean1, sd1,
                        levels(group)[2], mean2, sd2,
                        significance,
                        df_str, t_stat, p_str, cohen_d)
  
  return(result_str)
}


# Example Usage

# Run the function using a formula
# result <- independent_t_test_apa(score ~ group, data = mydata, var.equal = TRUE)
# cat(result)
