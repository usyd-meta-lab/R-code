# A Function to Create the Frequency Tables Needed for meta d'


meta_d_table <- function(data, n_conf = 6){
  
  
  
  # nr_s1
  nr_s1 <- data[data$target_left == 1,]
  nr_s1$response <- ifelse(nr_s1$response == "w", "responded s1", "responded s2")
  
  nr_s1 <- data.frame(table(nr_s1$response, factor(nr_s1$confidence, levels = 1:6)))
  nr_s1$Var2 <- as.numeric(as.character(nr_s1$Var2))
  # Custom sorting function
  sorted_df_nr_s1 <- nr_s1[order(
    nr_s1$Var1,
    ifelse(nr_s1$Var1 == "responded s1", -nr_s1$Var2, nr_s1$Var2)  # Reverse sorting for "responded s2"
  ), ]
  
  print(sorted_df_nr_s1)
  
  
  # nr_s2
  nr_s2 <- data[data$target_left == 0,]
  nr_s2$response <- ifelse(nr_s2$response == "w", "responded s1", "responded s2")
  
  nr_s2 <- data.frame(table(nr_s2$response, factor(nr_s2$confidence, levels = 1:n_conf)))
  nr_s2$Var2 <- as.numeric(as.character(nr_s2$Var2))
  
  # Custom sorting function
  sorted_df_nr_s2 <- nr_s2[order(
    nr_s2$Var1,
    ifelse(nr_s2$Var1 == "responded s1", -nr_s2$Var2, nr_s2$Var2)  # Reverse sorting for "responded s2"
  ), ]
  
  print(sorted_df_nr_s2)
  
  return(list(nr_s1 = sorted_df_nr_s1$Freq, nr_s2 = sorted_df_nr_s2$Freq ))
  
}