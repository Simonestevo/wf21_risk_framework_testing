
setwd("D:/git_code/wf21_risk_framework_testing")
inputs <- read.csv("test_inputs.csv")

names(inputs) <- tolower(names(inputs))

# Load libraries

library(tidyverse)

# Likelihood function

calculate_likelihood <- function(likelihood_input) {
  
  names(likelihood_input) <- tolower(names(likelihood_input))

  # Confirm species spatio-temporal overlap with pressure
  
  a <- likelihood_input[likelihood_input$criteria.number == "A",]$score
  b <- likelihood_input[likelihood_input$criteria.number == "B",]$score
  c <- likelihood_input[likelihood_input$criteria.number == "C",]$score
  
  # Calculate likelihood score
  
  if(a == "none" | b == "none") {
    
    likelihood <- "none"
    
    return(likelihood)
    
  } else if(any(c(a,b,c) == "high")) {
    
    likelihood <- "high"
    
    return(likelihood)
    
  } else if(all(c(a,b,c) == "low")) {
    
    likelihood <- "low"
    
    return(likelihood)
    
  } else {
    
    likelihood <- "medium"
    
    return(likelihood)
  } 
  
}

x <- inputs |> 
  filter(category == "Consequence") |> filter(species == "SRW") |> 
  filter(pressure == "collision (below water)")

calculate_consequence <- function(x) {
  
  names(x) <- tolower(names(x))
  
  # Get scores for each criteria
  
  d <- x[x$criteria.number == "D",]$score
  e <- x[x$criteria.number == "E",]$score
  f <- x[x$criteria.number == "F",]$score
  g <- x[x$criteria.number == "G",]$score
  h <- x[x$criteria.number == "H",]$score
  
  # Calculate consequence score
  
  if(d == "high" & e == "high") {
    
    consequence <- "high"
    
    return(consequence)
    
  } else {
    
    # Define priority
    priority <- c("low" = 1, "medium" = 2, "high" = 3)
    
    # Count occurrences
    tab <- table(x$score)
    
    # Find the max frequency
    max_count <- max(tab)
    
    # Values with the max count
    modes <- names(tab)[tab == max_count]
    
    # Choose the highest-priority value
    consequence <- modes[which.max(priority[modes])]
    
    return(consequence)
    
  } 
}

calculate_pressure_score <- function(x) {
  
  likelihood <- calculate_likelihood(x)
  
  consequence <- calculate_consequence(x)
  
  # Low concerns
  
  if(likelihood == "low" & consequence == "low") {
    
    pressure_score <- "low concern"
    
    return(pressure_score)
    
  } else if(likelihood == "medium" & consequence == "low") {
    
    pressure_score <- "low concern"
    
    return(pressure_score)
    
  } else if(likelihood == "low" & consequence == "medium") {
    
    pressure_score <- "low concern"
    
    return(pressure_score)
    
  # Medium concerns
  
  } else if(likelihood == "high" & consequence == "low") {
    
    pressure_score <- "medium concern"
    
    return(pressure_score)
    
  } else if(likelihood == "low" & consequence == "high") {
    
    pressure_score <- "medium concern"
    
  } else if(likelihood == "medium" & consequence == "medium") {
    
    pressure_score <- "medium concern"
    
    return(pressure_score)
    
  # High concerns
    
  } else if(likelihood == "high" & consequence == "medium") {
    
    pressure_score <- "high concern"
    
    return(pressure_score)
    
  }else if(likelihood == "medium" & consequence == "high") {
    
    pressure_score <- "high concern"
    
    return(pressure_score)
  
  # Very high concerns
    
  } else if(likelihood == "high" & consequence == "high") {
    
    pressure_score <- "very high concern"
    
    return(pressure_score)
    
  } else {
    
    pressure_score <- NA
    
  }
}


# Apply likelihood ----

likelihood_inputs <- inputs |> 
  filter(category == "Likelihood")

likelihood_list <- likelihood_input %>%
  group_by(pressure, species) %>%
  group_split()
  
likelihood_score_only <- calculate_likelihood(likelihood_list[[1]])

x <- likelihood_list[[1]] |> mutate(likelihood_score = likelihood_score_only)


# Apply consequence ----

consequence_inputs <- inputs |> 
  filter(category == "Consequence")

consequence_list <- consequence_inputs %>%
  group_by(pressure, species) %>%
  group_split()

consequence_score_only <- calculate_consequence(consequence_list[[1]])

y <- consequence_list[[1]] |> 
     mutate(consequence_score = consequence_score_only)

# Apply pressure score ----

assessment_list <- inputs %>%
  group_by(pressure, species) %>%
  group_split()


p_Score <- calculate_pressure_score(assessment_list[[1]])

pressure_score <- assessment_list[[1]] |> 
  mutate(pressure_score = p_Score)



