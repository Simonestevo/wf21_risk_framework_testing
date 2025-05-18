
rm(list = ls())

setwd("D:/git_code/wf21_risk_framework_testing")
inputs <- read.csv("test_inputs.csv")

# test inputs where everything else remains the same but E is always medium
# to see if the consequence score is completely dictated by E
inputs <- read.csv("test_inputs.csv")

save_outputs <- FALSE

names(inputs) <- tolower(names(inputs))

# Load libraries

library(tidyverse)

# Likelihood function

calculate_likelihood <- function(likelihood_input,consequence_inputs) {
  
  names(likelihood_input) <- tolower(names(likelihood_input))

  # Confirm species spatio-temporal overlap with pressure
  
  a <- likelihood_input[likelihood_input$criteria.number == "A",]$score
  b <- likelihood_input[likelihood_input$criteria.number == "B",]$score
  c <- likelihood_input[likelihood_input$criteria.number == "C",]$score
  e <- likelihood_input[consequence_input$criteria.number == "E",]$score
  
  # Calculate likelihood score
  
  if(a == "none" | b == "none") {
    
    likelihood <- "none"
    
    return(likelihood)

  } else if(any(c(a,b,c) == "high")) {

    likelihood <- "high"

    return(likelihood)
  
  } else if(e == "high") {
    
    likelihood <- "high"
    
    return(likelihood)
    
  } else if(all(c(a,b,c) == "low")) {
    
    likelihood <- "low"
    
    return(likelihood)
    

  } else {
    
    likelihood <- "medium"
   
     return(medium)
  }
  
}

# x <- inputs |> 
#   filter(category == "Consequence") |> filter(species == "blue whale") |> 
#   filter(criteria.number != "E")

calculate_consequence <- function(x) {
  
  names(x) <- tolower(names(x))
  
  # Get scores for each criteria
  
  d <- x[x$criteria.number == "D",]$score
  e <- x[x$criteria.number == "E",]$score
  f <- x[x$criteria.number == "F",]$score
  g <- x[x$criteria.number == "G",]$score
  h <- x[x$criteria.number == "H",]$score
  
  # Calculate consequence score
  # if the population is concentrated and the impact is lethal (high) then the consequence is high
  # regardless of other criteria
  
 if(d == "high" & e == "high") {
 
    
    consequence <- "high"
    
    return(consequence)
    
  # If the impact on the individual is low (i.e. nonlethal, temporary disturbance or injury),
  # then the population consequence can only ever be low
  
  } else if (e == "low") {

    consequence <- "low"

    return(consequence)

  # Otherwise take the most frequent score from the criteria
   
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
      consequence_a <- modes[which.max(priority[modes])]

      if(consequence_a == "high" & e != "high") {

        consequence <- e
        return(consequence)

      } else {

        consequence <- consequence_a
        
        return(consequence)
      #}
      
    }
} 


calculate_pressure_score <- function(likelihood, consequence) {
  
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
# 
# likelihood_inputs <- inputs |> 
#   filter(category == "Likelihood")
# 
# likelihood_list <- likelihood_inputs %>%
#   group_by(pressure, species) %>%
#   group_split()
#   
# l <- calculate_likelihood(likelihood_list[[1]])
# 
# x <- likelihood_list[[1]] |> mutate(likelihood_score = l)
# 
# 
# # Apply consequence ----
# consequence_inputs <- inputs |> 
#   filter(category == "Consequence")
# 
# consequence_list <- consequence_inputs %>%
#   group_by(pressure, species) %>%
#   group_split()
# 
# c <- calculate_consequence(consequence_list[[1]])
# 
# y <- consequence_list[[1]] |> 
#      mutate(consequence_score = c)

# Apply pressure score ----

assessment_list <- inputs %>%
  group_by(pressure, species) %>%
  group_split()

risk_outputs <- vector(mode = "list", length = length(assessment_list))

for (i in seq_along(assessment_list)) {
  
  # Get the pressure and species for the current iteration
  pressure <- unique(assessment_list[[i]]$pressure)
  species <- unique(assessment_list[[i]]$species)
  
  # Print the pressure and species
  print(paste("Pressure:", pressure))
  print(paste("Species:", species))
  
  # Apply likelihood, consequence, and pressure score functions

likelihood_inputs <- assessment_list[[i]] |> 
  filter(category == "Likelihood")

consequence_inputS <- assessment_list[[i]] |> 
  filter(category == "Consequence")

l <- calculate_likelihood(likelihood_inputs)

c <- calculate_consequence(consequence_inputs)

risk_outputs[[i]] <- assessment_list[[i]] |> 
  mutate(likelihood_score = l,
         consequence_score = c,
         pressure_score = calculate_pressure_score(unique(likelihood_score), 
                                                   unique(consequence_score)))

}

all_risk_outputs <- bind_rows(risk_outputs)

risk_output_summary <- all_risk_outputs |> 
                       select(pressure, species, likelihood_score, consequence_score, pressure_score) |> 
                       distinct()

View(risk_output_summary)


# wide form

species_risks <- risk_output_summary |> 
  select(pressure, species, pressure_score) |> 
  pivot_wider(names_from = species, values_from = pressure_score)


if(save_outputs == TRUE) {
  
  write.csv(risk_output_summary, "test_risk_output_summary.csv", row.names = FALSE)
  
  write.csv(species_risks, "test_risk_output_wide.csv", row.names = FALSE)

}