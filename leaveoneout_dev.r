rm(list=ls())
setwd("~/Library/CloudStorage/OneDrive-CardiffUniversity/Documents/Research/Papers/Stress testing null models/Data")
library(econullnetr); library(stringr)

## Leave one out (loo) function for null models

# Permute the econullnetr function "generate_null_net" whilst leaving out 
# one of the consumers to see its influence on the modelling

loo_simulations <- function(consumers, resources, sims, 
                                    data.type = "counts", summary.type = "sum"){ 
  
  ## Setup
  output <- list(NULL)
  nreps <- 1:(ncol(consumers)-1)
  
  ## Run the null network analysis for the original network
  
  # Null network generation
  invisible(capture.output(null_net_original <- generate_null_net(consumers,
                                                                  resources,
                                                                  sims, 
                                                                  data.type = data.type,
                                                                  summary.type = summary.type)))
  
  # Record the results
  output[[1]] <- list(rand.interactions = null_net_original$rand.data, 
                      obs.interactions = null_net_original$obs.interactions,
                      effect.data = test_interactions(null_net_original), 
                      n.iterations = sims)
  
  ## Run the null network analysis for the altered networks
  
  # Loop through the various permutations removing each consumer
  for (i in nreps){ 
    con <- unique(consumers$Taxon)[i]
    consumers_adj <- consumers[consumers$Taxon != con,]
    invisible(capture.output(null_net <- generate_null_net(consumers_adj,
                                                           resources,
                                                           sims, 
                                                           data.type = data.type,
                                                           summary.type = summary.type)))
 
    output[[i+1]] <- list(rand.interactions = null_net$rand.data, 
                          obs.interactions = null_net$obs.interactions,
                          effect.data = test_interactions(null_net), 
                          n.iterations = sims)
    
    print(paste(con, "removed", sep = " "))
    
    }
  
  ## Store the overal results
  names(output) <- c("original", paste(unique(consumers$Taxon), "removed", sep = "_"))
  class(output) <- "loo"
  return(output)
  
  }

loo_analysis <- function(loo, metrics = "all"){
  
  # Setup 
  ses_diff <- list(NULL) 
  mse <- list(NULL)
  output <- list(NULL)
  
  # Extract the effect data from the original model
  original_ses <- loo$original$effect.data
  
  for (i in 2:length(loo)){ 
    
    compare_ses <- loo[[i]]$effect.data
    original_ses_adj <- original_ses[original_ses$Consumer != 
                                       str_extract(names(loo)[i],
                                                   "[^_]+"),]
    
    ses_diff[[i-1]] <- (original_ses_adj$SES - compare_ses$SES)^2
    mse[[i-1]] <- (1/length(which(!is.na(ses_diff[[i-1]]))))*(sum(ses_diff[[i-1]], na.rm = T))
    
    output[[i-1]] <- list(ses_differences = ses_diff[[i-1]], mse = mse[[i-1]])
    }

  names(output) <- unique(original_ses$Consumer)
  res.table <- data.frame(species.removed = names(loo)[-1], mse = unlist(mse))
    
  loo.res <- list(res.table = res.table, ses_differences = ses_diff)
  return(loo.res)

}



## Test the functions

# Read in the data
consumers <- read.csv("econullnetr ants.csv")
resources <- read.csv("econull resources ants.csv")

# Try out the simulation function
ant_simulations <- loo_simulations(consumers = consumers, resources = resources,
                                   sims = 100, data.type = "counts", 
                                   summary.type = "sum")

# Try out the analysis function
ant_analysis <- loo_analysis(ant_simulations)






