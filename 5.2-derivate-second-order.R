# This script runs through the analysis for the paper: 
# Multi-decadal stability of fish productivity despite increasing coral reef degradation
# by Helen F. Yan and David R. Bellwood
# All code was written by Helen F. Yan 

# Load libraries ----
library(tidyverse)
library(readxl)
library(brms) # to run linear models
library(rstan) # to assess model diagnostics
library(DHARMa) # to assess model dianostics
library(tidybayes) # clean and plot bayesian models
library(patchwork) # stitching plots together
library(vegan) # run multivariate community analyses
library(ggvegan) # make vegan plots easier

# Custom functions -----------------------------------------------------------------------
# Function to manually calculate the first or second derivative from brmsfit GAMs
# Note: this matches the gratia::derivatives() function that is only compatible
# with mgcv models 

# let's write a function to calculate a specific order of derivative
# this is the function written for the models that assess the entire community
calc_derivative <- 
  function(model_level = 'community', gam_model, 
           epsilon = 0.01, order,
           min_x, max_x, smooth_term, x_term) {
    
    if (model_level == 'community') {
      
      # first, we need to extract the posterior draws for the smooth terms only
      # we'll create a new dataframe to predict from
      newdata <- 
        data.frame(x = seq(min_x, max_x, epsilon))
      
      # rename the column
      colnames(newdata) <- x_term
      
      # now we'll extract the posterior smooths
      post_values <- 
        posterior_smooths(gam_model,
                          smooth = smooth_term,
                          newdata = newdata) %>% 
        as.data.frame()
      
      # create a vector of x values to plug into the dataframe
      x_vals <- seq(min_x, max_x, epsilon)
      
      # now for the fun stuff: let's run the loop to generate the first order 
      # derivatives
      
      # create an empty list to infill
      der_list <- list()
      
      # code a progress bar
      pb_1 <- 
        txtProgressBar(min = 1, max = ncol(post_values), initial = 0, style = 3)
      
      print('Calculating first derivative...')
      
      # now let's loop through
      for (i in 1:ncol(post_values)) {
        
        # set progress bar
        setTxtProgressBar(pb_1, i)
        
        # we want to do all pairwise comparisons and include the lesser value
        # thus, we'll need to break the loop when it iterates to the larger value
        if (i < ncol(post_values)) {
          
          # select the columns that we care about from our dataframe
          in_df <- 
            post_values %>% 
            dplyr::select(i, i + 1)
          
          # now create a new dataframe to infill the list with calculated derivatives
          der_list[[i]] <- 
            tibble(first_der = (in_df[, 2] - in_df[, 1])/epsilon,
                   x = x_vals[i])
          
        } else break
      }
      
      close(pb_1)
      
      # now, let's combine all these separate dataframes into a single one
      der_list1 <- 
        do.call(rbind, der_list)
      
      # now, let's specify what to do if we decide we want the second
      # order derivative
      
      if (order == 1) {
        
        final_df <- 
          der_list1 %>% 
          mutate(order = 1)
        
        # rename the column to something more useful
        colnames(final_df)[2] <- x_term
        
        return(final_df)
        
      } else {
        
        # clean up the first derivative dataframe
        der_df2 <- 
          der_list1 %>% 
          # need to create a unique ID for each posterior iteration
          group_by(x) %>% 
          mutate(id = row_number()) %>% 
          ungroup() %>% 
          # arrange them in order
          arrange(x) %>% 
          # pivot the dataframe wider
          pivot_wider(names_from = 'x',
                      values_from = 'first_der') %>% 
          # remove the id column
          dplyr::select(-id) %>%
          # convert to dataframe for naming later on
          as.data.frame()
        
        # create an empty list to infill
        der_list2 <- list()
        
        # code a progress bar
        pb_2 <- 
          txtProgressBar(min = 1, max = ncol(der_df2), initial = 0, style = 3)
        
        print('Calculating second derivative...')
        
        # now let's loop through
        for (i in 1:ncol(der_df2)) {
          
          # set progress bar
          setTxtProgressBar(pb_2, i)
          
          # we want to do all pairwise comparisons and include the lesser value
          # thus, we'll need to break the loop when it iterates to the larger value
          if (i < ncol(der_df2)) {
            
            # select the columns that we care about from our dataframe
            in_df <- 
              der_df2 %>% 
              dplyr::select(i, i + 1)
            
            # now create a new dataframe to infill the list with calculated derivatives
            der_list2[[i]] <- 
              tibble(second_der = (in_df[, 2] - in_df[, 1])/epsilon,
                     x = x_vals[i])
            
          } else break
        }
        
        close(pb_2)
        
        final_df <- 
          do.call(rbind, der_list2) %>% 
          mutate(order = 2)
        
        # rename the column to something more useful
        colnames(final_df)[2] <- x_term
        
        return(final_df)
        
      }