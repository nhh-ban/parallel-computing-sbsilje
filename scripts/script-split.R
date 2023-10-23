# Assignment 1:  
library(tweedie) 
library(ggplot2)
library(doParallel)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 

# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

# Setup for parallel execution
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Modify the MTweedieTests function for parallel execution
MTweedieTests_parallel <- function(N, M, sig) {
  
  # Define the number of chunks to split M into
  chunks <- detectCores()
  
  # Number of simulations per chunk
  sims_per_chunk <- ceiling(M / chunks)
  
  # Using foreach for parallel execution
  results <- foreach(m = 1:chunks, 
                     .combine = c, 
                     .packages = "tweedie", 
                     .export = "simTweedieTest") %dopar% {
    start <- (m - 1) * sims_per_chunk + 1
    end <- min(m * sims_per_chunk, M)
    num_sims <- end - start + 1
    
    # Perform simulations for this chunk
    replicate(num_sims, simTweedieTest(N)) < sig
  }
  
  # Compute the average of results and return
  sum(results) / M
}

# Apply the modified function in the original loop
for (i in 1:nrow(df)) {
  df$share_reject[i] <- MTweedieTests_parallel(N = df$N[i], M = df$M[i], sig = .05)
}

# Close the cluster
stopCluster(cl)

# Print the data frame
df


## Assignemnt 4 

# This is one way of solving it - maybe you have a better idea? 
# First, write a function for simulating data, where the "type" 
# argument controls the distribution. We also need to ensure 
# that the mean "mu" is the same for both distributions. This 
# argument will also be needed in the t-test for the null 
# hypothesis. Therefore, if we hard code in a value here 
# we may later have an inconsistency between the mean of the 
# distributions and the t-test. So, we add it as an explicit 
# argument:  


library(magrittr)
library(tidyverse)

simDat <-
  function(N, type, mu) {
    if (type == "tweedie") {
      return(rtweedie(
        N,
        mu = mu,
        phi = 100,
        power = 1.9
      ))
    }
    if (type == "normal") {
      return(rnorm(N, mean = mu))
    }
    else{
      stop("invalid distribution")
    }
  }


# Next, the test. Note, we use mu two places:
# both for the data simulation and as the null.
simTest <-
  function(N, type, mu) {
    t.test(simDat(N = N,
                  type = type,
                  mu = mu),
           mu = mu)$p.value
  }


# Running many tests is almost the same as before.
# Here the mean is hard coded in, as we're not
# going to change it.
MTests <-
  function(N, M, type, sig) {
    sum(replicate(M,
                  simTest(
                    N = N,
                    type =
                      type,
                    mu =
                      10000
                  )) < sig) / M
  }


# We can now repeat the same analysis as before,
# but for both the tweedie and the normal:
df <-
  expand.grid(
    N = c(10, 100, 1000, 5000),
    M = 1000,
    type = c("tweedie", "normal"),
    share_reject = NA
  ) %>%
  as_tibble()


for (i in 1:nrow(df)) {
  print(i)
  df$share_reject[i] <-
    MTests(df$N[i],
           df$M[i],
           df$type[i],
           .05)
}

# As you see, with normally distributed data, N can
# be very small and the t-test is fine. With a tweedie,
# "large enough" can be many thousands. If we try
# different distributions or parameterizations, we might
# also get different results.
df %>%
  ggplot2::ggplot(aes(x = log(N), y = share_reject, col = type)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw() 