# Load libraries
library(tictoc)
library(tibble)

# Measure time elapsed for the solution as it is originally
tic()
source(file.path("scripts", "script-as-it-is.R"))
time_elapsed_as_it_is <- toc()

# Measure time elapsed for parallel computing
tic()
source(file.path("scripts", "script-parallel-computing.R"))
time_elapsed_parallel <- toc()

# Measure the time elapsed for the split of the M-simulations 
tic()
source(file.path("scripts", "script-split.R"))
time_elapsed_split <- toc()

# Create and print a tibble of the time elapsed results
time_elapsed_tibble <- tibble(
  Script = c("script_as_it_is", "script_parallel_computing", "script_split"),
  TimeElapsed_in_seconds = c(time_elapsed_as_is$toc - time_elapsed_as_is$tic,
                  time_elapsed_parallel$toc - time_elapsed_parallel$tic,
                  time_elapsed_split$toc - time_elapsed_split$tic)
)

time_elapsed_tibble

# The fastest method is the script which utilizes parallel processing 
# to distribute the computations across multiple CPU cores (script_parallel_computing). 
# This optimization significantly reduces the time required for calculations.

# The split method is also faster than the original method. It
# achieves speedup by splitting the simulations into multiple chunks, which are
# processed in parallel.

# The original method is the slowest because it does not take advantage
# of parallel processing. It relies on a sequential execution of the simulations.
# As a result, it takes the longest time to complete all calculations.

# The difference in execution time among the methods is likely due to the
# computational load and how efficiently it can be distributed across multiple
# CPU cores. Parallelization can significantly improve the performance of
# computationally intensive tasks.
