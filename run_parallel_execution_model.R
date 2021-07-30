# parallel execution of water model

library(parallel)
library(foreach)

# create clusters
no_cores <- detectCores(logical = TRUE) - 1
cluster_object <- makeCluster(no_cores)

# define batch_function
batch_function <- function(file_i)
{
  print(paste0("Processing file: ", file_i))
  source('MainProg.R', local=TRUE)
}

clusterApply(cluster_object, 1:15, fun = batch_function)

# stop and clear the current cluster
stopCluster(cluster_object)
