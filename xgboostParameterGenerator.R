xgboostParameterGenerator = function(FixedParameters, ParameterBoundList)
{
  
  # Obtain the parameters for the tree booster
  
  VaryingParameters = list(
    
    booster = ParameterBoundList$booster[sample(seq(length(ParameterBoundList$booster)), 1)],
    eta = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['eta']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['eta']])),
    lambda = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['lambda']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['lambda']])),
    alpha = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['alpha']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['alpha']])),
    scale_pos_weight = 1
    
  )
  
  if (VaryingParameters$booster %in% c('gbtree', 'dart'))
  {
    
    VaryingParameters = c(VaryingParameters, list(
      
      colsample_bynode = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['colsample_bynode']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['colsample_bynode']])),
      colsample_bylevel = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['colsample_bylevel']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['colsample_bylevel']])),
      colsample_bytree = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['colsample_bytree']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['colsample_bytree']])),
      gamma = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['gamma']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['gamma']])),
      grow_policy = 'depthwise',
      max_bin = seq(min(ParameterBoundList[['TreeBoosterParameters']][['max_bin']]), max(ParameterBoundList[['TreeBoosterParameters']][['max_bin']]))[sample(x = max(ParameterBoundList[['TreeBoosterParameters']][['max_bin']]) - min(ParameterBoundList[['TreeBoosterParameters']][['max_bin']]) + 1, 1)],
      max_delta_step = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['max_delta_step']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['max_delta_step']])),
      max_depth = seq(min(ParameterBoundList[['TreeBoosterParameters']][['max_depth']]), max(ParameterBoundList[['TreeBoosterParameters']][['max_depth']]))[sample(x = max(ParameterBoundList[['TreeBoosterParameters']][['max_depth']]) - min(ParameterBoundList[['TreeBoosterParameters']][['max_depth']]) + 1, 1)],
      max_leaves = 0,
      min_child_weight = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['min_child_weight']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['min_child_weight']])),
      refresh_leaf = 1, 
      process_type = 'default',
      sampling_method = c('uniform'),
      subsample = runif(1, min = min(ParameterBoundList[['TreeBoosterParameters']][['subsample']]), max = max(ParameterBoundList[['TreeBoosterParameters']][['subsample']])),
      tree_method = sample(ParameterBoundList[['TreeBoosterParameters']][['tree_method']], 1),
      num_parallel_tree = seq(min(ParameterBoundList[['TreeBoosterParameters']][['num_parallel_tree']]), max(ParameterBoundList[['TreeBoosterParameters']][['num_parallel_tree']]))[sample(x = max(ParameterBoundList[['TreeBoosterParameters']][['num_parallel_tree']]) - min(ParameterBoundList[['TreeBoosterParameters']][['num_parallel_tree']]) + 1, 1)]

    ))

    # If the tree_method parameter is 'exact', make sure that a non - zero value is given for the max_depth parameter
    
    VaryingParameters[['max_depth']] = ifelse((VaryingParameters[['tree_method']] %in% c('auto', 'exact')) & (VaryingParameters[['max_depth']] == 0), max(ParameterBoundList[['TreeBoosterParameters']][['max_depth']]), VaryingParameters[['max_depth']])

    # Set the parameters for the Dart booster
    
    if (VaryingParameters$booster == 'dart')
    {
      
      VaryingParameters = c(VaryingParameters, list(
        
        sample_type = sample(ParameterBoundList[['dart']][['sample_type']], 1),
        normalize_type = sample(ParameterBoundList[['dart']][['normalize_type']], 1),
        rate_drop = runif(1, min = min(ParameterBoundList[['dart']][['rate_drop']]), max = max(ParameterBoundList[['dart']][['rate_drop']])),
        one_drop = seq(min(ParameterBoundList[['dart']][['one_drop']]), max(ParameterBoundList[['dart']][['one_drop']]))[sample(x = max(ParameterBoundList[['dart']][['one_drop']]) - min(ParameterBoundList[['dart']][['one_drop']]) + 1, 1)],
        skip_drop = runif(1, min = min(ParameterBoundList[['dart']][['skip_drop']]), max = max(ParameterBoundList[['dart']][['rate_drop']]))
        
      ))
      
    }
    
  }
  
  # Set the parameters for the Linear booster
  
  if (VaryingParameters$booster == 'gblinear')
  {
    
    VaryingParameters = c(VaryingParameters, list(
      
      updater = sample(ParameterBoundList[['gblinear']][['updater']], 1),
      feature_selector = sample(ParameterBoundList[['gblinear']][['feature_selector']], 1),
      top_k = seq(min(ParameterBoundList[['gblinear']][['top_k']]), max(ParameterBoundList[['gblinear']][['top_k']]))[sample(x = max(ParameterBoundList[['gblinear']][['top_k']]) - min(ParameterBoundList[['gblinear']][['top_k']]) + 1, 1)]
      
    ))
    
  }
  
  # Return the parameters
  
  c(FixedParameters, VaryingParameters)
  
}

if (FALSE)
{
  
  # Create the list of fixed parameters
  
  FixedParameters = list(
    
    # General Parameters
    
    verbosity = 1, # c(0, 1, 2, 3)
    validate_parameters = TRUE,
    nthread = 7, # detectCores()
    disable_default_eval_metric = FALSE,
    
    # Learning task parameters
    
    objective = 'reg:squarederror', 
    eval_metric = 'rmse',
    seed_per_iteration = FALSE)
  
  # Define the sets from which the varying parameters for the hyper parameter optimization will be pulled
  
  ParameterBoundList = list(
    
    booster = c('gbtree', 'gblinear', 'dart'),
    #booster = c('gbtree'),
    
    # Parameters for Tree Booster
    
    TreeBoosterParameters = list(
      
      eta =  c(0, 1), # [0, 1], .3
      gamma = c(0, 10), # [0, Inf), 0
      max_depth = c(1, 10), # [0, Inf), 6
      min_child_weight = c(0, 10), # (0, Inf), 1
      # max_delta_step = c(0, 1), # (0, Inf), 0
      subsample = c(0, 1), # (0, 1], 1
      sampling_method = c('uniform'),
      # colsample_bynode = c(0, 1), # (0, 1], 1
      # colsample_bylevel = c(0, 1), # (0, 1], 1
      # colsample_bytree = c(0, 1), # (0, 1], 1
      lambda = c(.5, 10), # [0, Inf), 1
      alpha = c(0, 1), # [0, Inf), 0
      tree_method = c('auto', 'exact', 'approx', 'hist'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
      scale_pos_weight = 1,
      refresh_leaf = 1, # 1, c(0, 1)
      process_type = 'default', # 'default', c('default', 'update')
      grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
      max_leaves = 0, # 0, [0, Inf)
      max_bin = c(10, 250), #256, [1, Inf)
      num_parallel_tree = c(1, 10), #1, [1, Inf)
      
      # eta = c(0, 1), # [0, 1], .3
      # gamma = c(0, 0), # [0, Inf), 0
      # max_depth = c(2, 2), # [0, Inf), 6
      # min_child_weight = c(1, 1), # (0, Inf), 1
      max_delta_step = c(0, 0), # (0, Inf), 0
      # subsample = c(1, 1), # (0, 1], 1
      # sampling_method = c('uniform'),
      colsample_bynode = c(1, 1), # (0, 1], 1
      colsample_bylevel = c(1, 1), # (0, 1], 1
      colsample_bytree = c(1, 1) # (0, 1], 1
      # lambda = c(1, 1), # [0, Inf), 1
      # alpha = c(0, 0), # [0, Inf), 0
      # tree_method = c('auto'), # c('auto', 'exact', 'approx', 'hist'), 'auto'
      # scale_pos_weight = 1, # 1
      # refresh_leaf = 1, # 1, c(0, 1)
      # process_type = 'default', # 'default', c('default', 'update')
      # grow_policy = 'depthwise', # 'depthwise', c('depthwise', 'lossguide')
      # max_leaves = 0, # 0, [0, Inf)
      # max_bin = c(256, 256), #256, [1, Inf)
      # num_parallel_tree = c(1, 1) #1, [1, Inf)
      
    ),
    
    dart = list(
      
      # Additional parameters for Dart Booster
      
      sample_type = c('uniform', 'weighted'), # 'uniform', c('uniform', 'weighted')
      normalize_type = c('tree', 'forest'), # 'tree', c('tree', 'forest')
      rate_drop = c(0, 1), # 0, [0, 1]
      one_drop = c(0, 1), # 0, c(0, 1)
      skip_drop = c(0, 1) # 0, [0, 1]
      
    ),
    
    gblinear = list(
      
      # Parameters for Tree Booster  
      
      updater = 'coord_descent',
      feature_selector = c('cyclic', 'greedy', 'thrifty'), # 'cyclic', c('cyclic', 'shuffle', 'random', 'greedy', 'thrifty')
      top_k = c(1, 386) # 0
      
    )
    
  )
  
  # Set the seed
  
  set.seed(666)
  
  # Create the total list of parameters

  xgboostParameterGenerator(FixedParameters = FixedParameters, ParameterBoundList = ParameterBoundList)

}