stepAICParallel = function(object, scope, scale = 0, direction = c("both", "backward", "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, Cluster = NULL, k = 2, ...)
{
  mydeviance <- function(x, ...) {
    dev <- deviance(x)
    if (!is.null(dev)) 
      dev
    else extractAIC(x, k = 0)[2L]
  }
  cut.string <- function(string) {
    if (length(string) > 1L) 
      string[-1L] <- paste("\n", string[-1L], sep = "")
    string
  }
  re.arrange <- function(keep) {
    namr <- names(k1 <- keep[[1L]])
    namc <- names(keep)
    nc <- length(keep)
    nr <- length(k1)
    array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
                                                           namc))
  }
  step.results <- function(models, fit, object, usingCp = FALSE) {
    change <- sapply(models, "[[", "change")
    rd <- sapply(models, "[[", "deviance")
    dd <- c(NA, abs(diff(rd)))
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, abs(diff(rdf)))
    AIC <- sapply(models, "[[", "AIC")
    heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
                 "\nInitial Model:", deparse(formula(object)), "\nFinal Model:", 
                 deparse(formula(fit)), "\n")
    aod <- if (usingCp) 
      data.frame(Step = change, Df = ddf, Deviance = dd, 
                 `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC, 
                 check.names = FALSE)
    else data.frame(Step = change, Df = ddf, Deviance = dd, 
                    `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
                    check.names = FALSE)
    attr(aod, "heading") <- heading
    class(aod) <- c("Anova", "data.frame")
    fit$anova <- aod
    fit
  }
  Terms <- terms(object)
  object$formula <- Terms
  if (inherits(object, "lme")) 
    object$call$fixed <- Terms
  else if (inherits(object, "gls")) 
    object$call$model <- Terms
  else object$call$formula <- Terms
  if (use.start) 
    warning("'use.start' cannot be used with R's version of 'glm'")
  md <- missing(direction)
  direction <- match.arg(direction)
  backward <- direction == "both" | direction == "backward"
  forward <- direction == "both" | direction == "forward"
  if (missing(scope)) {
    fdrop <- numeric()
    fadd <- attr(Terms, "factors")
    if (md) 
      forward <- FALSE
  }
  else {
    if (is.list(scope)) {
      fdrop <- if (!is.null(fdrop <- scope$lower)) 
        attr(terms(update.formula(object, fdrop)), "factors")
      else numeric()
      fadd <- if (!is.null(fadd <- scope$upper)) 
        attr(terms(update.formula(object, fadd)), "factors")
    }
    else {
      fadd <- if (!is.null(fadd <- scope)) 
        attr(terms(update.formula(object, scope)), "factors")
      fdrop <- numeric()
    }
  }
  models <- vector("list", steps)
  if (!is.null(keep)) 
    keep.list <- vector("list", steps)
  n <- nobs(object, use.fallback = TRUE)
  fit <- object
  bAIC <- extractAIC(fit, scale, k = k, ...)
  edf <- bAIC[1L]
  bAIC <- bAIC[2L]
  if (is.na(bAIC)) 
    stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
  if (bAIC == -Inf) 
    stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
  nm <- 1
  Terms <- terms(fit)
  if (trace) {
    cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), 
        "\n\n", sep = "")
    utils::flush.console()
  }
  models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
                         edf, change = "", AIC = bAIC)
  if (!is.null(keep)) 
    keep.list[[nm]] <- keep(fit, bAIC)
  usingCp <- FALSE
  while (steps > 0) {
    steps <- steps - 1
    AIC <- bAIC
    ffac <- attr(Terms, "factors")
    if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata)) 
      ffac <- ffac[-st, ]
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    aod <- NULL
    change <- NULL
    if (backward && length(scope$drop)) {
      aod <- droptermParallel(fit, scope$drop, scale = scale, 
                              Cluster = Cluster, trace = max(0, trace - 1), k = k, ...)
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1L], paste("-", rn[-1L], 
                                        sep = " "))
      if (any(aod$Df == 0, na.rm = TRUE)) {
        zdf <- aod$Df == 0 & !is.na(aod$Df)
        nc <- match(c("Cp", "AIC"), names(aod))
        nc <- nc[!is.na(nc)][1L]
        ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
        if (any(is.finite(ch) & ch)) {
          warning("0 df terms are changing AIC")
          zdf <- zdf[!ch]
        }
        if (length(zdf) > 0L) 
          change <- rev(rownames(aod)[zdf])[1L]
      }
    }
    if (is.null(change)) {
      if (forward && length(scope$add)) {
        aodf <- addtermParallel(fit, scope$add, scale = scale, 
                        Cluster = Cluster, trace = max(0, trace - 1), k = k, ...)
        rn <- row.names(aodf)
        row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], 
                                           sep = " "))
        aod <- if (is.null(aod)) 
          aodf
        else rbind(aod, aodf[-1, , drop = FALSE])
      }
      attr(aod, "heading") <- NULL
      if (is.null(aod) || ncol(aod) == 0) 
        break
      nzdf <- if (!is.null(aod$Df)) 
        aod$Df != 0 | is.na(aod$Df)
      aod <- aod[nzdf, ]
      if (is.null(aod) || ncol(aod) == 0) 
        break
      nc <- match(c("Cp", "AIC"), names(aod))
      nc <- nc[!is.na(nc)][1L]
      o <- order(aod[, nc])
      if (trace) {
        print(aod[o, ])
        utils::flush.console()
      }
      if (o[1L] == 1) 
        break
      change <- rownames(aod)[o[1L]]
    }
    usingCp <- match("Cp", names(aod), 0) > 0
    fit <- update(fit, paste("~ .", change), evaluate = FALSE)
    fit <- eval.parent(fit)
    nnew <- nobs(fit, use.fallback = TRUE)
    if (all(is.finite(c(n, nnew))) && nnew != n) 
      stop("number of rows in use has changed: remove missing values?")
    Terms <- terms(fit)
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if (trace) {
      cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", 
          cut.string(deparse(formula(fit))), "\n\n", sep = "")
      utils::flush.console()
    }
    if (bAIC >= AIC + 1e-07) 
      break
    nm <- nm + 1
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
                           edf, change = change, AIC = bAIC)
    if (!is.null(keep)) 
      keep.list[[nm]] <- keep(fit, bAIC)
  }
  if (!is.null(keep)) 
    fit$keep <- re.arrange(keep.list[seq(nm)])
  step.results(models = models[seq(nm)], fit, object, usingCp)
}

if (FALSE)
{
  
  library(data.table)
  library(MASS)
  library(doParallel)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Set the number of predictors to be used
  
  NumberOfPredictors = 47
  
  # Set the seed
  
  set.seed(666)
  
  # Create a data set of potential predictors
  
  DataSet = as.data.table(matrix(data = runif(NumberOfObservations * 100), ncol = 100, nrow = NumberOfObservations))
  
  # Create a set of predictor variables 
  
  Predictors = sample(names(DataSet), NumberOfPredictors, replace = FALSE)
  
  # Create a weight variable
  
  DataSet$Weight = runif(NumberOfObservations)
  
  # Create a Poisson random variable
  
  DataSet$Poisson = round(100 * rpois(NumberOfObservations, DataSet$Weight * exp(as.matrix(DataSet[, Predictors, with = FALSE]) %*% (5 * seq(NumberOfPredictors) / sum(seq(NumberOfPredictors))))) / DataSet$Weight, 0)
  
  # Create an initial model object
  
  InterceptModel = glm(formula = as.formula('Poisson ~ 1'), family = poisson(link = 'log'), DataSet, weights = DataSet$Weight)
  
  # Create a scope for stepAIC
  
  glmFormula = as.formula(paste('Poisson ~', paste(paste('V', seq(100), sep = ''), collapse = ' + ')))
  
  # Initialize the summary table
  
  Summary = data.table()
  
  # Use step AIC
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4, trace = FALSE, steps = 61)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = NA, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
  
  # Use single threaded stepAICParallel
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = stepAICParallel(InterceptModel, scope = glmFormula, direction = 'both', k = 4, trace = FALSE, steps = 61, Cluster = NULL)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = 1, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
  
  # Use a multi threaded stepAICParallel
  
  for (j in seq(2, detectCores() - 1))
  {
    
    Cluster = makeCluster(j)
    
    StartTime = Sys.time()
    
    for (i in seq(10))
    {
      
      Results = stepAICParallel(InterceptModel, scope = glmFormula, direction = 'both', k = 4, steps = 61, Cluster = Cluster)
      
    }
    
    Summary = rbind(Summary, data.table(nthread = j, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
    
    stopCluster(Cluster)
    
  }

  showConnections()
  
  # Observe the results
  
  Summary
  
}

if (FALSE)
{
  
  library(data.table)
  library(MASS)
  library(doParallel)
  
  # Set the number of observations
  
  NumberOfObservations = 1000
  
  # Set the number of predictors to be used
  
  NumberOfPredictors = 47
  
  # Set the seed
  
  set.seed(666)
  
  # Create a data set of potential predictors
  
  DataSet = as.data.table(matrix(data = runif(NumberOfObservations * 100), ncol = 100, nrow = NumberOfObservations))
  
  # Create a set of predictor variables 
  
  Predictors = sample(names(DataSet), NumberOfPredictors, replace = FALSE)
  
  # Create a weight variable
  
  DataSet$Weight = runif(NumberOfObservations)
  
  # Create a Poisson random variable
  
  DataSet$Poisson = round(100 * rpois(NumberOfObservations, DataSet$Weight * exp(as.matrix(DataSet[, Predictors, with = FALSE]) %*% (5 * seq(NumberOfPredictors) / sum(seq(NumberOfPredictors))))) / DataSet$Weight, 0)
  
  # Create an initial model object
  
  InterceptModel = glm(formula = as.formula('Poisson ~ 1'), family = poisson(link = 'log'), DataSet, weights = DataSet$Weight)
  
  # Create a scope for stepAIC
  
  glmFormula = as.formula(paste('Poisson ~', paste(paste('V', seq(100), sep = ''), collapse = ' + ')))
  
  # Initialize the summary table
  
  Summary = data.table()
  
  # Use stepAIC
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = stepAIC(InterceptModel, scope = glmFormula, direction = 'both', k = 4, steps = 10)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = NA, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))
  
  # Use stepAICParallel with no cluster
  
  StartTime = Sys.time()
  
  for (i in seq(10))
  {
    
    Results = stepAICParallel(InterceptModel, scope = glmFormula, direction = 'both', k = 4, steps = 10, Cluster = NULL)
    
  }
  
  Summary = rbind(Summary, data.table(nthread = 1, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))

  for (j in seq(2, detectCores() - 1))
  {
    
    # Use stepAICParallel with no cluster
    
    StartTime = Sys.time()
    
    Cluster = makeCluster(j)
    
    for (i in seq(10))
    {
      
      Results = stepAICParallel(InterceptModel, scope = glmFormula, direction = 'both', k = 4, steps = 10, Cluster = Cluster)
      
    }
    
    stopCluster(Cluster)
    rm(Cluster)
    
    Summary = rbind(Summary, data.table(nthread = j, AverageDuration = as.numeric(Sys.time() - StartTime, units = 'secs') / 10))

  }
  
  showConnections()
  
  Summary

}