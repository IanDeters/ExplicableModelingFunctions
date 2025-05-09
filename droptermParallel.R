droptermParallel = function(object, scope, scale = 0, test = c("none", "Chisq", "F"), k = 2, sorted = FALSE, Cluster = NULL, trace = FALSE, ...) 
{
  x <- model.matrix(object)
  n <- nrow(x)
  asgn <- attr(x, "assign")
  tl <- attr(object$terms, "term.labels")
  if (missing(scope)) 
    scope <- drop.scope(object)
  else {
    if (!is.character(scope)) 
      scope <- attr(terms(update.formula(object, scope)), 
                    "term.labels")
    if (!all(match(scope, tl, 0L))) 
      stop("scope is not a subset of term labels")
  }
  ns <- length(scope)
  ndrop <- match(scope, tl)
  rdf <- object$df.residual
  chisq <- object$deviance
  dfs <- numeric(ns)
  dev <- numeric(ns)
  y <- object$y
  if (is.null(y)) {
    y <- model.response(model.frame(object))
    if (!is.factor(y)) 
      storage.mode(y) <- "double"
  }
  wt <- object$prior.weights
  if (is.null(wt)) 
    wt <- rep.int(1, n)
  
  if (is.null(Cluster) == TRUE)
  {
    
    for (i in seq_len(ns)) {
      if (trace) {
        message(gettextf("trying - %s", scope[i]), domain = NA)
        utils::flush.console()
      }
      ii <- seq_along(asgn)[asgn == ndrop[i]]
      jj <- setdiff(seq(ncol(x)), ii)
      z <- glm.fit(x[, jj, drop = FALSE], y, wt, offset = object$offset, 
                   family = object$family, control = object$control)
      dfs[i] <- z$rank
      dev[i] <- z$deviance
    }

  }
  
  else
  {

    # Register the cluster
    
    registerDoParallel(Cluster)

    Results = foreach(i = seq(ns), .combine = 'rbind') %dopar% {

      if (trace)
      {

        message(gettextf("trying - %s", scope[i]), domain = NA)
        utils::flush.console()

      }

      ii <- seq_along(asgn)[asgn == ndrop[i]]
      jj <- setdiff(seq(ncol(x)), ii)
      z <- glm.fit(x[, jj, drop = FALSE], y, wt, offset = object$offset,
                   family = object$family, control = object$control)

      data.frame('i' = i, dfs = z$rank, dev = z$deviance)

    }

    dev[Results$i] = Results$dev[Results$i]
    dfs[Results$i] = Results$dfs[Results$i]
    
  }
  
  scope <- c("<none>", scope)
  dfs <- c(object$rank, dfs)
  dev <- c(chisq, dev)
  dispersion <- if (is.null(scale) || scale == 0) 
    summary(object, dispersion = NULL)$dispersion
  else scale
  fam <- object$family$family
  loglik <- if (fam == "gaussian") {
    if (scale > 0) 
      dev/scale - n
    else n * log(dev/n)
  }
  else dev/dispersion
  aic <- loglik + k * dfs
  dfs <- dfs[1L] - dfs
  dfs[1L] <- NA
  aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
  aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic, row.names = scope, 
                    check.names = FALSE)
  o <- if (sorted) 
    order(aod$AIC)
  else seq_along(aod$AIC)
  if (all(is.na(aic))) 
    aod <- aod[, -3]
  test <- match.arg(test)
  if (test == "Chisq") {
    dev <- pmax(0, loglik - loglik[1L])
    dev[1L] <- NA
    nas <- !is.na(dev)
    LRT <- if (dispersion == 1) 
      "LRT"
    else "scaled dev."
    aod[, LRT] <- dev
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(Chi)"] <- dev
  }
  else if (test == "F") {
    if (fam == "binomial" || fam == "poisson") 
      warning(gettextf("F test assumes 'quasi%s' family", 
                       fam), domain = NA)
    dev <- aod$Deviance
    rms <- dev[1L]/rdf
    dev <- pmax(0, dev - dev[1L])
    dfs <- aod$Df
    rdf <- object$df.residual
    Fs <- (dev/dfs)/rms
    Fs[dfs < 1e-04] <- NA
    P <- Fs
    nas <- !is.na(Fs)
    P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail = FALSE)
    aod[, c("F value", "Pr(F)")] <- list(Fs, P)
  }
  aod <- aod[o, ]
  head <- c("Single term deletions", "\nModel:", deparse(formula(object)))
  if (scale > 0) 
    head <- c(head, paste("\nscale: ", format(scale), "\n"))
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}