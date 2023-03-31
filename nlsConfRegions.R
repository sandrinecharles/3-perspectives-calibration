nlsConfRegions <-function (nls, length = 1000, exp = 1.5) 
{
  if (!inherits(nls, "nls")) 
    stop("Use only with 'nls' objects")
  data <- eval(nls$call$data, sys.frame(0))
  np <- length(coef(nls))
  nl <- nrow(data)
  vardep <- all.vars(formula(nls)[[2]])
  varindep <- intersect(all.vars(formula(nls)[[3]]), colnames(data))
  "formula2function" <- function(formu) {
    arg1 <- all.vars(formu)
    arg2 <- vector("list", length(arg1))
    names(arg2) <- arg1
    Args <- do.call("alist", arg2)
    fmodele <- as.function(c(Args, formu))
    return(fmodele)
  }
  fmodele <- formula2function(formula(nls)[[3]])
  scer <- sum(residuals(nls)^2)
  scer95 <- scer * (1 + (np/(nl - np)) * qf(p = 0.95, df1 = np, 
                                            df2 = (nl - np), lower.tail = TRUE))
  student95 <- qt(0.975, df = nl - np)
  bornes <- cbind(coef(nls) - student95 * exp * (summary(nls)$parameters)[, 
                                                                          2], coef(nls) + student95 * exp * (summary(nls)$parameters)[, 
                                                                                                                                      2])
  colnames(bornes) <- c("Lower", "Upper")
  tirage <- vector(length = np)
  names(tirage) <- row.names(summary(nls)$parameters)
  tab <- matrix(ncol = np, nrow = 0)
  rss <- vector(length = 0)
  while (nrow(tab) < length) {
    tirage <- apply(bornes, 1, function(z) runif(n = 1, min = z[1], 
                                                 max = z[2]))
    listparavar <- c(tirage, data[varindep])
    predict <- do.call("fmodele", listparavar)
    rss1 <- sum((predict - data[, vardep])^2)
    if (rss1 < scer95) {
      tenth <- floor(100 * nrow(tab)/length)
      tab <- rbind(tab, tirage)
      rss <- c(rss, rss1)
    }
  }
  rownames(tab) <- 1:nrow(tab)
  listcr <- list(cr = tab, rss = rss, rss95 = scer95, bounds = bornes)
  class(listcr) <- "nlsConfRegions"
  return(listcr)
}