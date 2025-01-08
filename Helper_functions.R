par_summary <- function(x, digits = 3, ...){
  summ <- summary(x)
  fixef <- summ$fixed
  rand1 <- summ$random[[1]]
  rownames(rand1) <- paste0("SITE__",rownames(rand1))
  if(length(summ$random)>1){
    rand2 <- summ$random[[2]]
    rownames(rand2) <- paste0("SITE:PLOT__",rownames(rand2))
  } else{
    rand2 <- rand1[0,]
  }
  cor_pars <- summ$cor_pars
  spec_pars <- summ$spec_pars
  if("mo" %in% names(summ)){
    mo <- summ$mo
    rownames(mo) <- paste0("smplx__", rownames(mo))
    all <- do.call(rbind, list(fixef, rand1, rand2, spec_pars, mo))
  } else{
    all <- do.call(rbind, list(fixef, rand1, rand2, spec_pars))
  }
  all[,c("Bulk_ESS","Tail_ESS")] <- floor(all[,c("Bulk_ESS","Tail_ESS")])
  knitr::kable(all, digits = digits, ...)
}

resid_bysite <- function(x, coldat = regions, ...){
  res <- resid(x)
  dat <- x$data
  plotdat <- cbind(dat, res) %>%
    left_join(mutate(coldat, SITE = as.character(SITE)),
              by = "SITE")
  p1 <- ggplot(plotdat, aes(y = Estimate)) +
    geom_boxplot(aes(x = SITE)) +
    facet_wrap(~REGION, scales = "free_x") +
    geom_hline(yintercept = 0) + labs(x = "Site", y = "Estimated Residual") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0))
  p2 <- ggplot(plotdat, aes(y = Estimate)) +
    stat_summary(aes(x = REGION), fun.data = "mean_se", 
                 fun.args = list(mult = 1.96)) +
    geom_hline(yintercept = 0) + 
    labs(x = "Region", y = "Estimated Residual") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  gridExtra::grid.arrange(p1,p2, nrow= 1, widths = c(2,1))

}

resid_bysite_mult <- function(x, coldat = regions, ...){
  res <- resid(x)
  var_names <- dimnames(res)[[3]]
  pl_list <- lapply(var_names, function(i){
    res_l <- res[,,i]
    dat <- x$data
    plotdat <- cbind(dat, res_l) %>%
      left_join(mutate(coldat, SITE = as.character(SITE)),
                by = "SITE")
    p1 <- ggplot(plotdat, aes(y = Estimate)) +
      geom_boxplot(aes(x = SITE)) +
      facet_wrap(~REGION, scales = "free_x") +
      geom_hline(yintercept = 0) + 
      labs(x = "Site", y = "Estimated Residual",
           title = i) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0))
    p2 <- ggplot(plotdat, aes(y = Estimate)) +
      stat_summary(aes(x = REGION), fun.data = "mean_se", 
                   fun.args = list(mult = 1.96)) +
      geom_hline(yintercept = 0) + 
      labs(x = "Region", y = "Estimated Residual") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    pa <- gridExtra::grid.arrange(p1,p2, nrow= 1, widths = c(2,1))
  })
  
  
}
