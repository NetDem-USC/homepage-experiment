display <- function(num, pct=FALSE){
  if (!pct) return(sprintf("%.3f", num))
  if (pct) return(sprintf("%.1f", num*100))
}

run_model <- function(dv, dv_pre = NULL, trt = "HuffPost", control=NULL,
                      data = svy, seed=123, blocks="W3_Browser_treatment_w3",
                      more_vars = NULL, verbose=TRUE) {
  set.seed(seed)
  if(is.null(control) & trt == "FoxNews") { droptrt <- "HuffPost" }
  if(is.null(control) & trt == "HuffPost") { droptrt <- "FoxNews" }
  if(!is.null(control) & trt %in% c("FoxNews", "HuffPost")) { droptrt <- "Control" }
  
  svy_na <- na.omit(data[, c(vars, dv, dv_pre, more_vars)])
  svy_model <- model.matrix(~., svy_na[, c(vars, dv_pre, more_vars)])
  
  lasso_select <- cv.glmnet(x=svy_model,
                            y=as.vector(get(dv, svy_na)),
                            alpha=1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  inds <- which(coef.out != 0)
  
  lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
  if(length(inds) == 1) {
    if(!is.null(blocks)) {
      lin_cov <- paste0(" ~ ", blocks)
    } else {
      lin_cov <- NULL
    }
  } else {
    incl_vars <- grep("(Intercept)", rownames(coef.out)[inds], value = TRUE, invert = TRUE)
    for(v in 1:length(incl_vars)) {
      # if glmnet pulls out a single factor level from a factor var, throw in entire variable
      if(!incl_vars[v] %in% names(data) & !grepl('state|raceeth', incl_vars[v])) { 
        c <- unlist(sapply(1:length(names(data)), function(x) if(grepl(names(data)[x], incl_vars[v])) x))
        incl_vars[v] <- names(data)[tail(unique(c), 1)]
      }
      if(grepl('state', incl_vars[v])){
        incl_vars[v] <- paste0("I(state==", gsub("state", "", incl_vars[v]), ")")
      }
      if(grepl('raceeth', incl_vars[v])){
        incl_vars[v] <- paste0("I(raceeth==", gsub("raceeth", "'", incl_vars[v]), "')")
      }      
    }
    if(!is.null(blocks)){
      lin_cov <- paste0(" ~ ", blocks, " + ", paste(incl_vars, collapse = " + "))
    } else {
      lin_cov <- paste0(" ~ ", paste(incl_vars, collapse = " + "))
    }
  }
  if(!is.null(lin_cov)){
    lin_model <- lm_lin(lin_formula, covariates = formula(lin_cov), data = dplyr::filter(data, W3_PATA306_treatment_w3 != droptrt))
  } else{
    lin_model <- lm_robust(lin_formula, data = dplyr::filter(data, W3_PATA306_treatment_w3 != droptrt))
  }
  sig <- ifelse(abs(lin_model$statistic[2]) >= 1.96, "STARS!", "")
  
  if(verbose){
    message("Estimate: ", display(coef(lin_model)[2]))
    message("Std. Error: ", display(sqrt(vcov(lin_model)[2,2])))
    message("CI Lower: ", display(confint(lin_model)[2,1]))
    message("CI Upper: ", display(confint(lin_model)[2,2]))
  }
  # only pre-treatment DV
  if (!is.null(dv_pre)){
    lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3 + ", 
                                  dv_pre))
    ss <- summary(lm(lin_formula, data=as.data.frame(data), 
                     subset=W3_PATA306_treatment_w3 != droptrt)) 
    message("Pre-treatment DV, Adj R2 = ", display(ss$adj.r.squared))
    message("N = ", length(ss$residuals))
  }
  # Lasso covariates
  if(length(inds) > 1){
    lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3 + ", 
                                  paste(incl_vars, collapse = " + ")))
    ss <- summary(lm(lin_formula, data=as.data.frame(data), 
                     subset=W3_PATA306_treatment_w3 != droptrt))
    message("Lasso covariates, Adj R2 = ", display(ss$adj.r.squared))
    message("Covariates: ", paste(incl_vars, collapse = " + "))
    message("N = ", length(ss$residuals))
  }
  # all variables
  lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3 + ", 
                                paste(c(vars, dv_pre), collapse = " + ")))
  ss <- summary(lm(lin_formula, data=as.data.frame(data), 
                   subset=W3_PATA306_treatment_w3 != droptrt)) 
  message("All covariates, Adj R2 = ", display(ss$adj.r.squared)) 
  message("Covariates: ", paste(vars, collapse = " + "))
  message("N = ", length(ss$residuals))
  
  return(list(lin_model, sig, lin_cov))
}

extract_covariates <- function(mod){
  coefs <- names(coef(mod[[1]]))
  coefs <- gsub("_c$|Firefox_c", "", coefs)
  coefs <- coefs[coefs %in% names(svy)]
  return(coefs)
}

estimate_cace <- function(Y, D, Z, X, trt, control="Control", data = svy){
  form <- formula(paste0(Y, " ~ ", D, " + ", paste(X, collapse=" + "),
                         " | ", Z, " + ", paste(X, collapse=" + ")))
  dt <- filter(data, W3_PATA306_treatment_w3 %in% c(control, trt))
  mod <- iv_robust(form, data=dt)
  return(mod)
}

# to determine whether we need to run multiple imputation, compute the
# % of observations with at least one missing value in the covariates.
# Note that missing values in DV are not included in the estimation.
# If % of missing values is above 20%, then we will do multiple imputation.
compute_proportion_missing_covars <- function(mod, more_info=FALSE){
  # mod = output from run_model()
  
  # identify labels for covariates and dv
  covars <- attr(mod[[1]]$terms, "term.labels")
  dv <- mod[[1]]$outcome
  
  # replacing state dummies with state factor variable
  if (sum(grepl('state', covars))>0){
    toreplace <- grep('state', covars)
    covars[toreplace] <- 'state'
    covars <- unique(covars)
  }
  # replacing race dummies with race factor variable
  if (sum(grepl('raceeth', covars))>0){
    toreplace <- grep('raceeth', covars)
    covars[toreplace] <- 'raceeth'
    covars <- unique(covars)
  }  
  
  # extract relevant variables from svy file
  dd <- svy[,c(dv, covars)]
  
  # identify rows where DV is not missing
  no_miss_dv <- which(!is.na(dd[,dv]))
  
  # identify rows where at least one covariate has a missing value
  miss_covars <- apply(svy[no_miss_dv,covars], 1, function(x) any(is.na(x)))
  
  # print % of missing values
  message(display(mean(miss_covars), pct=TRUE), "% missing")
  
  if (more_info){
    print(summary(svy[no_miss_dv,covars]))
  }
  
  return(invisible(mean(miss_covars)))
}

# Compute power analysis for DIM, ITT and CACE
power2 <- function(dv, dim, itt, cace, covariates = TRUE, cace_mde = FALSE, D, trt, 
                   trt_var = "W3_PATA306_treatment_w3"){
  message("MDE without covariate adjustment:")
  pwr_dim <- power.t.test(n = dim$nobs, 
                          sd = sd(unlist(svy[dv]), na.rm=TRUE),
                          sig.level = 0.05, power=0.80)
  message(display(pwr_dim$delta), " (",
          display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                     na.rm=TRUE), pct=TRUE),
          "% of a 1-SD increase in DV)")
  if (covariates == TRUE){
    message("MDE with covariate adjustment:")
    message("Covariates: ", itt[[3]])
    reg <- summary(lm(paste0(dv, itt[[3]]), data=svy))
    ( pwr_itt <- power.t.test(n = itt[[1]]$nobs, 
                              sd = reg$sigma,
                              sig.level = 0.05, power=0.80
    ) )
    message(display(pwr_itt$delta), " (",
            display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                       na.rm=TRUE), pct=TRUE),
            "% of a 1-SD increase in DV)")
  }
  if(cace_mde == TRUE){
    message("MDE for CACE:") # following Bansak 2020
    # compute proportion of variation in D/Y left unexplained by Z, 
    # that is explained by covariates W
    if(covariates == TRUE){
      d_z_df <- svy[as.data.frame(svy)[,trt_var]  %in% c("Control", trt),]
      d_z_df <- na.omit(d_z_df[,c(D, trt_var, extract_covariates(itt))])
      d_y_df <- svy[as.data.frame(svy)[,trt_var]  %in% c("Control", trt),]
      d_y_df <- na.omit(d_y_df[,c(dv, trt_var, extract_covariates(itt))])
      out_d_z <- lm(paste0(D, "~", trt_var), data = d_z_df)
      out_d_y <- lm(paste0(dv, "~", trt_var), data = d_y_df)
      out_d_zw <- lm(paste0(D, "~", paste0(c(trt_var, extract_covariates(itt)), 
                                           collapse = "+")), data = d_z_df)
      out_d_yw <- lm(paste0(dv, "~", paste0(c(trt_var, extract_covariates(itt)), 
                                            collapse = "+")), data = d_y_df)
      r2dw <- max(0, ((summary(out_d_z)$sigma)^2 - 
                        (summary(out_d_zw)$sigma)^2)/((summary(out_d_z)$sigma)^2))
      r2yw <- max(0, ((summary(out_d_y)$sigma)^2 - 
                        (summary(out_d_yw)$sigma)^2)/((summary(out_d_y)$sigma)^2))
    }
    # compute compliance rate / average causal effect of treatment assignment
    # on treatment uptake
    d_z_df <- svy[as.data.frame(svy)[,trt_var]  %in% c("Control", trt),]
    out_d_z <- lm(paste0(D, "~", trt_var), data = d_z_df)
    compliance_rate <- out_d_z$coefficients[2]
    # compute conservative (upper) bound for minimum detectable effect size
    if(covariates == TRUE) {
      if(length(extract_covariates(itt)) > 1) {
        pwr_cace <- try(powerLATE::powerLATE.cov(pZ = .5, 
                                                 pi = compliance_rate, 
                                                 N = cace$nobs,
                                                 sig.level = 0.05,
                                                 power = 0.8,
                                                 r2dw = r2dw,
                                                 r2yw = r2yw,
                                                 effect.size = TRUE,
                                                 assume.ord.means = TRUE,
                                                 verbose = FALSE)$output.parameter)
      }else{
        pwr_cace <- try(powerLATE::powerLATE(pZ = .5, 
                                             pi = compliance_rate, 
                                             N = cace$nobs,
                                             sig.level = 0.05,
                                             power = 0.8,
                                             effect.size = TRUE,
                                             assume.ord.means = TRUE,
                                             verbose = FALSE)$output.parameter)    
      }
    }else{
      pwr_cace <- try(powerLATE::powerLATE(pZ = .5, 
                                           pi = compliance_rate, 
                                           N = cace$nobs,
                                           sig.level = 0.05,
                                           power = 0.8,
                                           effect.size = TRUE,
                                           assume.ord.means = TRUE,
                                           verbose = FALSE)$output.parameter)    
    }
    if(class(pwr_cace) == "try-error") {
      pwr_cace <- NA
    }
    message(display(pwr_cace), " (",
            display(pwr_cace / sd(unlist(svy[,dv]),
                                  na.rm=TRUE), pct=TRUE),
            "% of a 1-SD increase in DV)")
  }
  if(covariates == FALSE & cace_mde == TRUE){
    data.frame(pwr_itt = c(display(pwr_dim$delta), display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE))),
               pwr_cace = c(display(pwr_cace), display(pwr_cace / sd(unlist(svy[,dv]),
                                                                     na.rm=TRUE))))
  }
  if(covariates == TRUE & cace_mde == TRUE){
    data.frame(pwr_itt = c(display(pwr_itt$delta), display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE))),
               pwr_cace = c(display(pwr_cace), display(pwr_cace / sd(unlist(svy[,dv]),
                                                                     na.rm=TRUE))))     
  }
  if(covariates == FALSE & cace_mde == FALSE){
    data.frame(pwr_itt = c(display(pwr_dim$delta), display(pwr_dim$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE))))
  }
  if(covariates == TRUE & cace_mde == FALSE){
    data.frame(pwr_itt = c(display(pwr_itt$delta), display(pwr_itt$delta / sd(unlist(svy[,dv]),
                                                                              na.rm=TRUE))))     
  } 
}

format_latex2 <- function(dim, itt, cace, trt, pwr, dv, cace_mde = FALSE){
  if(cace_mde == TRUE){
    ltx <- paste0(
      dv, " & ", display(dim$coefficients), " (", display(dim$std.error), ")",
      " & ", display(coef(itt[[1]])[2]), " (", display(sqrt(vcov(itt[[1]])[2,2])), ")",
      " & ", display(cace$coefficients[2]), " (", display(cace$std.error[2]), ")",
      " & ", pwr$pwr_itt[1], 
      " & ", pwr$pwr_cace[1], "\\\\ \n",
      "(", trt, ") & [", display(dim$conf.low), ", ", display(dim$conf.high), "]",
      " & [", display(confint(itt[[1]])[2,1]), ", ", display(confint(itt[[1]])[2,2]), "]",
      " & [", display(cace$conf.low[2]), ", ", display(cace$conf.high[2]), "]",
      " & ($d$=", pwr$pwr_itt[2], ")",
      
      " & ($d$=", pwr$pwr_cace[2], ")\\\\"
    )
  }else{
    ltx <- paste0(
      dv, " & ", display(dim$coefficients), " (", display(dim$std.error), ")",
      " & ", display(coef(itt[[1]])[2]), " (", display(sqrt(vcov(itt[[1]])[2,2])), ")",
      " & ", display(cace$coefficients[2]), " (", display(cace$std.error[2]), ")",
      " & ", pwr$pwr_itt[1], "\\\\ \n",
      "(", trt, ") & [", display(dim$conf.low), ", ", display(dim$conf.high), "]",
      " & [", display(confint(itt[[1]])[2,1]), ", ", display(confint(itt[[1]])[2,2]), "]",
      " & [", display(cace$conf.low[2]), ", ", display(cace$conf.high[2]), "]",
      " & ($d$=", pwr$pwr_itt[2], ")\\\\"
    )    
  }
  message(gsub('_', '\\\\_', ltx))
}

format_latex3 <- function(dim, itt, dv){
  ltx <- paste0(
    dv, " & ", display(dim$coefficients), " (", display(dim$std.error), ")",
    " & ", display(coef(itt[[1]])[2]), " (", display(sqrt(vcov(itt[[1]])[2,2])), ")","\\\\ \n",
    "(", trt, " vs. ", control,") & [", display(dim$conf.low), ", ", display(dim$conf.high), "]",
    " & [", display(confint(itt[[1]])[2,1]), ", ", display(confint(itt[[1]])[2,2]), "]\\\\"
  )
  message(gsub('_', '\\\\_', ltx))
} # print stats: treatment vs. control


heterogeneous_effect <- function(dv, dv_pre=NULL, trt, moderator){
  # compute standard itt
  itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, verbose=FALSE)
  vars <- unique(c(extract_covariates(itt), moderator))
  # now compute itt but adding moderator (if it wasn't included)
  lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
  lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
  res <- lm_lin(lin_formula, covariates = lin_covars,
                data=svy[svy$W3_PATA306_treatment_w3 %in% c("Control", trt),])
  # extract t stat for interaction
  interaction_t <- res$statistic[grep(paste0(":", moderator), names(res$statistic))]
  sig <- ifelse(
    interaction_t > 1.96, "+",
    ifelse(interaction_t < (-1.96), "-", "n.s.")
  )
  return(sig)
}

heterogeneous_effect2 <- function(dv, dv_pre=NULL, trt, control, moderator){
  # compute standard itt
  itt <- run_model(dv = dv, dv_pre = dv_pre, trt = trt, verbose=FALSE)
  vars <- unique(c(extract_covariates(itt), moderator))
  # now compute itt but adding moderator (if it wasn't included)
  lin_formula <- formula(paste0(dv, " ~ W3_PATA306_treatment_w3"))
  lin_covars <- formula(paste0(" ~ ", paste(vars, collapse = " + ")))
  res <- lm_lin(lin_formula, covariates = lin_covars,
                data=svy[svy$W3_PATA306_treatment_w3 %in% c(control, trt),])
  # extract t stat for interaction
  interaction_t <- res$statistic[grep(paste0(":", moderator), names(res$statistic))]
  sig <- ifelse(
    interaction_t > 1.96, "+",
    ifelse(interaction_t < (-1.96), "-", "n.s.")
  )
  return(sig)
}