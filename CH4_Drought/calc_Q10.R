
TRC_PARMS_05 <- function(data.frame = NULL,
                         iterations = NULL,
                         priors.trc = brms::prior("normal(0.2 , 5)", nlpar = "a", lb = 0.1, ub = 5) +
                           brms::prior("normal(0.1, 0.03)", nlpar = "b", lb = 0.001, ub = 0.4),
                         idx.colname = NULL,
                         NEE.colname = NULL,
                         TA.colname = NULL) { 
  nee <- idx <- TA <- NULL
  
  data.frame$nee <- data.frame[,NEE.colname]
  data.frame$idx <- data.frame[,idx.colname]
  data.frame$TA <- data.frame[,TA.colname]
  
  df <- data.frame %>% dplyr::select(idx, nee, TA)
  
  equation  <- (nee ~ a * exp(b*TA))
  
  # PARM Data frame:
  parms <- base::data.frame(idx = base::as.character(),
                            a.mean = base::as.numeric(),
                            a.se = base::as.numeric(),
                            a.Bulk_ESS = base::as.numeric(),
                            a.Tail_ESS = base::as.numeric(),
                            a.Rhat = base::as.numeric(),
                            
                            b.mean = base::as.numeric(),
                            b.se = base::as.numeric(),
                            b.Bulk_ESS = base::as.numeric(),
                            b.Tail_ESS = base::as.numeric(),
                            b.Rhat = base::as.numeric(),
                            samples = base::as.numeric())
  
  base::message(" Your data frame looks good and you are now ready to start fitting models")
  
  for (i in base::unique(df$idx)){
    base::print(i)
    
    # Subset the file:
    base::try(df.sub <- df %>% dplyr::filter(idx == i), silent = T)
    # get priors:
    
    base::try(model.brms <- brms::brm(brms::bf(nee ~ a * exp(b*TA), a+b ~ 1, nl = TRUE),
                                      prior = priors.trc, data = df.sub,
                                      backend = "cmdstanr", iter = iterations, cores = 4, seed = 101), silent = F)
    
    base::print(model.brms)
    
    base::try(model.brms.df <- summary(model.brms)$fixed, silent = F)
    
    base::try(model.brms.df.a <- model.brms.df %>% dplyr::filter(base::row.names(model.brms.df) == 'a_Intercept'), silent = F)
    base::try(model.brms.df.b <- model.brms.df %>% dplyr::filter(base::row.names(model.brms.df) == 'b_Intercept'), silent = F)
    
    base::try(samples <- df.sub %>% dplyr::filter(idx == i) %>% dplyr::select(nee) %>% stats::na.omit() %>% base::nrow(), silent = F)
    
    base::try(results <- base::data.frame(idx = i,
                                          a.mean = model.brms.df.a$Estimate,
                                          a.se = model.brms.df.a$Est.Error,
                                          a.Bulk_ESS = model.brms.df.a$Bulk_ESS,
                                          a.Tail_ESS = model.brms.df.a$Tail_ESS,
                                          a.Rhat = model.brms.df.a$Rhat,
                                          b.mean = model.brms.df.b$Estimate,
                                          b.se = model.brms.df.b$Est.Error,
                                          b.Bulk_ESS = model.brms.df.b$Bulk_ESS,
                                          b.Tail_ESS = model.brms.df.b$Tail_ESS,
                                          b.Rhat = model.brms.df.b$Rhat,
                                          samples= samples) %>% mutate(Q10 = exp(10*b.mean)), silent=T)
    
    base::message('YOU DID IT!')
    base::print(results)
    base::try(parms <- parms %>% base::rbind(results), silent = T)
  }
  
  return(parms)
  
}

TRC_PARMS_04 <- function(data.frame = NULL,
                         iterations= NULL,
                         priors.trc = brms::prior("normal(2.0, 0.3)", nlpar = "Q10", lb = 1.0, ub = 5) +
                           brms::prior("normal(0.5, 0.3)", nlpar = "Rref", lb = 0.001, ub = 5),
                         idx.colname = NULL,
                         NEE.colname = NULL,
                         TA.colname = NULL,
                         Tref =NULL){
  
  # Squelch visible bindings note
  nee <- idx <- TA <- NULL
  data.frame$Tref <- Tref
  data.frame$nee <- data.frame[,NEE.colname]
  data.frame$idx <- data.frame[,idx.colname]
  data.frame$TA <- data.frame[,TA.colname]
  
  df <- data.frame %>% dplyr::select(idx, nee, TA, Tref)
  
  base::try(equation <- nee ~ Rref * Q10 * exp((TA-Tref)/10), silent = T)
  
  # PARM Data frame:
  base::try(parms <- base::data.frame(idx = base::as.character(),
                                      Q10.mean = base::as.numeric(),
                                      Q10.se = base::as.numeric(),
                                      Q10.Bulk_ESS = base::as.numeric(),
                                      Q10.Tail_ESS = base::as.numeric(),
                                      Q10.Rhat = base::as.numeric(),
                                      
                                      Rref.mean = base::as.numeric(),
                                      Rref.se = base::as.numeric(),
                                      Rref.Bulk_ESS = base::as.numeric(),
                                      Rref.Tail_ESS = base::as.numeric(),
                                      Rref.Rhat = base::as.numeric(),
                                      samples = base::as.numeric(),
                                      Tref = base::as.numeric()), silent = T)
  
  base::message(" Your data frame looks good and you are now ready to start fitting models")
  
  for (i in base::unique(df$idx)){
    base::print(i)
    
    # Subset the file:
    df.sub <- base::try(df %>% dplyr::filter(idx == i), silent = TRUE)
    
    # get priors:
    base::print(df.sub)
    
    base::try(model.brms <- brms::brm(brms::bf(nee ~ Rref * Q10 * exp((TA- Tref)/10), Q10 + Rref ~ 1, nl = TRUE),
                                      prior = priors.trc , data = df.sub,
                                      backend = "cmdstanr", iter = iterations, cores = 4, seed = 101), silent = F)
    
    base::print(model.brms)
    
    base::try(model.brms.df <- summary(model.brms)$fixed, silent = F)
    
    base::try(model.brms.df.Q10 <- model.brms.df %>% dplyr::filter(base::row.names(model.brms.df) == 'Q10_Intercept'), silent = F)
    base::try(model.brms.df.Rref <- model.brms.df %>% dplyr::filter(base::row.names(model.brms.df) == 'Rref_Intercept'), silent = F)
    
    base::try(samples <- df.sub %>% dplyr::filter(idx == i) %>% dplyr::select(nee) %>% stats::na.omit() %>% base::nrow(), silent = F)
    
    
    
    base::try(results <- base::data.frame(idx = i,
                                          Q10.mean = model.brms.df.Q10$Estimate,
                                          Q10.se = model.brms.df.Q10$Est.Error,
                                          Q10.Bulk_ESS = model.brms.df.Q10$Bulk_ESS,
                                          Q10.Tail_ESS = model.brms.df.Q10$Tail_ESS,
                                          Q10.Rhat = model.brms.df.Q10$Rhat,
                                          
                                          Rref.mean = model.brms.df.Rref$Estimate,
                                          Rref.se = model.brms.df.Rref$Est.Error,
                                          Rref.Bulk_ESS = model.brms.df.Rref$Bulk_ESS,
                                          Rref.Tail_ESS = model.brms.df.Rref$Tail_ESS,
                                          Rref.Rhat = model.brms.df.Rref$Rhat,
                                          Tref = Tref,
                                          samples = samples), silent = T)
    
    base::message('YOU DID IT!')
    base::print(results)
    base::try(parms <- parms %>% base::rbind(results), silent = T)
    
  }
  
  return(parms)
}
