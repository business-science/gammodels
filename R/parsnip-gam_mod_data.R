

make_gen_additive_mod <- function() {
    parsnip::set_new_model("gen_additive_mod")
}

make_gen_additive_mod_mgcv_gam <- function() {
    
    #### REGRESION
    model  = "gen_additive_mod"
    mode   = "regression"
    engine = "gam"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "mgcv")
    parsnip::set_dependency(model = model, eng = engine, pkg = "modelgam")
    
    #Args
    
    parsnip::set_model_arg(
        model        = "gen_additive_mod",
        eng          = "gam",
        parsnip      = "select_features",
        original     = "select",
        func         = list(pkg = "modelgam", fun = "select_features"),
        has_submodel = FALSE
    )
    
    parsnip::set_model_arg(
        model        = "gen_additive_mod",
        eng          = "gam",
        parsnip      = "adjust_deg_free",
        original     = "gamma",
        func         = list(pkg = "modelgam", fun = "adjust_deg_free"),
        has_submodel = FALSE
    )
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(pkg = "mgcv", fun = "gam"),
            defaults = list(
                select = FALSE,
                gamma  = 1
            )
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "numeric",
        value  = list(
            pre  = NULL,
            post = function(x, object) as.numeric(x),
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data),
                type = "response"
            )
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "conf_int",
        value  = list(
            pre  = NULL,
            post = function(results, object) {
                res <-tibble::tibble(.pre_lower = results$fit - 2*results$se.fit,
                                     .pre_upper = results$fit + 2*results$se.fit)
            },
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data),
                type = "link",
                se.fit = TRUE
            )
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "raw",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data)
            )
        )
    )
    
    #### CLASSIFICATION
    
    model  = "gen_additive_mod"
    mode   = "classification"
    engine = "gam"
    
    parsnip::set_model_engine(model = model, mode = mode, eng = engine)
    parsnip::set_dependency(model = model, eng = engine, pkg = "mgcv")
    parsnip::set_dependency(model = model, eng = engine, pkg = "modelgam")
    
    parsnip::set_encoding(
        model = model,
        eng   = engine,
        mode  = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )
    
    parsnip::set_fit(
        model = model,
        eng = engine,
        mode = mode,
        value = list(
            interface = "formula",
            protect = c("formula", "data"),
            func = c(pkg = "mgcv", fun = "gam"),
            defaults = list(
                select = FALSE,
                gamma  = 1,
                family = stats::binomial(link = "logit")
            )
        )
    )
    
    prob_to_class_2 <- function(x, object){
        
            x <- ifelse(x >= 0.5, object$lvl[2], object$lvl[1])
            unname(x)
    }
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "class",
        value  = list(
            pre  = NULL,
            post = function(results, object) {

                tbl <-tibble::as_tibble(results) 
                
                if (ncol(tbl)==1){
                        res<-prob_to_class_2(tbl, object) %>% 
                            tibble::as_tibble() %>% 
                            stats::setNames("values") %>%
                            dplyr::mutate(values = as.factor(values))
                     } else{
                         res <- tbl %>% 
                                apply(.,1,function(x) which(max(x)==x)[1])-1 %>% #modify in the future for something more elegant when gets the formula ok
                                tibble::as_tibble()
                 }
                
            },
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data),
                type = "response"
            )
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "prob",
        value  = list(
            pre  = NULL,
            post = function(results, object) {
                res <-tibble::as_tibble(results)
            },
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data),
                type = "response"
            )
        )
    )
    
    parsnip::set_pred(
        model  = model,
        eng    = engine,
        mode   = mode,
        type   = "raw",
        value  = list(
            pre  = NULL,
            post = NULL,
            func = c(fun = "predict"),
            args = list(
                object = rlang::expr(object$fit),
                newdata = rlang::expr(new_data)
            )
        )
    )
    
}
