# gen_additive_mod() - General Interface to Linear GAM Models
# - backend: gam
# - prediction:
#   - mode = "regression" (default) uses 
#   - mode = "classification"

#' Interface for Generalized Additive Models (GAM)
#'
#' @param mode A single character string for the type of model.
#' @param select_features TRUE or FALSE. If this is TRUE then can add an 
#'  extra penalty to each term so that it can be penalized to zero. 
#'  This means that the smoothing parameter estimation that is part of 
#'  fitting can completely remove terms from the model. If the corresponding 
#'  smoothing parameter is estimated as zero then the extra penalty has no effect. 
#'  Use `adjust_deg_free` to increase level of penalization.
#' @param adjust_deg_free If `select_features = TRUE`, then acts as a multiplier for smoothness. 
#'  Increase this beyond 1 to produce smoother models. 
#'  
#' 
#' @return 
#' A `parsnip` model specification 
#' 
#' @details 
#' 
#' __Available Engines:__
#' - __gam__: Connects to `mgcv::gam()`
#' 
#' __Parameter Mapping:__
#' 
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modelgam", ~ "mgcv::gam", 
#'     "select_features", "select (FALSE)",
#'     "adjust_deg_free", "gamma (1)"
#' ) %>% knitr::kable()
#' ```
#' 
#' @section Engine Details:
#' 
#' __gam__
#' 
#' This engine uses [mgcv::gam()] and has the following parameters, 
#' which can be modified through the [parsnip::set_engine()] function. 
#' 
#' ``` {r echo=F}
#' str(mgcv::gam)
#' ```
#' 
#' @section Fit Details:
#' 
#' __MGCV Formula Interface__
#' 
#' Fitting GAMs is accomplished using parameters including:
#' 
#' - [mgcv::s()]: GAM spline smooths
#' - [mgcv::te()]: GAM tensor product smooths
#' 
#' These are applied in the `fit()` function:
#' 
#' ``` r
#' fit(value ~ s(date_mon, k = 12) + s(date_num), data = df)
#' ```
#' 
#' 
#' @examples 
#' 
#' library(tidymodels)
#' library(modelgam)
#' library(modeltime)
#' library(tidyverse)
#' library(timetk)
#' library(lubridate)
#' 
#' m750_extended <- m750 %>%
#'     group_by(id) %>%
#'     future_frame(.length_out = 24, .bind_data = TRUE) %>%
#'     mutate(lag_24 = lag(value, 24)) %>%
#'     ungroup() %>%
#'     mutate(date_num = as.numeric(date)) %>%
#'     mutate(date_month = month(date))
#' 
#' m750_train  <- m750_extended %>% drop_na()
#' m750_future <- m750_extended %>% filter(is.na(value))
#' 
#' model_fit_gam <- gen_additive_mod(mode = "regression") %>%
#'     set_engine("gam", family=Gamma(link="log"), method = "REML") %>%
#'     fit(value ~ s(date_month, k = 12) 
#'         + s(date_num) 
#'         + s(lag_24) 
#'         + s(date_num, date_month), 
#'         data = m750_train)
#' 
#' model_fit_gam %>% predict(m750_future, type = "numeric") 
#' 
#' model_fit_gam %>% predict(m750_future, type = "raw") 
#' 
#'  
#' @export
gen_additive_mod <- function(mode = "regression", 
                    select_features = NULL,
                    adjust_deg_free = NULL,
                    smoother_dim_term1 = NULL,
                    smoother_dim_term2 = NULL,
                    smoother_dim_term3 = NULL,
                    smoother_dim_term4 = NULL,
                    smoother_dim_term5 = NULL) {
    
    args <- list(
        select_features = rlang::enquo(select_features),
        adjust_deg_free = rlang::enquo(adjust_deg_free),
        smoother_dim_term1 = rlang::enquo(smoother_dim_term1),
        smoother_dim_term2 = rlang::enquo(smoother_dim_term2),
        smoother_dim_term3 = rlang::enquo(smoother_dim_term3),
        smoother_dim_term4 = rlang::enquo(smoother_dim_term4),
        smoother_dim_term5 = rlang::enquo(smoother_dim_term5)
    )
    
    parsnip::new_model_spec(
        "gen_additive_mod",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.gen_additive_mod <- function(x, ...) {
    cat("GAM Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)
    
    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }
    
    invisible(x)
}

#' @export
#' @importFrom stats update
update.gen_additive_mod <- function(object,
                           select_features = NULL,
                           adjust_deg_free = NULL,
                           smoother_dim_term1 = NULL,
                           smoother_dim_term2 = NULL,
                           smoother_dim_term3 = NULL,
                           smoother_dim_term4 = NULL,
                           smoother_dim_term5 = NULL,
                           parameters = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        select_features = rlang::enquo(select_features),
        adjust_deg_free = rlang::enquo(adjust_deg_free),
        smoother_dim_term1 = rlang::enquo(smoother_dim_term1),
        smoother_dim_term2 = rlang::enquo(smoother_dim_term2),
        smoother_dim_term3 = rlang::enquo(smoother_dim_term3),
        smoother_dim_term4 = rlang::enquo(smoother_dim_term4),
        smoother_dim_term5 = rlang::enquo(smoother_dim_term5)
    )
    
    args <- parsnip::update_main_parameters(args, parameters)
    
    if (fresh) {
        object$args <- args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
    }
    
    parsnip::new_model_spec(
        "gen_additive_mod",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.gen_additive_mod <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'gam'` for translation.")
        engine <- "gam"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}


# FIT - GAM -----

#' Low-Level GAM function for translating modeltime to forecast
#'
#' @param formula A dataframe of xreg (exogenous regressors)
#' @param data A numeric vector of values to fit
#' @param ... Additional arguments passed to `forecast::Arima`
#'
#' @export
gen_additive_mod_fit_impl <- function(formula, data, k_term1 = NULL, k_term2 = NULL, k_term3 = NULL, k_term4 = NULL, k_term5 = NULL, ...) {
    
    # X & Y
    others <- list(...)
    
    k <- c(k_term1, k_term2, k_term3, k_term4, k_term5)
    
    #"fit" when it comes from fit() and "parsnip::fit" when it comes from modeltime_refit().
    callingFun = as.list(sys.call(-6))[[1]] 
    
    y <- all.vars(formula)[1]
    x <- attr(stats::terms(formula, data = data), "term.labels")
    
    smooth_replacement <- function(term, dimension, pattern = "\\)"){
        
        return(stringr::str_replace(term, pattern = pattern, replacement = stringr::str_glue(", k = {dimension})")))
        
    }
    
    if (!is.null(k) & callingFun == "fit"){
        
        smooth_terms <- purrr::map2(x[1:length(k)], k, smooth_replacement) %>%
            stringr::str_c(collapse = " + ")
        
        rest_terms <- x[(length(k)+1):length(x)]
        
        formula <- as.formula(stringr::str_glue('{y} ~ {smooth_terms %>% stringr::str_c(rest_terms, sep = " + ")}'))
    } else{
        formula <- as.formula(stringr::str_glue('{y} ~ {stringr::str_c(x, collapse = " + ")}'))
    }
    
    
    gam_fit <- mgcv::gam(formula = formula, data = data, ...)
    
    
    # RETURN
    modeltime::new_modeltime_bridge(
        class = "gen_additive_mod_fit_impl",
        
        # Models
        models = list(
            model_1 = gam_fit
        ),
        
        data = tibble(
            .date      = timetk::tk_make_timeseries(start_date = "1970", length_out = length(gam_fit$residuals)),
            .actual    = data[[y]],
            .fitted    = gam_fit$fitted.values,
            .residuals = gam_fit$residuals
        ),
        
        extras = list(formula = formula,
                      others  = others,
                      y = y,
                      x = x,
                      k = k,
                      fun = callingFun),
        
        # Description - Convert arima model parameters to short description
        desc = 'GAM Model'
    )
    
}

#' @export
print.gen_additive_mod_fit_impl <- function(x, ...) {
    print(x$models$model_1)
    invisible(x)
}


# PREDICT ----

#' @export
predict.gen_additive_mod_fit_impl <- function(object, new_data, ...) {
    
    gen_additive_mod_predict_impl(object, new_data, ...)
}


#' Bridge prediction Function for GAMS Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `stats::predict()`
#'
#' @return
#' A vector of values (predictions) with class `numeric`.
#'
#' @export
gen_additive_mod_predict_impl <- function(object, new_data, ...) {
    
    # PREPARE INPUTS
    model  <- object$models$model_1
    
    preds <- stats::predict(model, new_data = new_data, newdata = new_data,  type = "response", ...) %>%
        as.numeric()
    
    return(preds)
    
}








