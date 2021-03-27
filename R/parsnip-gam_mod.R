# gam_mod() - General Interface to Linear GAM Models
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
#' model_fit_gam <- gam_mod(mode = "regression") %>%
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
gam_mod <- function(mode = "regression", 
                    select_features = NULL,
                    adjust_deg_free = NULL) {
    
    args <- list(
        select_features = rlang::enquo(select_features),
        adjust_deg_free = rlang::enquo(adjust_deg_free)
    )
    
    parsnip::new_model_spec(
        "gam_mod",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )
    
}

#' @export
print.gam_mod <- function(x, ...) {
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
update.gam_mod <- function(object,
                           select_features = NULL,
                           adjust_deg_free = NULL,
                           parameters = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        select_features = rlang::enquo(select_features),
        adjust_deg_free = rlang::enquo(adjust_deg_free)
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
        "gam_mod",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.gam_mod <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'gam'` for translation.")
        engine <- "gam"
    }
    x <- parsnip::translate.default(x, engine, ...)
    
    x
}

