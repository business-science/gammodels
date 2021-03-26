# gam_mod() - General Interface to Linear GAM Models
# - backend: gam
# - prediction:
#   - mode = "regression" (default) uses 
#   - mode = "classification"

#' General Interface for GAM Models
#'
#' @param mode A single character string for the type of model.
#'  
#' @export
gam_mod <- function(mode = "regression") {
    
    args <- list()
    
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
update.gam_mod <- function(object, parameters = NULL,
                           fresh = FALSE, ...) {
    
    parsnip::update_dot_check(...)
    
    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }
    
    args <- list(
        
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