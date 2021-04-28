#' Tuning Parameters for GAMS Models
#'
#'
#' @inheritParams dials::Laplace
#'
#' @details
#' The main parameters for ARIMA models are:
#'
#'  - `smoother_dim`: The the dimension of the basis used to represent a smooth term.
#'
#' @examples
#' smoother_dim()
#'
#'
#' @name gen_additive_mod


#' @export
#' @rdname gen_additive_mod_params
smoother_dim_term1 <- function(range = c(0L, 10L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smoother_dim_term1 = "Dimension GAM Smoother"),
        finalize  = NULL
    )
}


#' @export
#' @rdname gen_additive_mod_params
smoother_dim_term2 <- function(range = c(0L, 10L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smoother_dim_term2 = "Dimension GAM Smoother"),
        finalize  = NULL
    )
}


#' @export
#' @rdname gen_additive_mod_params
smoother_dim_term3 <- function(range = c(0L, 10L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smoother_dim_term3 = "Dimension GAM Smoother"),
        finalize  = NULL
    )
}


#' @export
#' @rdname gen_additive_mod_params
smoother_dim_term4 <- function(range = c(0L, 10L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smoother_dim_term4 = "Dimension GAM Smoother"),
        finalize  = NULL
    )
}


#' @export
#' @rdname gen_additive_mod_params
smoother_dim_term5 <- function(range = c(0L, 10L), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(smoother_dim_term5 = "Dimension GAM Smoother"),
        finalize  = NULL
    )
}




