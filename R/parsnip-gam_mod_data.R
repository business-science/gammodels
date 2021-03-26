

make_gam_mod <- function() {
    parsnip::set_new_model("gam_mod")
}

make_gam_mod_mgcv_gam <- function() {
    
    model  = "gam_mod"
    mode   = "regression"
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
            defaults = list()
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
                newdata = rlang::expr(new_data)
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
