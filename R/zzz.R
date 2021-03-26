

# IMPORTS ----
# - Parsnip gives user access to parsnip functions (fit.model_spec, set_engine)
# - mgcv gives user access to spline function (s, ti, ...)

#' @import mgcv
#' @import parsnip
 
# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    
    make_gam_mod()
    
    make_gam_mod_mgcv_gam()
    
}

