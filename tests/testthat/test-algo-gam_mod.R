testthat::context("TEST gen_additive_mod: GAM")

# SETUP---

m750_extended <- m750 %>%
    group_by(id) %>%
    future_frame(.length_out = 24, .bind_data = TRUE) %>%
    mutate(lag_24 = lag(value, 24)) %>%
    ungroup() %>%
    mutate(date_num = as.numeric(date)) %>%
    mutate(date_month = month(date))

m750_train  <- m750_extended %>% drop_na()
m750_future <- m750_extended %>% filter(is.na(value))

classif_df <- readRDS(url("https://github.com/noamross/gams-in-r-course/raw/master/csale.rds"))

classif_split <- initial_split(classif_df, prop = 0.8, strata = purchase)

#Regression ----

model_spec_reg <- gen_additive_mod() %>%
                  set_engine("gam", family = gaussian(), method = "REML")

model_spec_clas <- gen_additive_mod(mode = "classification") %>%
                   set_engine("gam", family = binomial(), method = "REML")
                   
                   


# PARSNIP ----

test_that("gen_additive_mod: Regression Parsnip Test", {
    
    testthat::skip_on_cran()
    
    model_fit <<- model_spec_reg %>%
        fit(value ~ s(date_month, k = 12), data = m750_train)
    
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(m750_future) %>%
        modeltime_forecast(new_data = m750_future)
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "gam")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$family$family, "gaussian")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    
    
    # Structure
    testthat::expect_identical(nrow(m750_future), nrow(predictions_tbl))
    testthat::expect_identical(m750_future$date, predictions_tbl$.index)
    
    # Out-of-Sample Accuracy Tests
    
    resid <- model_fit$fit$residuals
    
    # - Max Error less than 2000
    testthat::expect_lte(max(abs(resid)), 3000)
    
    # - MAE less than 1200
    testthat::expect_lte(mean(abs(resid)), 1200)
    
})


test_that("gen_additive_mod: Classification Parsnip Test", {
    
    testthat::skip_on_cran()
    
    model_fit <<- model_spec_clas %>%
        fit(as.factor(purchase) ~ s(mortgage_age, k = 12), data = training(classif_split))
    
    
    predictions_prob_tbl <- model_fit %>%
        predict(testing(classif_split), type = "prob")
    
    predictions_class_tbl <- model_fit %>%
        predict(testing(classif_split), type = "class")
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "gam")
    testthat::expect_s3_class(model_fit, "model_fit")
    testthat::expect_equal(model_fit$fit$family$family, "binomial")
    testthat::expect_equal(model_fit$lvl, c("0", "1"))
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "purchase")
    
    
    # Structure
    testthat::expect_identical(nrow(predictions_prob_tbl), nrow(predictions_class_tbl))
    testthat::expect_identical(nrow(predictions_prob_tbl), nrow(testing(classif_split)))
    testthat::expect_identical(levels(predictions_class_tbl$.pred_class), c("0"))
    
    # Out-of-Sample Accuracy Tests
    
    resid <- model_fit$fit$residuals
    
    # - Max Error less than 6
    testthat::expect_lte(max(abs(resid)), 6)
    
    # - MAE less than 2
    
    testthat::expect_lte(mean(abs(resid)), 2)
    
})
