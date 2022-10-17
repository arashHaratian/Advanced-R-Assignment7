#' ridgereg Class
#'
#' This class calculates all the interesting parameter for the ridge regression and uses the following methods:
#'
#' @field beta Regression Coefficients
#' @field fitted_val Fitted Values
#' @field residual_val Residual Values
#' @field df Degrees of Freedom
#' @field res_var Residual Variance
#' @field var_reg Variance of the Regression Coefficients
#' @field t_values t-values of the coefficients
#' @field p_values p-values of the coefficients
#' @field formula formula of the model
#' @field call Function Call as a string
#'
#' @import methods
#' @export ridgereg

ridgereg <- setRefClass(
  "ridgereg",
  fields = list(
    beta = "numeric",
    fitted_val = "numeric",
    residual_val = "numeric",
    df = "numeric",
    res_var = "numeric",
    var_reg = "numeric",
    t_values = "numeric",
    p_values = "numeric",
    formula = "formula",
    call = "character"
  ),
  methods = list(
    initialize = function(formula, data, lambda){

      stopifnot(
        is.data.frame(data),
        inherits(formula, "formula"),
        is.numeric(lambda)
      )

      dependent <- all.vars(formula)[1]
      scaled_data <- data
      for(i in seq_along(data)){
        if(is.numeric(data[, i]) && names(data[i]) != dependent){
          scaled_data[, i] <- scale(data[, i])
        }
      }

      X <- model.matrix(formula, scaled_data)

      n <- nrow(X)
      p <- ncol(X)


      beta_local <- solve(t(X) %*% X + diag(lambda, p)) %*% t(X) %*% data[[dependent]]
      .self$fitted_val <- as.vector(X %*% beta_local)
      residual_val_local <- data[[dependent]] - .self$fitted_val
      .self$residual_val <- as.vector(residual_val_local)
      .self$df <- n-p
      .self$res_var <- as.vector((t(residual_val_local) %*% residual_val_local)/.self$df)
      .self$var_reg <- diag(.self$res_var * solve(t(X) %*% X))
      .self$t_values <- as.vector(beta_local / sqrt(.self$var_reg))

      .self$p_values <- as.vector(pt(-abs(t_values),df = .self$df))

      beta_vector <- as.vector(beta_local)
      names(beta_vector) <- rownames(beta_local)
      .self$beta <- beta_vector

      .self$formula <- formula
      formula_string <- as.character(formula)
      .self$call <- paste0(
        "ridgereg(formula = ",
        formula_string[[2]],
        " ~ ",
        formula_string[[3]],
        ", data = ",
        deparse(substitute(data)),
        ", lambda = ",
        deparse(substitute(lambda)),
        ")"
      )
    },
    #-------------------- print()
    show = function() {
      cat("Call:\n",
          format(.self$call),
          "\n\nCoefficients:\n")

      coef_table <- data.frame(t(.self$beta))
      colnames(coef_table) <- names(.self$beta)
      rownames(coef_table) <- ""

      print.data.frame(coef_table)
    },

    #-------------------- predict()
    predict = function(new_x){
      stopifnot(
        is.data.frame(new_x)
        # (names(.self$beta))[-1]  %in% names(new_x)
      )
      new_x_matrix <- model.matrix(.self$formula, new_x)
      print(colnames(new_x_matrix))
      y_hat <- new_x_matrix %*% matrix(.self$beta, ncol = 1)
      return(y_hat)
    },
    #-------------------- 3 small functions from linreg:)
    resid = function(){
      return(.self$residual_val)
    },

    pred = function(){
      return(.self$fitted_val)
    },

    coef = function(){
      return(.self$beta)
    },

    #-------------------- summary()
    summary = function(){
      result <- data.frame(
        "Estimate" = .self$beta,
        "Std_Error" = sqrt(.self$var_reg),
        "t_value" = .self$t_values,
        "p_value" = .self$p_values
      )
      result$" " <- ifelse(
        result [[4]] >= 1,
        " ",
        ifelse(
          result [[4]] > 0.1,
          ".",
          ifelse(
            result[[4]] > 0.05,
            "*",
            ifelse(
              result[[4]] > 0.001,
              "**",
              "***"
            )
          )
        )
      )

      cat("Coefficients:\n")
      print.data.frame(result)
      cat("\n\nResidual standard error:", sqrt(.self$res_var), "on", .self$df, "degrees of freedom")
    }
  )
)

