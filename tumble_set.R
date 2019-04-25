#' Tumble Set
#'
#' This function prepares three variables to be passed to tumble_plot for plotting.
#' @param dat Dataset
#' @param outcome Outcome variable
#' @param target Independent variable
#' @param moderator Moderator variable
#' @keywords interaction
#' @keywords plots
#' @keywords tumble plots
#' @export
#' @examples
#' tumble_set()

tumble_set = function(dat, outcome, target, moderator) {
  # full interaction model
  intrxn_formula = as.formula(paste(outcome,'~',target,"*",moderator))
  intrxn_model = lm(intrxn_formula,data=dat)
  # model between predictors, target as outcome of moderator
  inputs_formula = as.formula(paste(target,'~',moderator))
  inputs_model = lm(inputs_formula,data=dat)
  # descriptives for predictor inputs
  target_desc = c('mean'=mean(dat[,target]),
                  'sd'=sd(dat[,target]))
  moderator_desc = c('mean'=mean(dat[,moderator]),
                     'sd'=sd(dat[,moderator]))
  # computation of +/- 1 SD values for moderator
  lowmod <- moderator_desc['mean']-moderator_desc['sd']
  highmod <- moderator_desc['mean']+moderator_desc['sd']
  # target values predicted for low and high moderator
  inputs_prediction_data = data.frame(c(lowmod,highmod))
  # reuse names from the inputs model, leave out (Intercept) term
  names(inputs_prediction_data) = names(coef(inputs_model))[-1]
  predicted_targets = predict(inputs_model, inputs_prediction_data)
  # standard error of estimate for target as an outcome
  target_residual = summary(inputs_model)$sigma
  # Adjust the predicted values by adding/subtracting the se
  # low target w/ low moderator
  # high target w/ low moderator
  # low target w/ high moderator
  # high target w/ high moderator
  adjusted_predicted_targets = c(
    rep(predicted_targets,each=2)
    + rep(c(-1,1)*target_residual,times=2))
  # construct the prediction data data.frame
  intrxn_prediction_data = data.frame(
    adjusted_predicted_targets,          # target input
    c(lowmod,lowmod,highmod,highmod),    # moderator input
    c(adjusted_predicted_targets         # interaction term
      *c(lowmod,lowmod,highmod,highmod)))
  # names from the interaction model, leave out (Intercept)
  names(intrxn_prediction_data) = names(coef(intrxn_model))[-1]
  # add some factor labels to indicate the computations.
  intrxn_prediction_data = cbind(
    intrxn_prediction_data,
    'target'=rep(c('low','high'),times=2),
    'moderator'=rep(c('low','high'),each=2))
  tumble_predictions = predict(intrxn_model, intrxn_prediction_data)
  # update our data.frame
  prediction=paste(outcome,'hat',sep='_')
  intrxn_prediction_data[,prediction] = tumble_predictions
  tumble_dat <- list(
    'interaction_model'=intrxn_model,
    'input_model'=inputs_model,
    'tumble_data'=intrxn_prediction_data,
    'target'=target,
    'moderator'=moderator,
    'outcome'=outcome,
    'prediction'=prediction,
    'target_desc'=target_desc,
    'moderator_desc'=moderator_desc,
    'highmod'=highmod,
    'lowmod'=lowmod,
    'tumble_dat' = T)
  return(tumble_dat)
}

setwd("C:/Users/laure/OneDrive/Desktop/RData/tumbleplot")
document()
