#' Tumble Set
#'
#' This function prepares an interaction model for tumble plotting.
#' @param dat A dataset of class data.frame.
#' @param outcome The dependent variable.
#' @param target The continuous independent variable of class numeric.
#' @param moderator The continuous moderator of class numeric.
#' @keywords interaction
#' @keywords tumble plot
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
    class = 'tumble data')
  return(tumble_dat)
}


#' Tumble Plot
#'
#' This function plots a tumble plot for probing a continuous by continuous interaction with empirical endpoints.
#' @param dat A dataset of class data.frame.
#' @param outcome The dependent variable.
#' @param target The continuous independent variable of class numeric.
#' @param moderator The continuous moderator of class numeric.
#' @param axis_labels A list of two character objects for the x- and y-axes. Defaults to c('x','y').
#' @param plot_title A character object for the plot title. Defaults to NA.
#' @param legend_title A character object for the legend title. Defaults to NA.
#' @param plot_limits A list of two numeric objects for xlim and ylim. Defaults to the ranges of the independent and dependent variables.
#' @keywords interaction
#' @keywords tumble plot
#' @export
#' @examples
#' tumble_plot()

tumble_plot=function(dat,
                     outcome,
                     target,
                     moderator,
                     axis_labels=c('x','y'),
                     plot_title = NA,
                     legend_title=NA,
                     plot_limits){
 if(class(dat) != "data.frame"){
   print("Object dat is not class data.frame.")
 }else{
   # name of the column designated as the predictor of interest
   tumble_dat <- tumble_set(dat,
              outcome,
              target,
              moderator)
   input = tumble_dat$target
   output = tumble_dat$prediction
   plot_data = tumble_dat$tumble_data

   par(
     mfrow=c(1,5),
     mar = c(.2, .2, .2, .2),
     oma = c(4,4,3,.00001)
     )
   layout(
     matrix(c(1,2), nrow = 1),
     widths = c(3,2)
     )

   if(!missing(plot_limits)){
     plot(NA,
          xlim=plot_limits[[1]],
          ylim=plot_limits[[2]],
          ann=FALSE,)
     title(main = plot_title, outer = T)
   }else{
     plot(NA,
          xlim=range(plot_data[,input]),
          ylim=range(plot_data[,output]),
          ann=FALSE)
     title(main = plot_title, outer = T)
   }

  # blank canvas set up for plotting
  plot.window(
    xlim=range(plot_data[,input]),
    ylim=range(plot_data[,output]))

  # formula for lines
  plot_formula = as.formula(paste(output,"~",input))

  # for each level of the moderator, here "high" and "low"
  lines(plot_formula,
        data = plot_data[plot_data$moderator=='high',],
        type='o')

  lines(plot_formula,
        data = plot_data[plot_data$moderator!='high',],
        type='o', lty='dashed')

  # annotations to the plot
  mtext(axis_labels[1], side=1, line=2.5, cex=1)
  mtext(axis_labels[2], side=2, line=2.5, cex=1)

  # add a legend
  legend_text=c(
    paste(round(tumble_dat$highmod,1),'(1 SD Above Mean)'),
    paste(round(tumble_dat$lowmod,1),'(1 SD Below Mean)'))
  # add a legend title if specified
  plot(NULL,
       bty = 'n',
       xaxt = 'n',
       yaxt = 'n',
       xlim=c(0,1),
       ylim=c(0,1),
       xlab = '',
       ylab = '')
    if(!is.na(legend_title)){
    legend("left",
           legend_text,
           cex=.7,
           lty=c("solid","dashed"),
           title=legend_title,
           bty='n',
           text.width = .75,
           ncol = 1
           )
  }else{legend("left",
               legend_text,
               cex=.7,
               lty=c("solid","dashed"),
               bty='n',
               text.width = .75,
               ncol = 1
               )
  }
  }
 }



