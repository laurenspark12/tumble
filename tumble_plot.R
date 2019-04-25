#' Tumble Plot
#'
#' This function uses tumble_set data to tumble plot.
#' @param tumble_dat A tumble_dat item, created from tumble_set.
#' @param axis_labels A list of x- and y- axis labels.
#' @param legend_title Title of the legend (moderator variable).
#' @param plot_limits Plot limits.
#' @keywords interaction
#' @keywords plots
#' @keywords tumble plots
#' @export
#' @examples
#' tumble_plot()

tumble_plot=function(tumble_dat, axis_labels=c('x','y'), legend_title=NA,plot_limits){
 if(tumble_dat$tumble_dat != T){
   print("Not a tumble dataset - use tumble_set() first!")
   return()
 }else{

   # name of the column designated as the predictor of interest
   input = tumble_dat$target
   output = tumble_dat$prediction
   plot_data = tumble_dat$tumble_data

   if(!missing(plot_limits)){
     plot(NA,
          xlim=plot_limits[[1]],
          ylim=plot_limits[[2]],
          ann=FALSE)
   }else{
     plot(NA,
          xlim=range(plot_data[,input]),
          ylim=range(plot_data[,output]),
          ann=FALSE)
   }
  # blank canvas set up for plotting
  plot.window(xlim=range(plot_data[,input]), ylim=range(plot_data[,output]))

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
  if(!is.na(legend_title)){
    legend("bottomleft",legend_text,cex=.8,
           lty=c("solid","dashed"),
           title=legend_title,
           bty='n',inset=.01)
  }else{legend("bottomleft",legend_text,cex=.8,
               lty=c("solid","dashed"),
               bty='n',inset=.01)
  }
 }
}

setwd("C:/Users/laure/OneDrive/Desktop/RData/tumbleplot")
document()
