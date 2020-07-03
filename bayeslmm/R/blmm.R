#' Perform a Bayesian linear effect model
#'
#' @export
#' @param formula An R model formula for mixed effect model
#' @param data The name of the data frame
#' @param core The number of CPU cores that the model will use
#' @param seed The inizial conditions of the ranmber generator
#' @return A list with two elements: model and waic.
#' @examples
#' \dontrun{
#' M <- lmm(rt ~ days + (1|subject), data)
#' }


lmm <- function(formula, data, cores = 4, seed = 10101){
  M <- brms::brm(as.formula(formula),
           cores = cores,
           chains = 4,
           seed = seed,
           data = data)

  list(model = M,
       waic = brms::waic(M)$estimates['waic', 'Estimate'])
}

#' A linear mixed effect faced plot
#'
#' @export
#' @param data A data frame
#' @return A ggplot object
#' @import ggplot2
#' @examples
#' \dontrun{
#' lmmplot(sleepstudy)
#' }
#'

lmmplot <- function(data){
  ggplot(data,
         aes(x = day, y = rt, colour = subject)) +
    geom_point() +
    stat_smooth(method = 'lm', se = F) +
    facet_wrap(~subject) +
    theme_minimal() +
    theme(legend.position = 'none')
}
