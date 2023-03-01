#' CFR Plotter
#'
#' Create a multi-panel CFR plotter with different lags
#' @param data The input data
#' @param lag The lag 
#' @param max_lag The maximum lag
#' @return A multi-panel plot of case fatality rates with different lags
#' @export
plot_lagged_cfr = function(data, lag, max_lag) {
	xaxt = ifelse(lag==max_lag,'s','n')
	xlim = c(data$date[max_lag+1], tail(data$date,1))
	cfr_lagged = tail(data$new_deaths_smoothed_per_million, -lag)/head(data$new_cases_smoothed_per_million, -lag)
	plot(data$date[-c(1:lag)], cfr_lagged,
			 type='l',
			 xlab='Case report date',
			 ylab=paste0('CFR (lag = ',lag,')'),
			 ylim=c(0.0, 0.4),
			 xaxt=xaxt,
			 xlim=xlim,
			 cex.axis=1.5,
			 cex.lab=1.5
	)
}