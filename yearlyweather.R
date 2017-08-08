
yearlyweather <- function(weatheryear,station = NULL) {
  getSummarizedWeather(station,start_date = paste0(weatheryear,"-01-01"),
                       end_date = paste0(weatheryear, "-12-31"),
                       opt_all_columns = TRUE)
}
