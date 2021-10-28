Date_Marks <-function (DF_input)
{
  library(lubridate)

  DF_input$date<-paste(DF_input$Year, "-", DF_input$Month, "-", DF_input$Day_Month, " ", DF_input$Hour_Day, ":00:00", sep="")
  DF_input$date<-as.POSIXct(DF_input$date, format="%Y-%m-%d %H:%M:%OS")

  DF_input$Week_Day   <-  wday(DF_input$date) # day of the week
  DF_input$Day_Year   <-  yday(DF_input$date) # day of the year
  DF_input$Week_Year  <-  as.numeric(strftime(DF_input$date,format="%W"))  # Week of year
  DF_input$Hour_Day   <-  hour(DF_input$date) # Hour of the day
  DF_input$Hour_Year  <-  (DF_input$Day_Year-1)*24+DF_input$Hour_Day # Hour of year
  DF_input$Hour_Week  <-  DF_input$Hour_Day+(DF_input$Week_Day-1)*24
  DF_input$Year       <-  year(DF_input$date)
  DF_input$Month       <-  month(DF_input$date)

  return (DF_input)
}