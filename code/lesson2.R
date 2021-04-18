
2.2
1. By viewing the data in excel
2. All cycles
3. readxl library + function read_excel, specifying the sheet
4. making the data tidy
5. salmonella_cfu_od600.xlsx
6. [email]

data$year_week
class(data$year_week)

convert_week_to_date <- function(x, y){
  map2(
    x, 
    y,
    get_date) %>%
    unlist() %>%
    lubridate::as_date()


  
  data_tidy <- data %>%
  separate(col = "year_week", into = c("year", "week")) %>%
  mutate(
    date =
      convert_week_to_date(x = week, y = year)
  )
colnames(data_tidy)

data_tidy[c(1:4), "date"]
