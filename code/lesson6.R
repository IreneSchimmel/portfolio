library(DT)
library(tidyverse)
datatable(iris, options = list(pageLength = 21))

library(htmlwidgets)
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%
  addMarkers(lat=52.084036170323024, lng=5.173630727235891, popup="Here be HL7!")
m

rabbit_locations <- tibble(rabbitnr = c(1:5),
                           lat = c(52.0852,52.0832,52.08247,52.0834,52.08309),
                           long = c(5.171109,5.171248,5.171248,5.176055,5.17268))
leaflet(data = rabbit_locations) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(rabbitnr), label = ~as.character(rabbitnr), icon = bunbun_marker)

bunbun_marker <- makeIcon(
  iconUrl = "https://toppng.com/uploads/preview/easy-cute-bunny-drawing-11549512755w3v0ootraj.png",
  iconWidth = 50, iconHeight = 60)
)

library(threejs)
library(palmerpenguins)

z <- penguins$bill_length_mm
x <- penguins$flipper_length_mm
y <- penguins$body_mass_g
N = length(levels(penguins$species))
scatterplot3js(x, y, z, color = rainbow(N)[penguins$species])

library(dygraphs)
library(xts)

rabbitdata <- data.frame(
  time = seq(from=Sys.Date()-(10*365), to=Sys.Date(), by = 21),
  oorlengte=runif((10*365)/21+1, min = 6, max = 10)
)
rabbitdata

rabbitdata$oorlengte[120:138]<- rabbitdata$oorlengte[120:138]+4

rabbitdata.xts <- xts(x = rabbitdata$oorlengte, order.by = rabbitdata$time)

dygraph(rabbitdata.xts) %>%
  dySeries(label = "average ear length (cm)", color = "black") %>%
  dyShading(from= rabbitdata$time[120], to = rabbitdata$time[138], color = "#FFE6E6") %>%
  dyRangeSelector(dateWindow = c(rabbitdata$time[90], rabbitdata$time[150]))

library(plotly)

fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
fig

plot_ly(
  y = rnorm(50),
  type = "box") %>%
  add_trace(
    y = rnorm(50, 1)
  )

fig_sankey <- plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("A1", "A2", "B1", "B2", "C1", "C2"),
    color = c("blue", "blue", "purple", "purple", "darkred", "darkred"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value = c(8,4,2,8,4,2)
  )
)

fig_sankey <- fig_sankey %>% layout(
  title = "Basic Sankey Diagram",
  font = list(
    size = 10
  )
)
fig_sankey

unzip(~/Data_science/bookdown-minimal-master.zip)

