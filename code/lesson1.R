library(tidyverse)
library(drc)
library(readxl)
library(dplyr)
CE.LIQ.FLOW.062 <- read_excel("CE.LIQ.FLOW.062_Tidydata.xlsx") ##importing file
CE.LIQ.FLOW.062
inspect_columns <- CE.LIQ.FLOW.062  %>% dplyr::select(RawData, compName, compConcentration)

class(CE.LIQ.FLOW.062$compConcentration)
CE.LIQ.FLOW.062$compConcentration
CE.LIQ.FLOW.062$compConcentration <- gsub(",", ".", CE.LIQ.FLOW.062$compConcentration)
CE.LIQ.FLOW.062$compConcentration <- as.numeric(CE.LIQ.FLOW.062$compConcentration)

inspect_columns

inspect_columns %>% distinct(compConcentration)

fs::dir_tree(here::here("~/OneDrive/Documenten/DSFB2"))

CE.LIQ.FLOW.062$expType

controlNegative <- CE.LIQ.FLOW.062 %>% filter(expType == "controlNegative")
controlNegative$RawData

CE.LIQ.FLOW.062

negativeControl <- CE.LIQ.FLOW.062 %>% filter(compName == "S-medium")

nc_mean <- mean(negativeControl$RawData)
nc_mean
negativeControl$RawData
nc_mean

inspect_columns$compName <- as.function(inspect_columns$compName)
means <- inspect_columns %>% group_by(compName, compConcentration) %>% summarise(mean = mean(RawData, na.rm = TRUE))
means$relative <- means$mean / nc_mean
means %>% map_dbl(means$mean)
means

means %>% ggplot() +
  geom_point(aes(x = log10(compConcentration), y = relative, colour = compName))
             
             