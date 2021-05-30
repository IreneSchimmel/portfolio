library(tidyverse)
library(palmerpenguins)
library(RColorBrewer)
library(viridis)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

penguins

mass_hist <- ggplot(data = penguins, aes(x = body_mass_g)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.8,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_minimal() +
  labs(x = "Body mass (g)",
       y = "Frequency",
       title = "Penguin body mass")

mass_hist

mass_hist + scale_fill_manual(values = cbp1)

display.brewer.all(colorblindFriendly = TRUE)

bp <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_boxplot(aes(fill = Species)) +
  theme_minimal() +
  theme(legend.position = "top")
bp
bp + scale_fill_brewer(palette = "PiYG")

sp <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species)) +
  theme_minimal()+
  theme(legend.position = "top")
sp
sp + scale_color_brewer(palette = "Paired")

bp + scale_colour_viridis_d(option = "plasma")
