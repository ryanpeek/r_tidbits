# text stuff
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(showtext)
font_paths()
font_files() %>% filter(grepl("Atkinson", ps_name))
font_add_google("Atkinson Hyperlegible")
#font_add(family = "Atkinson Hyperlegible", regular = "Atkinson-Hyperlegible-Regular-102.ttf")
showtext_auto()

# tst
ggplot() +
  geom_point(data=penguins, aes(x=body_mass_g, y=flipper_length_mm, fill=species), alpha=0.8, pch=21, size=3, color=alpha("gray80", 0.5)) +
  ggdark::dark_theme_classic(base_family = "Atkinson Hyperlegible") +
  scale_fill_manual("Penguin Species", values = c("darkorange","purple","cyan4")) +
  labs(title="Palmer Penguins Data", y="Flipper Length (mm)", x="Body Mass (g)")
