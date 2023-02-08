# text stuff
library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(showtext)
showtext_opts(dpi=300)
# list all font paths on local computer
font_paths()

# search for a font name or family
font_files() %>% filter(grepl("Atkinson", ps_name))
font_files() %>% filter(grepl("Roboto", family))
# add locally
#font_add(family = "Atkinson Hyperlegible", regular = "Atkinson-Hyperlegible-Regular-102.ttf")

# add straight from google
fnt_to_add <- "Caveat"
font_add_google(fnt_to_add)

# call this for all future plots, else _begin and _end
showtext_auto()

# tst
ggplot() +
  geom_point(data=penguins, aes(x=body_mass_g, y=flipper_length_mm, fill=species), alpha=0.8, pch=21, size=3, color=alpha("gray80", 0.5)) +
  ggdark::dark_theme_classic(base_family = fnt_to_add, base_size = 14) +
  scale_fill_manual("Penguin Species", values = c("darkorange","purple","cyan4")) +
  labs(title="Palmer Penguins Data", y="Flipper Length (mm)", x="Body Mass (g)")


# Can we Export? ----------------------------------------------------------

ggsave(glue::glue("figs/penguins_example_w_fonts_{fnt_to_add}.png"),
       width = 8, height = 6, units="in", dpi=300)
ggsave(glue::glue("figs/penguins_example_w_fonts_{fnt_to_add}.pdf"),
       width = 8, height = 6, units="in", device = cairo_pdf)
