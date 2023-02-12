# text stuff
library(dplyr)
library(ggplot2)
library(glue)
library(palmerpenguins)
library(showtext)
showtext_opts(dpi=300)

# list all font paths on local computer
font_paths()

# add a new path
# font_paths(new = r"(C:\Users\RPeek\AppData\Local\Microsoft\Windows\Fonts)")

# Pick a Font -------------------------------------------------------------

# pick a font pair
fnt_headers <- "PT Sans"
fnt_text <- "Noto Sans"

# PT Sans: header text
# Noto Sans: body text

fnt_headers <- "Merriweather"
fnt_text <- "Source Sans Pro"
# Merriweather: good for headings, not for small
# Source Sans Pro: good for smaller text

fnt_headers <- "Lora"
fnt_text <- "Libre Franklin"
# Lora: Sans Serif for labeling, good for header and subheader
# Libre Franklin: Serif for reading, chart text, good for callouts

fnt_headers <- "Roboto"
fnt_text <- "Roboto Condensed"
# Roboto: good for header and subheader
# Roboto Condensed: very good for small labeling

# same!?
fnt_headers <- "Assistant"
fnt_text <- "Assistant"

# same!?
fnt_headers <- "Atkinson Hyperlegible"
fnt_text <- "Atkinson Hyperlegible"

# FUN
# Caveat: cursive fun, best for larger headers
# Schoolbell: scrawl


# Look Locally ------------------------------------------------------------

# search for a font name or family
font_files() %>% filter(grepl(fnt_to_add, ps_name))
font_files() %>% filter(grepl(fnt_to_add, family))

# ttf name
(ttf <- font_files() %>% filter(grepl(fnt_to_add, ps_name)) %>%
    select(file) %>%
    filter(grepl("Regular", file)))

# add locally
font_add(family = fnt_to_add, regular = glue("{ttf}"))


# Look on Web -------------------------------------------------------------

# add straight from google
font_add_google(fnt_headers)
font_add_google(fnt_text)

# Plot --------------------------------------------------------------------

# call this for all future plots, else _begin and _end
showtext_auto()

# tst plot
ggplot() +
  geom_point(data=penguins, aes(x=body_mass_g, y=flipper_length_mm, fill=species), alpha=0.8, pch=21, size=3, color=alpha("gray80", 0.5)) +
  ggdark::dark_theme_classic(base_family = fnt_headers) +
  scale_fill_manual("Penguin Species", values = c("darkorange","purple","cyan4")) +
  labs(title="Palmer Penguins Data", y="Flipper Length (mm)", x="Body Mass (g)") +
  theme(title = element_text(face="bold", size=22),
        axis.text = element_text(family=fnt_text, size = 14),
        legend.text = element_text(family=fnt_text, size = 16))


# Can we Export? ----------------------------------------------------------

ggsave(glue::glue("figs/penguins_example_w_fonts_{fnt_headers}.png"),
       width = 8, height = 6, units="in", dpi=300)

#ggsave(glue::glue("figs/penguins_example_w_fonts_{fnt_to_add}.pdf"),
#       width = 8, height = 6, units="in", device = cairo_pdf)
