# packages --------------------------------------------------------------------
library(tidyverse)
library(gggibbous)
library(ggforce)
library(ggtext)
library(geofacet)
library(googlesheets4)
library(rlang)
library(patchwork)

# setup -----------------------------------------------------------------------
googlesheets4::gs4_auth("cedric.batailler@gmail.com")

theme_set(theme_void(base_family = "Avenir Next Condensed"))

theme_update(
  legend.position = "none",
  plot.title = element_text(hjust = .5, face = "bold", color = "grey35",
                            size = 13, margin = margin(b = 10, t = 6)),
  plot.caption = element_text(color = "grey65", size = 8,
                              margin = margin(15, 0, 5, 0)),
  strip.text = element_blank(),
  panel.spacing = unit(.075, "lines"),
  plot.margin = margin(rep(7, 4)),
  plot.background = element_rect(color = "grey94", fill = "grey94", size = 1.8)
)
# data sets -------------------------------------------------------------------
dpt_key <- "1hKY0kjzLm55q1R2jD7FmbRkC6XTb0El4oEbVvi98pwo"
gs4_browse(dpt_key)

dpt <- range_read(dpt_key, sheet = "departement", col_types = "ccnn")

vaccination_rate_per_departement <-
  read_delim("https://www.data.gouv.fr/fr/datasets/r/7969c06d-848e-40cf-9c3c-21b5bd5a874b",
             delim = ";") %>%
  readr::write_rds("import/archive.rds") %>% 
  # convert percentage to actual percentage
  mutate(across(starts_with("couv"),
                ~ .x / 100))

# data transformation ---------------------------------------------------------
# tweak the grid
dpt <- dpt %>% mutate(row = - row)

ggplot(dpt, aes(x = col, y = row, label = name)) +
  geom_tile() +
  geom_label() +
  coord_fixed(.9)

# add location and data set to `vaccination_rate_per_departement`
vaccination_rate_per_departement <-
  vaccination_rate_per_departement %>%
  right_join(dpt, by = c("dep" = "code"))

# -----------------------------------------------------------------------------
map_fr_ratio         <- .9
map_dpt_to_highlight <- "38" # can be a vctr
map_show_max_size    <- TRUE
map_max_size_point   <- 8

map_var_of_interest  <- expr(couv_tot_dose1) # used to build the rate variable

# data prep -------------------------------------------------------------------
# need a few transformation because of geom_moon
vaccination_rate_per_departement <-
  vaccination_rate_per_departement %>%
  # tidyr::expand_grid(status = c("vaccinated", "unvaccinated")) %>%
  mutate(rate = !!map_var_of_interest)

# map -------------------------------------------------------------------------
plot_map <-
  ggplot(vaccination_rate_per_departement,
       aes(
         # dep position
         x = col, y = row,
         # pop size
         size = pop,
         # vaccination rate
         ratio = rate,
         fill = rate,
         color = rate,
       )
) +
  # GEOM
  geom_point(size = map_max_size_point,
             fill = "white",
             shape = 21,
             alpha = .5 * map_show_max_size # if map_show_max_size == FALSE, 0
             ) +
  geom_moon(right = TRUE) +
  geom_moon(aes(ratio = 1 - rate), right = FALSE, fill = "white") +
  geom_mark_circle(aes(filter = dep %in% c(map_dpt_to_highlight),
                       label = name, group = name), size = .5) +

  # SCALES
  scale_size(
    range = c(0, map_max_size_point),  # dep overall size
    limits = c(0, NA), # make sure that 0 = 0 in this scale
    guide = "none"     # show size legend
  ) +

  scale_fill_viridis_b(
    option = "A",
    limits = c(0, 1),
    breaks = seq(0, 1, by = .1),
    aesthetics = c("color", "fill"),
    guide = "none"
  ) +
  # theming
  theme(legend.position = "bottom") +

  # COORDS
  coord_fixed(map_fr_ratio)

demo_dep_rate <-
  tibble(rate = c(.30, .50, .80),
         y = .50,
         x = c(.08, .22, .35))

demo_dep_pop <-
  tibble(rate = c(.65, .65, .65),
         size = c(50, 100, 200),
         y = c(0, 0, 0),
         x = c(.08, .22, .35))

# legend ----------------------------------------------------------------------
legend <-
  ggplot() +
  # GEOMS
  # legend: vaccination rate
  geom_moon(data = demo_dep_rate, aes(x = x, y = y, ratio = rate, color = rate, fill = rate)) +
  geom_moon(data = demo_dep_rate, aes(x = x, y = y, ratio = 1 - rate, color = rate), right = FALSE) +

  # legend: population size
  geom_point(data = demo_dep_pop, aes(x = x, y = y),
             size = 10,
             fill = "white",
             shape = 21,
             alpha = .5 * map_show_max_size # if map_show_max_size == FALSE, 0
  ) +
  geom_moon(data = demo_dep_pop, aes(x = x, y = y, ratio = rate, color = rate, fill = rate, size = size)) +
  geom_moon(data = demo_dep_pop, aes(x = x, y = y, ratio = 1 - rate, color = rate, size = size), right = FALSE) +

  # text
  geom_textbox(
    data = tibble(
      x = 0,
      y = c(1.3, .90, .70, -0.60),
      label = c(
        "<b style='font-size:18pt'>Couverture vaccinale en France</b><br><br>Avec l'arrivée du variant **delta** en Europe, la vaccination se présente comme un levier incontournable pour limiter la propagation du Covid-19. ",
        "Un enjeu majeur sera donc de vacciner la population jusqu'à atteindre le seuil de **80% d'immunité**. Une telle proportion semble pour l'instant hors de porté.",
        "<b style='font-size:12pt'>Légende </b>",
        "Source : _sante.gouv.fr_"
        )
    ),
    aes(x = x, y = y, label = label),
    width = 1,
    color = "black",
    family = "Playfair Display",
    lineheight = 1.7,
    size = 3,
    fill = NA,
    box.colour = "#ffffff00",
    hjust = 0
  ) +
  # SCALES
  # scale_size_
  scale_size(range = c(0, 10),
             limits = c(0, NA)) +
  scale_x_continuous(limits = c(0, .6)) +
  scale_y_continuous(limits = c(-.6, 1.5)) +
  scale_fill_viridis_b(
    option = "A",
    limits = c(0, 1),
    breaks = seq(0, 1, by = .1),
    aesthetics = c("color", "fill"),
    guide = "none"
  ) +
  coord_cartesian(clip = "off") +
  #  ANNOTATIONS
  geom_curve(data = data.frame(x = 0.43,
                               y = 0.33,
                               xend = 0.38,
                               yend = 0.45),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = 0.22, arrow = arrow(30L, unit(0.05, "inches"),
                                             "last", "closed"),
             inherit.aes = FALSE) +
  geom_text(data = tibble(x = 0.38, y = 0.29,
                          label = c("Département vacciné à 80%")),
            mapping = aes(x = x, y = y, label = label), size = 3,
            family = "Avenir", inherit.aes = FALSE) +
  geom_curve(data = data.frame(x = 0.15, y = -0.20, xend = 0.19, yend = -0.057),
             mapping = aes(x = x, y = y, xend = xend, yend = yend),
             curvature = -0.315, arrow = arrow(30L, unit(0.05, "inches"),
                                               "last", "closed"),
             inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 0.15, y = -0.27,
                              label = "1 250 000 habitants"),
            mapping = aes(x = x, y = y, label = label),
            size = 3,
            family = "Avenir",
            inherit.aes = FALSE)


(legend + plot_map) + plot_layout(widths = c(1, 2), guides = "collect")

# Saving ----------------------------------------------------------------------
ggsave("export/plot.png", device = ragg::agg_png, scale = .8)
