library(sf)
library(mapview)
library(broom)
library(tidyverse)

merged_collisions <- st_read("data/merged_collisions.shp")
mc_geometry <- select(merged_collisions, id = Input_FID)

collisions <- merged_collisions %>%
  st_drop_geometry() %>%
  select(
    Input_FID,
    matches("^n"),
    n_total = Total_Coll
  ) %>%
  group_by(Input_FID) %>%
  summarize(across(matches("^n"), max)) %>%
  set_names(
    'id',
    'broadsides',
    'headons',
    'readends',
    'dark',
    'wet',
    'proceeding_straight',
    'making_left_turn',
    'unsafe_speed',
    'unsafe_turn_or_lane_change',
    'total'
  )

collisions_props <- collisions %>%
  select(-id) %>%
  summarise_all(sum) %>%
  pivot_longer(
    -total,
    names_to = "type",
    values_to = "count"
  ) %>%
  mutate(type_prop = count/total) %>%
  pull(type_prop, type)

intersections_with_collisions <- collisions %>%
  filter(total > 15) %>%
  select(-total) %>%
  pivot_longer(
    -id,
    names_to = "type",
    values_to = "count"
  ) %>%
  uncount(count) %>%
  mutate(type = factor(type, levels = c(
    'broadsides',
    'headons',
    'readends',
    'dark',
    'wet',
    'proceeding_straight',
    'making_left_turn',
    'unsafe_speed',
    'unsafe_turn_or_lane_change')
    )
  ) %>%
  arrange(id)

intersections <- unique(intersections_with_collisions$id)

# Goodness of fit test
# Null hypothesis:
# the proportion of collisions at each intersection = the proportion of collisions in the city
# Alternative hypothesis:
# The proportion of collisions at each intersection != the proportion of collisions in the city

stringr::str_to_title(gsub("_", " ", names(collisions_props)))

collisions_props %>%
  enframe() %>%
  mutate(name = stringr::str_to_title(gsub("_", " ", name))) %>%
  mutate(name = fct_reorder(name, value)) %>%
  ggplot(aes(value, name)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = scales::percent(value, accuracy = 0.1)), nudge_x = 0.03) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = NULL,
    title = "Proportions of Collision Types in San Francisco",
    subtitle = "2017-2021",
    caption = "Source: https://transbase.sfgov.org/dashboard/dashboard.php"
  ) +
  pilot::theme_pilot(grid = "v")

ggsave("figs/proportions_of_collision_types.png")

cx_tests <- intersections_with_collisions %>%
  group_by(id) %>%
  group_map(~chisq.test(table(.x[,"type"]), p = collisions_props, rescale.p = TRUE)) %>%
  set_names(intersections)

cx_test_ss <- cx_tests %>%
  keep(~.x$p.value < 0.05) %>%
  map_dfr(augment, .id = "id") %>%
  hacksaw::cast_numeric(id) %>%
  transmute(
    id,
    type = Var1,
    residuals = .observed - .expected
    ) %>%
  pivot_wider(
    id_cols = id,
    names_from = type,
    values_from = residuals
  )

collisions_obs_exp <- inner_join(mc_geometry, cx_test_ss, by = "id")
st_write(collisions_obs_exp, "data/collisions_obs_exp.shp")
# mapview(collisions_obs_exp, alpha = 0.6)

