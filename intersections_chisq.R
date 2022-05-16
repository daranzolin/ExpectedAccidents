library(sf)
library(mapview)
library(broom)
library(hacksaw)
library(tidyverse)

merged_collisions <- st_read("data/merged_collisions.shp")
mc_geometry <- select(merged_collisions, id = Input_FID)

collisions <- merged_collisions %>%
  st_drop_geometry() %>%
  select(
    id = Input_FID,
    matches("^n")
  ) %>%
  group_by(id) %>%
  summarize_all(max) %>%
  set_names(
    'id',
    'viol_following_too_close',
    'viol_other',
    'viol_red_signal',
    'viol_unsafe_speed',
    'viol_unsafe_turn',
    'viol_leftturn_row',
    'type_broadsides',
    'type_headons',
    'type_hit_obj',
    'type_readends',
    'type_sideswipes',
    'lght_dark',
    'lght_day',
    'lght_dusk',
    'cond_dry',
    'cond_wet',
    'veh1_changing_lanes',
    'veh1_leftturn',
    'veh1_rightturn',
    'veh1_uturn',
    'veh1_proceed_straight'
  )

collisions_list <- collisions %>%
  select_split(
    violations = c(id, matches("^viol")),
    col_type = c(id, matches("^type")),
    lighting = c(id, matches("^lght")),
    road_conditiions = c(id, matches("^cond")),
    vehicle1 = c(id, matches("^veh1"))
  )

collisions_props <- collisions_list %>%
  map(~{
    sums <- .x %>%
      select(-id) %>%
      summarise_all(sum)
    total <- sum(unlist(sums))
    sums %>%
      mutate(total = total) %>%
      pivot_longer(
        -total,
        names_to = "type",
        values_to = "count"
      ) %>%
      mutate(type_prop = count/total) %>%
      pull(type_prop, type)
  })

collisions_list2 <- collisions_list %>%
  map(~{
    factor_levels <- names(.x)[-1]
    .x %>%
      mutate(total = rowSums(across(2:last_col()))) %>%
      filter(total > 15) %>%
      select(-total) %>%
      pivot_longer(
        -id,
        names_to = "type",
        values_to = "count"
      ) %>%
      uncount(count) %>%
      mutate(type = factor(type, levels = factor_levels)) %>%
      arrange(id)
  })

collisions_props %>%
  map_dfr(enframe, .id = "type") %>%
  mutate(name = fct_reorder(name, value)) %>%
  ggplot(aes(value, name)) +
  geom_col(fill = "steelblue", color = "black") +
  # geom_text(aes(label = scales::percent(value, accuracy = 0.1)), nudge_x = 0.03) +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~type, scales = "free") +
  labs(
    x = NULL,
    y = NULL,
    title = "Proportions of Collision Types in San Francisco",
    subtitle = "2017-2021",
    caption = "Source: https://transbase.sfgov.org/dashboard/dashboard.php"
  ) +
  pilot::theme_pilot(grid = "v")

ggsave("figs/proportions_of_collision_types.png")

# Goodness of fit test
# Null hypothesis:
# the proportion of collisions at each intersection = the proportion of collisions in the city
# Alternative hypothesis:
# The proportion of collisions at each intersection != the proportion of collisions in the city

testing <- names(collisions_props)
cx_tests <- 1:5 %>%
  map(~{
    itr <- .x
    df <- collisions_list2[[itr]]
    intersection_ids <- unique(df$id)
    collisions_list2[[itr]] %>%
      group_by(id) %>%
      group_map(~chisq.test(table(.x[,"type"]), p = collisions_props[[itr]])) %>%
      set_names(intersection_ids)
  }) %>%
  set_names(testing)

ss_cx_tests <- cx_tests %>%
  map(~keep(.x, ~.x$p.value < 0.05)) %>%
  map(map_dfr, augment, .id = "intersection") %>%
  map(~{
    .x %>%
      hacksaw::cast_numeric(intersection) %>%
      transmute(
        intersection,
        type = Var1,
        residuals = .observed - .expected
      ) %>%
      pivot_wider(
        id_cols = intersection,
        names_from = type,
        values_from = residuals
      )
  })

violations <- mc_geometry %>%
  inner_join(ss_cx_tests$violations, by = c("id" = "intersection"))

collision_types <- mc_geometry %>%
  inner_join(ss_cx_tests$col_type, by = c("id" = "intersection"))

lighting <- mc_geometry %>%
  inner_join(ss_cx_tests$lighting, by = c("id" = "intersection"))

road_conditions <- mc_geometry %>%
  inner_join(ss_cx_tests$road_conditiions, by = c("id" = "intersection"))

vehicle1 <- mc_geometry %>%
  inner_join(ss_cx_tests$vehicle1, by = c("id" = "intersection"))

mv <- partial(mapview, alpha.regions = 0.02)

mv(violations, col.regions = "firebrick")
mv(collision_types, col.regions = "steelblue")
mv(lighting, col.regions = "yellow")
mv(road_conditions, col.regions = "grey2")
mv(vehicle1, col.regions = "forestgreen")

st_write(violations, "data/violations.shp")
st_write(collision_types, "data/collision_types.shp")
st_write(lighting, "data/lighting.shp")
st_write(road_conditions, "data/road_conditions.shp")
st_write(vehicle1, "data/vehicle1.shp")

