# read metric data
kwl_p <- readr::read_csv(here::here("analysis", "data", "raw_data",
                                    "KWL-pottery-metrics-tidy-ave.csv"))

kwl_anping <- readxl::read_excel(here::here("analysis", "data", "raw_data",
                                    "KWL-anping-jar.xls"), sheet = 2)

kwl_stoneware <- readxl::read_excel(here::here("analysis", "data", "raw_data",
                                            "KWL-stoneware.xls"), sheet = 2,
                                    col_types = "text")

kwl_porcelain <- readxl::read_excel(here::here("analysis", "data", "raw_data",
                                               "KWL-porcelain.xls"), sheet = 2,
                                    col_types = "text")

sample_p <- readr::read_csv(here::here("analysis", "data", "raw_data",
                                       "kwl-list-of-sampling-squares.csv"))

kwl_chro <- readxl::read_excel(here::here("analysis", "data", "raw_data",
                                            "KWL_chronology.xlsx"), sheet = 4)

kwl_chro_6 <- readxl::read_excel(here::here("analysis", "data", "raw_data",
                                          "KWL_chronology.xlsx"), sheet = 5)

library(tidyverse)

# tidy data for a later join
kwl_chro_tidy <-
  kwl_chro %>%
  mutate(area = str_sub(Pit, 5, 7),
         Pit = str_sub(Pit, 1, 4)) %>%
  mutate(area = ifelse(area == "", "ABCD", area)) %>%
  mutate(area = sapply(strsplit(area, ""), paste, collapse= ",")) %>%
  separate_rows(area) %>%
  pivot_longer(cols = starts_with("L"),
               names_to = "layer",
               values_to = "period") %>%
  mutate(layer = str_sub(layer, 2, 3)) %>%
  mutate(layer = ifelse(nchar(layer) == 1, paste0("0", layer), layer))

# divided into 6 layers
kwl_chro_tidy_6 <-
  kwl_chro_6 %>%
  mutate(area = str_sub(Pit, 5, 7),
         Pit = str_sub(Pit, 1, 4)) %>%
  mutate(area = ifelse(area == "", "ABCD", area)) %>%
  mutate(area = sapply(strsplit(area, ""), paste, collapse= ",")) %>%
  separate_rows(area) %>%
  pivot_longer(cols = starts_with("L"),
               names_to = "layer",
               values_to = "period") %>%
  mutate(layer = str_sub(layer, 2, 3)) %>%
  mutate(layer = ifelse(nchar(layer) == 1, paste0("0", layer), layer))

# get the sampling squares and join the chronology data
anping_sam <-
  kwl_anping %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(`文化層` == "上文化層") %>%
  filter(`重量(g)` >= min(kwl_p$Weight)) %>%
  left_join(kwl_chro_tidy, by = c("Pit" = "Pit",
                                  "編號/層位" = "layer",
                                  "坑內位置" = "area"))

anping_count <-
  anping_sam %>%
  count(period) %>%
  rename(`anping jar` = "n") %>%
  drop_na()

stoneware_sam <-
  kwl_stoneware %>%
  filter(`編號/空間號` %in% sample_p$the_sq) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`),
         `腹厚(cm)` = as.numeric(`腹厚(cm)`)) %>%
  filter(`文化層` == "上文化層") %>%
  filter(`重量(g)` >= min(kwl_p$Weight)) %>%
  left_join(kwl_chro_tidy, by = c("編號/空間號" = "Pit",
                                  "編號       /層位" = "layer",
                                  "坑內位置" = "area"))

stoneware_count <-
  stoneware_sam %>%
  count(period) %>%
  rename(`stoneware` = "n") %>%
  drop_na()

porcelain_sam <-
  kwl_porcelain %>%
  filter(`編號/      空間號` %in% sample_p$the_sq) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  filter(`文化層` == "上文化層") %>%
  filter(`重量(g)` >= min(kwl_p$Weight)) %>%
  left_join(kwl_chro_tidy, by = c("編號/      空間號" = "Pit",
                                  "編號/層位" = "layer",
                                  "坑內位置" = "area"))

porcelain_count <-
  porcelain_sam %>%
  count(period) %>%
  rename(`porcelain` = "n") %>%
  drop_na()

ceramic_type <-
  kwl_p %>%
  count(period) %>%
  rename(`local pottery` = "n") %>%
  left_join(porcelain_count) %>%
  left_join(stoneware_count) %>%
  left_join(anping_count) %>%
  pivot_longer(!period, names_to = "type", values_to = "number") %>%
  mutate(period = case_when(period == "pre-e" ~ "pre-European",
                            period == "post-e" ~ "post-European",
                            period == "ch-con" ~ "Chinese"))

# plot
library(ggplot2)
library(viridis)

# barplot
bar_a <-
ggplot(ceramic_type,
       aes(x = period,
           y = number,
           fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(limits = rev) +
  theme_minimal()

bar_b <-
ggplot(ceramic_type,
       aes(x = period,
           y = number,
           fill = type)) +
  geom_bar(stat = "identity",
           position = position_dodge()) +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(limits = rev) +
  theme_minimal()

library(cowplot)
ggdraw(bar_b + theme_half_open(12)) +
  draw_plot(bar_a, .15, .45, .5, .5) +
  draw_plot_label(
    c("A", "B"),
    c(0, 0.45),
    c(1, 0.95),
    size = 12
  )

