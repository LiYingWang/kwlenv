library(tidyverse)
# read data
kwl_fauna <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_P.xlsx"), sheet = 1,
                                col_types = "text")

kwl_fauna_HM <- readxl::read_excel(here::here("analysis","data","raw_data","KWL_faunal_HM.xlsx"), sheet = 1,
                                   col_types = "text")

kwl_fauna_broken <- readxl::read_excel(here::here("analysis","data","raw_data","broken_fauna.xlsx"), sheet = 1,
                                   col_types = "text")

sample_p <- readr::read_csv(here::here("analysis", "data", "raw_data", "kwl-list-of-sampling-squares.csv"))

kwl_chro_6 <- readxl::read_excel(here::here("analysis", "data", "raw_data", "KWL_chronology.xlsx"), sheet = 5)

# tidy chronological data
kwl_chro_tidy_6 <-
  kwl_chro_6 %>%
  mutate(area = str_sub(Pit, 5, 7), Pit = str_sub(Pit, 1, 4)) %>%
  mutate(area = ifelse(area == "", "ABCD", area)) %>%
  mutate(area = sapply(strsplit(area, ""), paste, collapse= ",")) %>%
  separate_rows(area) %>%
  pivot_longer(cols = starts_with("L"), names_to = "layer", values_to = "period") %>%
  mutate(layer = str_sub(layer, 2, 3)) %>%
  mutate(layer = ifelse(nchar(layer) == 1, paste0("0", layer), layer))

# combine faunal data of cultural layers and temporal sequences
fauna_sam <-
  kwl_fauna %>%
  mutate(Pit = paste(context, pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層" &!layer == "00") %>% #remove sc
  #filter(!str_detect(`總報告類別`, "[[:punct:]]+|[0-9]+")) %>%
  mutate(area = ifelse(is.na(area), "C", area)) %>% # assign an area to the blanks, which won't change the results
  left_join(kwl_chro_tidy_6, by = c("Pit" = "Pit", "layer" = "layer", "area" = "area"))

# combine faunal data of middens and temporal sequences
fauna_H_sam <-
  kwl_fauna_HM %>%
  select(-`空間號`, -`層   位`) %>%
  mutate(Pit = paste("P", pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(componant == "上文化層") %>%
  left_join(kwl_chro_tidy_6, by = c("Pit" = "Pit", "layer" = "layer", "area" = "area"))

# combine and tidy data from midden and cultural layers
fauna_combined_context <-
  rbind(fauna_sam, fauna_H_sam) %>% # combine the two datasets
  filter(!`部位/名稱` %in% c("角","犄角","角?")) %>%  #not associated with diet
  filter(!`部位/名稱`== "角基部"|!is.na(`部位/左右`)) %>% # associated with cranial parts
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  group_by(taxa) %>%
  mutate(`Weight (g)` = sum(`重量(g)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  mutate(category = case_when(str_detect(animal, "e deer") ~ "deer", str_detect(animal, "fish") ~ "fish",
                              str_detect(animal, "cattle")|str_detect(animal, "buffalo")~ "cattle",
                              animal == "aves" ~ "bird", TRUE ~ animal)) %>%
  mutate(class = case_when(str_detect(taxa, "muntjac")|str_detect(taxa, "ammal")|str_detect(taxa, "erv")|
                           str_detect(taxa, "Sus")|str_detect(taxa, "Rattus")|str_detect(taxa, "Rusa")|
                           str_detect(taxa, "Bos")|str_detect(taxa, "Bubalus")~ "mammal",
                           str_detect(category, "fish") ~ "fish", str_detect(category, "bird") ~ "bird",
                           TRUE ~ "reptile")) %>%
  mutate(`部位/左右` = case_when(`部位/左右` == "Ｌ" ~ "L", `部位/左右` == "Ｒ" ~ "R", TRUE ~ `部位/左右`)) %>%
  mutate(category = fct_relevel(category, "deer","boar","muntjac","cattle","rat","bird","fish","turtle")) %>%
  mutate(modify = case_when(str_detect(`人為痕跡`, "切")|str_detect(`人為痕跡`, "削")|
                            str_detect(`人為痕跡`, "砍")~ "cutmarks",
                            str_detect(`人為痕跡`, "火")|str_detect(`人為痕跡`, "燒")~ "burn", TRUE ~ "no")) %>%
  filter(is.na(refitted)) # for NISP

# taxa counts
fauna_combined_taxa <-
  fauna_combined_context %>%
  count(taxa, `Weight (g)`) %>%
  mutate(taxa = str_replace(taxa, "cervidae", "cervid")) %>%
  mutate(taxa = case_when(str_detect(taxa, "mm") ~ paste("Unidentified", tolower(taxa), sep =" "),
                          TRUE ~ taxa)) %>%
  mutate(`Common name` = case_when(str_detect(taxa, "Cer") ~ "Sika deer", str_detect(taxa, "Bub") ~ "Water buffalo",
                                   str_detect(taxa, "Rus") ~ "Sambar deer", str_detect(taxa, "Ree") ~ "Muntjac",
                                   str_detect(taxa, "Sus") ~ "Boar", str_detect(taxa, "Bos") ~ "Yellow cattle",
                                   str_detect(taxa, "Rat") ~ "Rat", str_detect(taxa, "Pli") ~ "Catfish",
                                   str_detect(taxa, "Act") ~ "Ray-finned fish", str_detect(taxa, "Tes") ~ "Turtle",
                                   str_detect(taxa, "cervid") ~ "Deer family")) %>%
  mutate(group = case_when(str_detect(`Common name`, "deer") ~ 1, str_detect(`Common name`, "Deer") ~ 2,
                           str_detect(taxa, "Sus") ~ 3, str_detect(taxa, "Bub")|str_detect(taxa, "Bos") ~ 4,
                           str_detect(taxa, "munt") ~ 5, str_detect(taxa, "Rat") ~ 6, str_detect(`Common name`, "fish") ~ 7,
                           str_detect(taxa, "Tes") ~ 8, str_detect(taxa, "Ave") ~ 9, str_detect(taxa, "large m") ~ 10,
                           str_detect(taxa, "medium m") ~ 11, TRUE ~ 12)) %>%
  mutate(`Common name` = as.factor(`Common name`)) %>%
  group_by(group) %>%
  arrange(desc(`Weight (g)`), .by_group = TRUE) %>%
  ungroup () %>%
  rename(Taxon = taxa, NISP = n) %>%
  select(Taxon, `Common name`,  NISP, `Weight (g)`) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), across(where(is.character), ~"Total"))) %>%
  mutate(`Common name`= ifelse(is.na(`Common name`)|`Common name` == "Total", "-", `Common name`))

# calculate MNI
fauna_combined_MNI <-
  fauna_combined_context %>%
  filter(!is.na(`部位/左右`)) %>%
  count(`部位/左右`, category, `部位/名稱`)

################### ubiquity ###################
# calculate the number of units for each cultural layers
kwl_unit_per_period <-
  kwl_chro_6 %>%
  mutate(all = pmap_chr(select(., -Pit, -Grid), ~toString(unique(na.omit(c(...)))))) %>%  # unite and remove duplicates
  mutate(unit = str_extract(Pit, "[A-Z]+[0-9]{3}")) %>%
  distinct(unit, .keep_all = TRUE) %>%
  mutate(CL1 = ifelse(str_detect(all, "CL1"), unit, NA)) %>%
  mutate(CL2 = ifelse(str_detect(all, "CL2"), unit, NA)) %>%
  mutate(CL3 = ifelse(str_detect(all, "CL3"), unit, NA)) %>%
  mutate(CL4 = ifelse(str_detect(all, "CL4"), unit, NA)) %>%
  mutate(CL5 = ifelse(str_detect(all, "CL5"), unit, NA)) %>%
  mutate(CL6 = ifelse(str_detect(all, "CL6"), unit, NA))

# count the units by cultural layers for later join
unit_by_period <-
  data.frame(unit_count = colSums(!is.na(kwl_unit_per_period))) %>%
  slice_tail(n = 6) %>%
  tibble::rownames_to_column("period")

# ubiquity of taxa from the 40 units by temporal sequences
fauna_ubiquity <-
  fauna_combined_context %>%
  select(Pit, layer, category, `重量(g)`, period, taxa) %>%
  count(period, Pit, category) %>%
  count(period, category) %>%
  drop_na(period, category) %>%
  left_join(unit_by_period) %>% # join the unit counts for each period
  mutate(ubiquity = paste(round(n/unit_count *100, 2), "%")) %>%
  select(-n, -unit_count) %>%
  pivot_wider(names_from = period, values_from = ubiquity) %>%
  arrange(category)

################### Taxonomic abundance ###################
# barplot I: classes of vertebrates by period (NISP)
verte_class_barplot <-
  fauna_combined_context %>%
  drop_na(period, class) %>%
  group_by(period) %>%
  mutate(class = factor(class, levels = c("mammal", "bird", "fish", "reptile"), ordered = TRUE)) %>%
  ggplot(aes(x = period, fill = class))+
  geom_bar(position = "fill", width = 0.7) + #position_dodge2(preserve = "single")
  labs(y = "NISP", x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.title=element_blank())

# barplot II: classes of vertebrates by period (normed NISP across classes)
verte_class_barplot_normed <-
  fauna_combined_context %>%
  count(period, class) %>% #class
  drop_na(period, class) %>%
  mutate(NNISP = case_when(str_detect(class, "mammal") ~ n/205,
                           str_detect(class, "bird") ~ n/130,
                           str_detect(class, "fish") ~ n/150,
                           str_detect(class, "reptile") ~ n/200)) %>%
  mutate(class = factor(class, levels = c("mammal", "bird", "fish", "reptile"), ordered = TRUE)) %>%
  group_by(period) %>%
  summarise(total_per_period = sum(NNISP), across()) %>%
  mutate(`%NNISP` = (NNISP/total_per_period)*100) %>%
  ggplot(aes(x = period, y = `%NNISP`))+
  geom_bar(stat = "identity", aes(fill = class), width = 0.7) +
  labs(y = "%NNISP", x = NULL) +
  theme_minimal() +
  theme(legend.title=element_blank())

# barplot III: relative abundance of mammals by period
library(viridis)
verte_mammal_barplot <-
  fauna_combined_context %>%
  filter(class == "mammal" &!category == "rat") %>%
  drop_na(period, category) %>% # remove category to get absolute abundance
  mutate(category = factor(category, levels = c("deer", "boar", "muntjac", "cattle"), ordered = TRUE)) %>%
  ggplot(aes(x = period, fill = category))+
  geom_bar(position = "fill", width = 0.6) +
  labs(y = "NISP", x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  scale_fill_viridis_d(option="magma", begin = 0.2, end = 0.9,) +
  guides(fill= guide_legend(title="mammal"))

#ggsave(here::here("analysis", "figures", ".png"), h = 3, w =5, units = "in")

################### Skeletal part ###################
fauna_deer_only <-
  fauna_combined_context %>%
  filter(category %in% c("deer") & !is.na(period))

deer_elements <-
  fauna_deer_only %>%
  filter(!is.na(`部位/名稱`)) %>%
  count(`部位/名稱`) #distinct

fauna_deer_portion <-
  fauna_deer_only %>%
  mutate(portion = case_when(
    `部位/名稱` %in% c("胸椎","腰椎","頸椎","第二頸椎","脊椎","肋骨","薦骨","胸骨") ~ "mid-meaty\n(body)",
    `部位/名稱` %in% c("肩胛骨","肱骨","尺骨","橈骨","脛骨","髖骨","股骨","肱或股骨","上肢骨") ~ "meaty\n(upper limb)",
    `部位/名稱` %in% c("掌骨","掌骨或蹠骨","蹠骨","跗骨","趾骨","腕骨","跟骨","astragalus","第1趾骨","第2趾骨","第3趾骨") ~ "non-meaty\n(lower limb)",
    `部位/名稱` %in% c("上顎骨","上顎及齒","上顎齒","下顎及齒","下顎骨","下顎齒","臼齒","枕骨","頭骨",
                       "顱骨","齒","角基部") ~ "non-meaty\n(cranial parts)"))

weight_head <-
fauna_deer_portion %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  filter(portion == "non-meaty\n(cranial parts)") %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`, na.rm = T)) %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  labs(x= NULL, y= "weight of cranial parts(g)") +
  theme_minimal()

deer_portion_plot <-
  fauna_deer_portion %>%
  select(period, portion, animal) %>%
  drop_na() %>%
  count(period, portion) %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"))) %>% #ordered = TRUE
  ggplot(aes(period, n, fill = portion)) + #fill = portion
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"), widtg = 0.6) + #stat = "identity", position = "fill"
  #facet_wrap(~taxa) +
  labs(x = NULL, y = NULL) +
  guides(fill= guide_legend(title="portion"))+
  scale_fill_viridis_d(labels=c('upper limb', 'body', 'head','lower limb'))+
  #scale_fill_discrete(breaks=c("body\n(mid-meaty)", "upper limb\n(meaty)", "lower limb\n(meaty)","cranial elements(none-meaty)")) +
  theme_minimal()

#ggsave(here::here("analysis", "figures", "talk-fauna-2.png"), h = 4, w =4, units = "in")

library(cowplot)
plot_grid(fauna_mammal_barplot, deer_portion_plot,
          labels = c('A', 'B'), rel_widths = c(1.3, 1), label_size = 12)

#ggsave(here::here("analysis", "figures", "talk-fauna.png"), h = 4, w =9, units = "in")

################### fragmentation #######################
# calculate MNE
deer_MNE <-
  fauna_deer_only  %>%
  filter(!is.na(`部位/左右`)&is.na(overlap)) %>% #remove overlapped fragments
  count(period,`部位/左右`,`部位/名稱`,`部位/位置`) %>%
  group_by(period) %>%
  summarise(MNE = sum(n), across())

deer_NISP <-
  fauna_deer_only %>%
  count(period) %>%
  rename(NISP = n)

frag_index <-
  left_join(deer_MNE, deer_NISP) %>%
  mutate(index = MNE/NISP)

# broken bones(all taxa)
fauna_broken <-
  kwl_fauna_broken %>%
  mutate(Pit = paste(context,pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  filter(!layer == "00") %>% #remove sc
  mutate(area = ifelse(is.na(section), "A", section)) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  left_join(kwl_chro_tidy_6) %>%
         #   by = c("Pit" = "Pit", "layer" = "layer", "area" = "area")) %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`)) %>%
  filter(!is.na(period))

fauna_broken_plot <-
  fauna_broken %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  labs(x= NULL, y = "weight(g)") +
  #scale_y_continuous(breaks = seq(0, 90, by = 10)) +
  theme_minimal()

#ggsave(here::here("analysis", "figures", "talk-fauna-6.png"), h = 2, w =4, units = "in")

################### Cutmarks ###################
# count cutmarks throughout all taxa
fauna_total_cut <-
  fauna_combined_context %>%
  filter(modify == "cutmarks") %>%
  group_by(taxa, period) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na(period, taxa)

# cutmarks on deer bones
fauna_deer_joints <-
  fauna_deer_portion  %>%
  mutate(joint = case_when(`部位/名稱` %in% c( "肩胛骨") ~ "shoulder",
                             `部位/名稱` %in% c("橈骨","尺骨","肱骨") ~ "elbow",
                             `部位/名稱` %in% c("掌骨","腕骨") ~ "wrist", #"掌骨或蹠骨"
                             `部位/名稱` %in% c("髖骨","股骨","薦骨") ~ "hip",
                             `部位/名稱` %in% c("脛骨") ~ "knee",
                             `部位/名稱` %in% c("蹠骨", "跟骨", "附骨", "astragalus") ~ "ankle")) #"蹠骨(或掌骨)"

# deer joints with cutmarks
fauna_deer_cut <-
  fauna_deer_joints %>%
  filter(modify == "cutmarks") %>%
  group_by(period, joint, cutmarks) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer bones by portion in total
fauna_deer_NNISP <-
  fauna_deer_joints %>%
  group_by(period, joint) %>%
  count() %>%
  left_join(fauna_deer_cut) %>%
  rename(NISP = n) %>%
  drop_na() %>%
  #mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  mutate(proportion = round(`NISP with cutmarks`/NISP, 2), NNISP = NISP/2) %>%
  group_by(period) %>%
  mutate(`%NNISP` = round(NNISP/sum(NNISP), 2)) %>%
  mutate(cut_percent = `%NNISP` * proportion)

# plot
deer_cut <-
  ggplot(fauna_deer_NNISP, aes(x = period, y = proportion,
         fill = factor(joint, levels = (c("shoulder","elbow","wrist","hip","knee","ankle"))))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  labs(x = NULL, y = "joints with cutmarks (%NNISP)") +
  guides(fill= guide_legend(title="joints"))+
  scale_fill_viridis_d(labels=c("shoulder","elbow","wrist","hip","knee","ankle")) +
  theme_minimal()
  #theme(legend.position = "none")

ggsave(here::here("analysis", "figures", "talk-fauna-3.png"), h = 4, w =4.5, units = "in")

# t-test (for two groups only)
res <- t.test(cut_percent ~ period, data = fauna_deer_NNISP)

################### Cutmarks related to skinning ###################
# Skeletal elements related to skinning
deerskin_portion <-
  fauna_deer_only %>%
  mutate(skinning_cut = case_when(str_detect(.$`部位/名稱`, "掌骨")|
                                  str_detect(.$`部位/名稱`, "蹠骨") ~ "metapodials",
                                  str_detect(.$`部位/名稱`, "腕骨")|
                                  str_detect(.$`部位/名稱`, "跗骨") ~ "carpal/tarsal",
                                  str_detect(.$`部位/名稱`, "趾骨") ~ "phalanges",
                                  str_detect(.$`部位/名稱`, "頸椎")|
                                  str_detect(.$`部位/名稱`, "枕骨")|
                                  str_detect(.$`部位/名稱`, "薦骨") ~ "neck/tail regions",
                                  TRUE ~ "others"))
  #filter(!`人為痕跡` %in% c("火燒"))

# NISP for skeletal elements related to skinning
deerskin_portion_NISP <-
  deerskin_portion %>%
  filter(!skinning_cut == "others") %>%
  group_by(period, skinning_cut) %>%
  count() %>%
  rename(NISP_skin_bone = n) %>%
  drop_na()

# plot bones associated with hide removal processes
deer_skinning_related <-
  deerskin_portion_NISP %>%
  ggplot(aes(x = period, y = NISP_skin_bone , fill = skinning_cut)) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(x = NULL, y = "NISP") +
  scale_fill_viridis_d(labels=c('carpal/\ntarsal', 'metapodials', 'phalanges','neck/\ntail regions')) +
  theme_minimal()

# NISP with circular and straight cutmarks
deerskin_portion_NISP_cut <-
  deerskin_portion %>%
  filter(str_detect(`人為痕跡`, "切")) %>%
  group_by(period, skinning_cut) %>%
  count() %>%
  left_join(deerskin_portion_NISP) %>%
  drop_na() %>%
  mutate(proportion = n/NISP_skin_bone)

# plot the proportion of skinning bones with cutmarks
deer_cut_skinning <-
  deerskin_portion_NISP_cut %>%
  ggplot(aes(x = period, y = proportion , fill = skinning_cut)) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  labs(x = NULL, y = "%NISP") +
  scale_fill_viridis_d(labels=c('carpal/\ntarsal', 'metapodials', 'phalanges','neck/\ntail regions')) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "talk-fauna-5.png"), h = 3, w =5, units = "in")

# combine
plot_grid(deer_cut, deer_cut_skinning,
          labels = c('A', 'B'), rel_widths = c(1, 1.55), label_size = 12)

ggsave(here::here("analysis", "figures", "deer-cut.png"), h = 4, w =8, units = "in")

# t-test
res <- t.test(cut_percent ~ period, data = fauna_deer_skin_NNISP)

###############modification################
# burn bones
burnbone_deer <-
  fauna_deer_only %>%
  filter(modify == "burn") %>%
  count(period) %>%
  rename(burn = n)

# bones with cutmarks
cutmarks_deer <-
  fauna_deer_only %>%
  filter(modify == "cutmarks") %>%
  count(period) %>%
  rename(cutmarks = n)

# NISP per period
fauna_deer_only_NISP <-
  fauna_deer_only %>%
  count(period) %>%
  rename(NISP = n)

# join
deer_modify<-
  fauna_deer_only_NISP %>%
  left_join(cutmarks_deer) %>%
  left_join(burnbone_deer)
