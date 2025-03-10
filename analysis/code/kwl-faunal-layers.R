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
fauna_combined_context_all <-
  rbind(fauna_sam, fauna_H_sam) %>% # combine the two datasets
  filter(!`部位/名稱` %in% c("角","犄角","角?")) %>%  #not associated with diet
  filter(!`部位/名稱`== "角基部"|!is.na(`部位/左右`)) %>% # associated with cranial parts
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  group_by(taxa) %>%
  mutate(`Weight (g)` = sum(`重量(g)`, na.rm = T)) %>%
  ungroup() %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  mutate(category = case_when(str_detect(animal, "sika")|str_detect(animal, "sambar")|
                                str_detect(animal, "e deer")~ "deer", str_detect(animal, "pig")|
                                str_detect(animal, "boar") ~ "suid", str_detect(animal, "fish") ~ "fish",
                              str_detect(animal, "cattle")|str_detect(animal, "buffalo")~ "bovine",
                              animal == "aves" ~ "bird", TRUE ~ animal)) %>%
  mutate(class = case_when(str_detect(taxa, "muntjac")|str_detect(taxa, "ammal")|str_detect(taxa, "erv")|
                             str_detect(taxa, "Sus")|str_detect(taxa, "Rattus")|str_detect(taxa, "Rusa")|
                             str_detect(taxa, "Bos")|str_detect(taxa, "Bubalus")~ "mammal",
                           str_detect(category, "fish") ~ "fish", str_detect(category, "bird") ~ "bird",
                           TRUE ~ "reptile")) %>%
  mutate(`部位/左右` = case_when(`部位/左右` == "Ｌ" ~ "L", `部位/左右` == "Ｒ" ~ "R", TRUE ~ `部位/左右`)) %>%
  mutate(category = fct_relevel(category, "deer","suid","muntjac","bovine","rat","bird","fish","turtle")) %>%
  mutate(cutmarks = ifelse(str_detect(`人為痕跡`, "切"), "yes", "no"),
         fractures = ifelse(str_detect(`人為痕跡`, "削")|str_detect(`人為痕跡`, "砍")|
                              str_detect(`人為痕跡`, "折")|str_detect(`人為痕跡`, "敲"), "yes", "no"),
         burning =  ifelse(str_detect(`人為痕跡`, "火")|str_detect(`人為痕跡`, "燒"), "yes", "no")) %>%
  filter(is.na(refitted)) # for NISP

# taxa counts
fauna_combined_taxa <-
  fauna_combined_context_all %>%
  count(taxa, `Weight (g)`) %>%
  mutate(taxa = str_replace(taxa, "cervidae", "cervid")) %>%
  mutate(taxa = case_when(str_detect(taxa, "mm") ~ paste("Indeterminate", tolower(taxa), sep =" "),
                          TRUE ~ taxa)) %>%
  mutate(`Common name` = case_when(str_detect(taxa, "Cer") ~ "Sika deer", str_detect(taxa, "Bub") ~
                                     "Water buffalo", str_detect(taxa, "Rus") ~ "Sambar deer",
                                   str_detect(taxa, "Ree") ~ "Muntjac", taxa == "Sus scrofa" ~ "Suid",
                                   str_detect(taxa, "Bos") ~ "Yellow cattle", str_detect(taxa, "Rat") ~
                                     "Rat", str_detect(taxa, "Pli") ~ "Catfish", str_detect(taxa, "Act") ~
                                     "Ray-finned fish", str_detect(taxa, "Tes") ~ "Turtle",
                                   str_detect(taxa, "cervid") ~ "Deer", taxa == "Sus scrofa domestica" ~
                                     "Domestic pig", taxa == "Aves" ~ "Birds")) %>%
  mutate(group = case_when(str_detect(`Common name`, "deer") ~ 1, str_detect(`Common name`, "Deer") ~ 2,
                           str_detect(taxa, "Sus") ~ 3, str_detect(taxa, "Bub")|str_detect(taxa, "Bos") ~ 4,
                           str_detect(taxa, "munt") ~ 5, str_detect(taxa, "Rat") ~ 6,
                           str_detect(`Common name`, "fish") ~ 7, str_detect(taxa, "Tes") ~ 8,
                           str_detect(taxa, "Ave") ~ 9, str_detect(taxa, "large m") ~ 10,
                           str_detect(taxa, "medium m") ~ 11, TRUE ~ 12)) %>%
  group_by(group) %>%
  arrange(desc(`Weight (g)`), .by_group = TRUE) %>%
  ungroup () %>%
  rename(Taxon = taxa, NISP = n) %>%
  select(Taxon, `Common name`,  NISP, `Weight (g)`) %>%
  bind_rows(summarise(., across(where(is.numeric), sum), across(where(is.character), ~"Total"))) %>%
  mutate(`Common name`= ifelse(is.na(`Common name`)|`Common name` == "Total", "-", `Common name`)) %>%
  mutate(Taxon = ifelse(Taxon == "Reeves's muntjac", "Muntiacus reevesi", Taxon)) %>%
  mutate(Taxon = ifelse(Taxon == "Cervus nippon", "Cervus nippon taiouanus", Taxon))

# calculate MNI
fauna_combined_MNI <-
  fauna_combined_context_all %>%
  filter(!is.na(`部位/左右`)) %>%
  count(`部位/左右`, category, `部位/名稱`)

fauna_combined_context <- fauna_combined_context_all %>% filter(!taxa == "Rattus sp.") # remove rat

################### NISP & Taxonomic abundance ###################
# barplot I: NISP by period (NISP)
NISP_barplot <-
  fauna_combined_context %>%
  count(period) %>%
  drop_na() %>%
  ggplot(aes(x = period, y = n)) +
  geom_col(width = 0.6) + #geom_bar(stat = "identity", #  geom_col(width = 0.6) +  #
           #position = position_dodge2(preserve = "single"), widtg = 0.6) +
  labs(y = "NISP", x = NULL) +
  theme_minimal() +
  theme(legend.title=element_blank())

# barplot: across taxon
NISP_barplot_taxon <-
  fauna_combined_context %>%
  count(period, category) %>%
  drop_na() %>%
  ggplot(aes(x = period, y = n, fill = category)) +
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"), widtg = 0.6) +
  labs(y = "NISP", x = NULL) +
  theme_minimal() +
  theme(legend.title=element_blank())

# barplot II: broken bones (all taxa)
fauna_broken <-
  kwl_fauna_broken %>%
  mutate(area = ifelse(is.na(area), "C", area)) %>% # assign to blanks that won't change the pattern
  mutate(Pit = paste(context,pit, sep = "")) %>%
  filter(Pit %in% sample_p$the_sq) %>%
  left_join(kwl_chro_tidy_6) %>%
  filter(!is.na(period)) %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`))

fauna_broken_plot <-
  fauna_broken %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"), order = T)) %>%
  ggplot(aes(x= period, y= weight)) +
  geom_col(width = 0.6)+
  labs(x= NULL, y = "Small bone (g)") +
  theme_minimal()

# barplot III: classes of vertebrates by period (normed NISP across classes)
verte_class_barplot_normed <-
  fauna_combined_context %>%
  count(period, class) %>% #class
  drop_na(period, class) %>%
  mutate(NNISP = case_when(str_detect(class, "mammal") ~ n/205,
                           str_detect(class, "bird") ~ n/120,
                           str_detect(class, "fish") ~ n/150,
                           str_detect(class, "reptile") ~ n/200)) %>%
  mutate(class = factor(class, levels = c("mammal", "bird", "fish", "reptile"), ordered = TRUE)) %>%
  group_by(period) %>%
  summarise(total_per_period = sum(NNISP), across()) %>%
  mutate(`%NNISP` = (NNISP/total_per_period)*100) %>%
  ggplot(aes(x = period, y = `%NNISP`))+
  geom_bar(stat = "identity", aes(fill = class)) +
  labs(y = "%NNISP", x = NULL) +
  theme_minimal()

# barplot V: relative abundance of mammals by period
library(viridis)
verte_mammal_barplot <-
  fauna_combined_context %>%
  filter(class == "mammal") %>%
  drop_na(period, category) %>% # remove category to get absolute abundance
  mutate(category = factor(category, levels = c("deer", "suid", "muntjac", "bovine"), ordered = TRUE)) %>%
  ggplot(aes(x = period, fill = category))+
  geom_bar(position = "fill") +
  labs(y = "%NISP", x = NULL) +
  scale_y_continuous(labels = scales::percent) +  #scales::percent
  theme_minimal() +
  scale_fill_viridis_d(option="magma", begin = 0.2, end = 0.9) +  #plasma
  guides(fill= guide_legend(title="mammal"))

library(cowplot)
top <- plot_grid(NISP_barplot, fauna_broken_plot, labels = c("A","B"))
bottom <- plot_grid(verte_class_barplot_normed, verte_mammal_barplot, labels = c('C', 'D'))
plot_grid(top, bottom, ncol = 1, align = "h", axis = 'l', rel_heights = c(1,1.2))

ggsave(here::here("analysis","figures", "NISP_pro_by_layer.png"),
       bg = "white",width = 7, height = 4, dpi = 360, units = "in")

################### ubiquity ###################
# calculate the number of units for each cultural layers
kwl_unit_per_period <-
  kwl_chro_6 %>%
  mutate(all = pmap_chr(select(., -Pit, -Grid), ~toString(unique(na.omit(c(...)))))) %>% # unite & remove duplicates
  mutate(unit = str_extract(Pit, "[A-Z]+[0-9]{3}")) %>%
  distinct(unit, .keep_all = TRUE) %>%
  mutate(CL1 = ifelse(str_detect(all, "CL1"), unit, NA)) %>% mutate(CL2 = ifelse(str_detect(all, "CL2"), unit, NA)) %>%
  mutate(CL3 = ifelse(str_detect(all, "CL3"), unit, NA)) %>% mutate(CL4 = ifelse(str_detect(all, "CL4"), unit, NA)) %>%
  mutate(CL5 = ifelse(str_detect(all, "CL5"), unit, NA)) %>% mutate(CL6 = ifelse(str_detect(all, "CL6"), unit, NA))

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

################### Skeletal part ###################
fauna_deer_only <-
  fauna_combined_context %>%
  filter(category %in% c("deer") & !is.na(period))

deer_element_BMD <- # based on Reitz and Wing (2008: 246, 247)
  fauna_deer_only %>%
  filter(!is.na(`部位/名稱`)) %>%
  count(`部位/名稱`,`部位/位置`) %>%
  mutate(part = paste0(`部位/名稱`,`部位/位置`)) %>%
  mutate(BMD = case_when(part == "astragaluscom." ~ 0.61, part %in% c("下顎及齒中段","下顎及齒片段","下顎骨片段", "下顎及齒前段") ~ 0.57,
         part == "下顎骨art." ~ 0.36, part == "下顎骨prox." ~ 0.61, part %in% c("尺骨art.", "尺骨com.", "尺骨prox.art.") ~ 0.45,
         part == "尺骨片段" ~ 0.44, part == "掌骨dis." ~ 0.58, part == "掌骨prox." ~ 0.69, part == "掌骨prox.art." ~ 0.56,
         part == "掌骨或蹠骨dis." ~ 0.575 , part == "掌骨或蹠骨dis.art." ~ 0.505, part == "掌骨或蹠骨prox." ~ 0.67,
         part %in% c("掌骨或蹠骨片段", "掌骨dis.sh.l") ~ 0.74, part == "橈骨dis." ~ 0.38, part == "橈骨dis.art." ~ 0.43,
         part == "橈骨prox." ~ 0.62, part == "橈骨prox.art." ~ 0.42, part %in% c("橈骨中段", "橈骨prox.art.sh.l") ~ 0.68,
         part %in% c("第1趾骨com.", "第1趾骨dis.") ~ 0.57, part  %in% c("第1趾骨片段", "趾骨片段") ~ 0.42, part == "第1趾骨prox." ~ 0.36,
         part == "第2趾骨prox." ~ 0.28, part %in% c("第2趾骨com.", "第2趾骨dis.","第2趾骨dis.art.") ~ 0.35,
         part == "第3趾骨com." ~ 0.25, part == "第二頸椎片段" ~ 0.16, part == "肋骨prox." ~ 0.26, part == "肋骨dor." ~ 0.25,
         part == "肋骨中段" ~ 0.40, part == "肋骨片段" ~ 0.24, part == "股骨dis.art." ~ 0.28, part == "股骨prox." ~ 0.36,
         part == "股骨prox.art." ~ 0.41, part %in% c("股骨中段", "股骨片段") ~ 0.57, part %in% c("肩胛骨art.", "肩胛骨dis.art.") ~ 0.36,
         part %in% c("肩胛骨dis.", "肩胛骨中段") ~ 0.49, part == "肩胛骨片段" ~ 0.34, part %in% c("肱骨dis.art.sh.", "肱骨dis.sh.")  ~ 0.63,
         part == "肱骨dis.art."  ~ 0.39, part %in% c("肱骨prox.art.", "肱骨prox.")  ~ 0.24, part %in% c("肱骨中段", "肱骨片段")  ~ 0.53,
         part == "胸椎片段" ~ 0.24, part == "胸骨片段" ~ 0.22, part == "脊椎片段"  ~ 0.27, part == "腰椎axi."  ~ 0.29,
         part %in% c("脛骨dis.", "脛骨dis.art.sh.") ~ 0.51, part == "脛骨dis.art."  ~ 0.50, part %in% c("脛骨prox.", "脛骨prox.art.") ~ 0.30,
         part %in% c("脛骨中段", "脛骨dis.art.sh.l") ~ 0.74, part == "腕骨cun.com." ~ 0.72, part == "腕骨trap.com." ~ 0.74,
         part == "腕骨sca.com." ~ 0.98, part == "腕骨unc.com." ~ 0.78, part %in% c("薦骨axi.", "薦骨片段") ~ 0.19,
         part %in% c("跗骨com.", "跗骨片段") ~ 0.62, part %in% c("跟骨com.", "跟骨片段") ~ 0.64, part == "跟骨dis.art." ~ 0.41,
         part == "蹠骨dis.art." ~ 0.50, part %in% c("蹠骨dis.sh.l", "蹠骨中段") ~ 0.74, part == "蹠骨prox." ~ 0.65,
         part == "蹠骨prox.art." ~ 0.55, part == "頸椎prox.art." ~ 0.16, part == "髖骨art." ~ 0.27, part == "髖骨isc." ~ 0.41,
         part == "髖骨ili.art." ~ 0.49,  part == "上肢骨中段" ~ 0.63)) %>%
  filter(!is.na(BMD)) %>%
  group_by(`部位/名稱`, BMD) %>%
  summarise(NISP = sum(n, na.rm = T)) %>%
  mutate(exp_NISP = case_when(`部位/名稱` %in% c("第1趾骨", "第2趾骨", "趾骨") ~ 17*8, `部位/名稱` == "第3趾骨" ~ 17*4,
                              `部位/名稱` %in% c("第二頸椎","頸椎", "薦骨", "胸骨") ~ 17*1, `部位/名稱` == "腰椎" ~ 17*6,
                              `部位/名稱` %in% c ("脊椎", "胸椎") ~ 17*13, `部位/名稱` == "肋骨" ~ 17*26,
                              TRUE ~ 17*2)) %>%
  mutate(sur_per = NISP/exp_NISP*100)

write.csv(deer_element_BMD, here::here("analysis", "data", "derived_data", "deer_BMD.csv"))

Survival_BMD <-
  deer_element_BMD %>%
  ggplot(aes(x= BMD, y= sur_per)) +
  geom_point() +
  labs(x= "Bone Mineral Density", y= "% Survivorship") +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  theme_minimal()

lm_sur_BMD <- summary(lm(NISP ~ BMD, data = deer_element_BMD))
cor_sur_BMD <- cor.test(deer_element_BMD$NISP, deer_element_BMD$BMD)

fauna_deer_portion <-
  fauna_deer_only %>%
  mutate(portion = case_when(
    `部位/名稱` %in% c("胸椎","腰椎","頸椎","第二頸椎","脊椎","肋骨","薦骨","胸骨") ~ "axial",
    `部位/名稱` %in% c("肩胛骨","肱骨","尺骨","橈骨") ~ "upper forelimb", #upper limb, "上肢骨"
    `部位/名稱` %in% c("脛骨","髖骨","股骨") ~ "upper hindlimb",
    `部位/名稱` %in% c("掌骨","掌骨或蹠骨","蹠骨","跗骨","腕骨","跟骨","astragalus") ~ "lower limb",
    `部位/名稱` %in% c("上顎骨","上顎及齒","下顎及齒","下顎骨","枕骨","頭骨","顱骨", "角基部") ~ "head", #
    `部位/名稱` %in% c("第1趾骨","第2趾骨","第3趾骨","趾骨") ~ "foot"))  #"上顎齒","下顎齒","臼齒","齒"

deer_axial <-
  fauna_deer_portion %>%
  filter(portion == "axial") %>%
  select()

deer_metapodials <-
  fauna_deer_only %>%
  mutate(metapodials = case_when(`部位/名稱` == "掌骨" ~ "metacarpals", `部位/名稱` == "蹠骨" ~ "metatarsals",
                                 `部位/名稱` == "掌骨或蹠骨" ~ "metapodials")) %>%
  filter(!is.na(metapodials)) %>%
  count(period, metapodials)

deer_portion_plot <-
  fauna_deer_portion %>%
  select(period, portion, animal) %>%
  drop_na() %>%
  count(period, portion) %>%
  mutate(period = factor(period, levels = c("CL1","CL2","CL3","CL4","CL5","CL6"))) %>% #ordered = TRUE
  mutate(portion= factor(portion, levels = c('head','upper forelimb','upper hindlimb','axial','lower limb','foot'))) %>%
  ggplot(aes(period, n, fill = portion)) + #fill = portion
  geom_bar(stat = "identity",
           position = position_dodge2(preserve = "single"), widtg = 0.6) + #stat = "identity", position = "fill"
  labs(x = "Cultural layer", y = "NISP") +
  guides(fill= guide_legend(title="deer portion")) +
  scale_fill_viridis_d(labels = c('head','upper forelimb','upper hindlimb','axial','lower limb','foot')) +
  theme_minimal()

# long bones
deer_long_bone <-
  fauna_deer_only %>%
  filter(`部位/名稱` %in% c("脛骨","股骨","肱骨","橈骨","尺骨","上肢骨","掌骨","掌骨或蹠骨","蹠骨")) %>%
  count(period, `部位/位置`) %>%
  mutate(zone = case_when(str_detect(`部位/位置`, "prox.")|`部位/位置` == "art." ~ "proximal",
                          str_detect(`部位/位置`, "dis.") ~ "distal", str_detect(`部位/位置`, "com.") ~ "complete",
                          str_detect(`部位/位置`, "段") ~ "medial"))

long_bone_period <-
  fauna_deer_only %>%
  filter(`部位/名稱` == "上肢骨") %>%
  count(period)

deer_longbone_pro <-
  deer_long_bone %>%
  ggplot(aes(x = period, fill = zone)) +
  geom_bar(position = "fill", width = 0.7) +
  labs(y = "%NISP", x = "cultural layer") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

plot_grid(deer_portion_plot, Survival_BMD,
          labels = c('A', 'B'), rel_widths = c(2.2, 1), label_size = 12)

ggsave(here::here("analysis", "figures", "deer_elements.png"), h = 3, w = 10, units = "in")

################### fragmentation #######################
# calculate MNE
deer_MNE <-
  fauna_deer_only  %>%
  filter(!is.na(`部位/左右`)&is.na(overlap)) %>% #remove overlapped fragments
  count(period,`部位/左右`,`部位/名稱`,`部位/位置`) %>%
  group_by(period) %>%
  summarise(MNE = sum(n), across())

deer_NISP <- fauna_deer_only %>% count(period) %>% rename(NISP = n)

frag_index <- left_join(deer_MNE, deer_NISP) %>% mutate(index = MNE/NISP)

weight_head <-
  fauna_deer_portion %>%
  mutate(`重量(g)` = as.numeric(`重量(g)`)) %>%
  filter(portion == "head") %>%
  group_by(period) %>%
  summarise(weight = sum(`重量(g)`, na.rm = T)) %>%
  filter(!is.na(period)) %>%
  ggplot(aes(x= period, y=weight,  width=0.5)) +
  geom_bar(stat="identity")+
  theme_minimal()

#ggsave(here::here("analysis", "figures", "talk-fauna-6.png"), h = 2, w =4, units = "in")

################### Cut marks ###################
# count cut marks across taxa
fauna_total_cut <-
  fauna_combined_context %>%
  filter(cutmarks == "yes") %>%
  group_by(taxa, period) %>%
  count() %>%
  drop_na(period, taxa)

# joints of major limbs for deer
fauna_deer_joints <- # based on Lyman (2005)
  fauna_deer_portion  %>%
  mutate(part = paste0(`部位/名稱`,`部位/位置`)) %>%
  select(period, cutmarks, part, "人為痕跡", "部位/名稱") %>%
  mutate(joint = case_when(str_detect(part, "肩胛骨dis")|str_detect(part, "肩胛骨art")|str_detect(part, "肱骨prox") ~ "shoulder",
         str_detect(part, "肱骨dis")|str_detect(part, "橈骨prox")|str_detect(part, "尺骨prox")|str_detect(part, "尺骨art") ~ "elbow",
         str_detect(part, "橈骨dis")|str_detect(part, "尺骨dis")|str_detect(part, "腕骨")|str_detect(part, "掌骨prox") ~ "wrist",
         str_detect(part, "髖骨art")|str_detect(part, "髖骨ili")|str_detect(part, "股骨prox") ~ "hip",
         str_detect(part, "股骨dis")|str_detect(part, "脛骨prox") ~ "knee",
         str_detect(part, "脛骨dis")|str_detect(part, "跟骨") |str_detect(part, "跗骨")|
         str_detect(part, "astragalus")|str_detect(part, "蹠骨prox") ~ "ankle")) #"蹠骨(或掌骨)"

# deer joints with cutmarks
fauna_deer_cut <-
  fauna_deer_joints %>%
  filter(cutmarks == "yes"|str_detect("人為痕跡", "砍")) %>%
  group_by(period, joint) %>%
  count() %>%
  rename(`NISP with cutmarks` = n) %>%
  drop_na()

# deer joints in total
deer_cut_pro <-
  fauna_deer_joints %>%
  group_by(period, joint) %>%
  count() %>%
  left_join(fauna_deer_cut) %>%
  rename(NISP = n) %>%
  drop_na() %>%
  #mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  #mutate(proportion = round(`NISP with cutmarks`/NISP, 2), NNISP = NISP/2) %>% #%NNISP= NNISP/sum(NNISP)
  group_by(period) %>%
  mutate(proportion  = `NISP with cutmarks`/NISP)

# plot joins with cut marks
deer_cut_plot <-
  ggplot(deer_cut_pro, aes(x = period, y = proportion,
         fill = factor(joint, levels = (c("shoulder","elbow","wrist","hip","knee","ankle"))))) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_color_discrete(breaks=c("CL1", "CL2", "CL3", "CL4", "CL5", "CL6")) +
  labs(x = "Cultural layer", y = "%NISP") +
  guides(fill= guide_legend(title="joints"))+
  scale_fill_viridis_d(labels=c("shoulder","elbow","wrist","hip","knee","ankle")) +
  theme_minimal()

ggsave(here::here("analysis", "figures", "deer_cutmarks.png"), w =6, h = 2.5, units = "in")

# t-test (for two groups only)
res <- deer_cut_pro %>% filter(period %in% c("CL4", "CL5"))
t.test(proportion ~ period, data = res)

################### cut marks related to skinning ###################
# Skeletal elements related to skinning
deerskin_portion <-
  fauna_deer_only %>%
  mutate(skinning_cut = case_when(
    str_detect(.$`部位/名稱`, "掌骨")|str_detect(.$`部位/名稱`, "蹠骨") ~ "metapodials",
    str_detect(.$`部位/名稱`, "腕骨")|str_detect(.$`部位/名稱`, "跗骨") ~ "carpal/tarsal",
    str_detect(.$`部位/名稱`, "趾骨") ~ "phalanges", str_detect(.$`部位/名稱`, "頸椎")|
    str_detect(.$`部位/名稱`, "枕骨") ~ "neck/tail regions", TRUE ~ "others"))

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
  scale_fill_viridis_d(labels=c('carpal/\ntarsal', 'metapodials', 'neck/\ntail regions', 'phalanges')) +
  theme_minimal()

# NISP with skinning cut marks
deerskin_portion_NISP_cut <-
  deerskin_portion %>%
  filter(!skinning_cut == "others") %>%
  filter(str_detect(`人為痕跡`, "s")) %>%
  group_by(period, skinning_cut) %>%
  count() %>%
  left_join(deerskin_portion_NISP) %>%
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

# combine
plot_grid(deer_cut, deer_cut_skinning, labels = c('A', 'B'), rel_widths = c(1, 1.55), label_size = 12)

ggsave(here::here("analysis", "figures", "deer-cut.png"), h = 4, w =8, units = "in")

# t-test
res <- t.test(cut_percent ~ period, data = fauna_deer_skin_NNISP)

###############modification################
# bones with surface modifications
burned_deer <- fauna_deer_only %>% filter(burning == "yes") %>% count(period) %>% rename(burned = n)
cutmark_deer <- fauna_deer_only %>% filter(cutmarks == "yes") %>% count(period) %>% rename(cutmarks = n)
frac_deer <- fauna_deer_only %>% filter(fractures  == "yes") %>%
  filter(`部位/名稱` %in% c("脛骨","股骨","肱骨","橈骨","上肢骨","掌骨","掌骨或蹠骨","蹠骨")) %>%
  count(period) %>% rename(fractures = n)
post_deposit <-fauna_deer_only %>% filter(!is.na(`保存狀態`)) %>% count(period) %>%
  rename(`post-deposition` = n)

list_modif <- list(burned_deer, cutmark_deer, frac_deer, post_deposit, deer_NISP)

# join
deer_modify <-
  list_modif %>%
  reduce(inner_join, by = 'period') %>%
  mutate(cut_per = round(cutmarks/NISP, 3) * 100, burn_per = round(burned/NISP, 3) * 100,
         frac_per = round(fractures/NISP, 3) * 100, post_per= round(`post-deposition`/NISP, 3) * 100) %>%
  mutate(burned = paste(burned, paste0("(", burn_per, " %)")),
         cutmarks = paste(cutmarks, paste0("(", cut_per, " %)")),
         fractures = paste(fractures, paste0("(", frac_per, " %)")),
         `post-deposition` = paste(`post-deposition`, paste0("(", post_per, " %)"))) %>%
  rename(`cut marks` = cutmarks) %>%
  select(-cut_per, -frac_per, -burn_per, -post_per)

# other exploration
root <-fauna_deer_only %>% filter(str_detect(`保存狀態`, "X")) %>% count(period) %>% left_join(deer_NISP) %>%
  mutate(root_per = n/NISP)
weather <-fauna_deer_only %>% filter(str_detect(`保存狀態`, "$")) %>% count(period) %>% left_join(deer_NISP) %>%
  mutate(weather_per = n/NISP)

deer_age <-
  fauna_deer_only %>%
  filter(!is.na(`部位/左右`)&is.na(overlap)) %>% #remove overlapped fragments
  mutate(age = case_when(str_detect(`個體/年齡`, "幼")~"young", TRUE ~ "mature")) %>%
  filter(!is.na(age)) %>%
  count(period, age,`部位/左右`,`部位/名稱`)

# pig
pig_bone <-
  fauna_combined_context %>%
  filter(category == "suid") %>%
  count(period, `部位/名稱`, `部位/左右`, `個體/年齡`)

pig_M3 <-
  fauna_combined_context %>%
  filter(category == "suid") %>%
  filter(str_detect(`部位/名稱`, "齒"))
