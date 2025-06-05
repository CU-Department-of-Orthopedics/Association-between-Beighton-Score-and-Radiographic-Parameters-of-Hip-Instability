## Analysis 

rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(gtsummary)
library(gt)

extrafont::font_import(prompt = FALSE)
extrafont::loadfonts(device = "win", quiet = TRUE)

dat_og <- ...

theme_times <- theme_classic(base_family = "Times New Roman", base_size = 14) +
  theme(
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 24),
    axis.text.y = element_text(color = "black"),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(2, 6, 2, 5),
    axis.title.y = element_text(margin = margin(r = 10))
  )

theme_set(theme_times)

# Rocky Mountain color palette
RMNP_colors <- c("#EAEBE4", "#FEBA15", "#5E8C61", "#6E4CBA", "#EC8FCB")

### 1. Beighton Score relationship with 3D morphology measurements
{
# o	LCEA
{
lcea_mod <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og)
summary(lcea_mod)
lce_mod_sum <- lcea_mod %>% 
  tbl_regression()
}

# o	Acetabular coverage
{
Acetabular_Coverage_mod <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Acetabular_Coverage_mod)

Acetabular_Coverage_mod_sum <- Acetabular_Coverage_mod %>% 
  tbl_regression()
}

# o	Acetabular Surface Area
{
Acetabular_Surface_Area_mod <- lmer(Acetabular_Surface_Area ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Acetabular_Surface_Area_mod)

Acetabular_Surface_Area_mod_sum <- Acetabular_Surface_Area_mod %>% 
  tbl_regression()
}
  
# o	Acetabular version 12 o’clock
{
Acetabular_Version_12_mod <- lmer(Acetabular_Version_12 ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_12_mod)

Acetabular_Version_12_mod_sum <- Acetabular_Version_12_mod %>% 
  tbl_regression()
}
  
# o	Acetabular version 2 o’clock
{
Acetabular_Version_2_mod <- lmer(Acetabular_Version_2 ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_2_mod)

Acetabular_Version_2_mod_sum <- Acetabular_Version_2_mod %>% 
  tbl_regression()
}
  
# o	Acetabular version 3 o’clock
{
Acetabular_Version_3_mod <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_3_mod)

Acetabular_Version_3_mod_sum <- Acetabular_Version_3_mod %>% 
  tbl_regression()
}

# o	Tonnis
{
Tonnis_mod <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Tonnis_mod)

Tonnis_mod_sum <- Tonnis_mod %>% 
  tbl_regression()
}

# o	Femoral Torsion
{
Femoral_Torsion_mod <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Femoral_Torsion_mod)

Femoral_Torsion_mod_sum <- Femoral_Torsion_mod %>% 
  tbl_regression() 
}

# o	Femoral Neck Shaft Angle
{
FNSA_mod <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og)
summary(FNSA_mod)

FNSA_mod_sum <- FNSA_mod %>% 
  tbl_regression()
}

# o	Combined Version
{
Combined_Version_mod <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og)
summary(Combined_Version_mod)

Combined_Version_mod_sum <- Combined_Version_mod %>% 
  tbl_regression()
}


## Combined Regression models 
# Explicitly list regression summary tables in correct order
sum_tables <- list(
  Acetabular_Coverage_mod_sum,
  Acetabular_Surface_Area_mod_sum,
  Acetabular_Version_12_mod_sum,
  Acetabular_Version_2_mod_sum,
  Acetabular_Version_3_mod_sum,
  Combined_Version_mod_sum,
  Femoral_Torsion_mod_sum,
  FNSA_mod_sum,
  lce_mod_sum,
  Tonnis_mod_sum
)

# Define corresponding labels in same order
tab_labels <- c(
  "Acetabular Coverage",
  "Acetabular Surface Area",
  "Acetabular Version 12 o'clock",
  "Acetabular Version 2 o'clock",
  "Acetabular Version 3 o'clock",
  "Combined Version",
  "Femoral Torsion",
  "FNSA",
  "LCEA",
  "Tönnis"
)

# Apply labels to each summary table
sum_tables <- Map(function(tbl, name) 
                    modify_header(tbl, label = paste("**", name, "**")), 
                  sum_tables, tab_labels)

# Merge into a wide table
wide_tbl <- tbl_merge(
  tbls = sum_tables,
  tab_spanner = tab_labels
) %>% 
  modify_header(label = " ")

# Show final merged table
wide_tbl


}

### 2. Demographics Analysis
{
## Summary Table Fn
{
  
  library(gtsummary)
  library(gt)
  library(nlme)
  
  reset_gtsummary_theme() 
  
  summary_render <- list(
    all_continuous() ~ c(
      "{mean} ({sd})", 
      "{median} ({p25}, {p75})",
      "{min}, {max}"),      
    all_categorical() ~ "{n} ({p}%)"
  )
  
  lme_stat_function <- function(data, by, variable, group, ...) {
    model <- nlme::lme(
      data = data, 
      fixed = reformulate(response = variable, termlabels = by),
      random = reformulate(glue::glue("1 | {group}")),
      na.action = na.omit
    )
    
    res <- anova(model) # Extracts p-value
    
    tibble::tibble(
      variable = variable,
      test = "Linear Mixed Model",
      p.value = res[by, "p-value"]  # Extract correct p-value
    )
  }
  
  chisq.lmm <- function(data, variable, by, group, ...) {   
    test_res <- chisq.test(x = data[[variable]], y = as.factor(data[[by]]), correct = FALSE, simulate.p.value = TRUE) %>%    
      broom::tidy()
    
    tibble::tibble(
      variable = variable,
      test = "Chi-Square Test",
      p.value = test_res$p.value
    )
  }
  
  anova_stat_render <- list(
    all_continuous() ~ "aov", 
    all_categorical() ~ "chisq.test")
  
  
  ttest_stat_render <- list(
    all_continuous() ~ "t.test", 
    all_categorical() ~ "chisq.lmm")
  
  lme_stat_render <- list(
    all_continuous() ~ lme_stat_function,
    all_categorical() ~ chisq.lmm
  )
  
  }

dat_sum <- dat_og %>% 
  mutate(
    age_group = case_when(
      Age < 20 ~ "0-19",
      Age >= 20 & Age < 45 ~ "20-44",
      Age >= 45 ~ "45+"
    ),
    race_group = case_when(
      Race != "White or Caucasian" ~ "Non-White or Caucasian",
      TRUE ~ "White or Caucasian"
    ),
    Gender = str_to_title(toupper(Gender))
  )


## Comparison of Hips 
dat_sum %>%
  select( -Design, -HipID) %>%
  tbl_summary(
    include = -Patient_ID,
    by = Instability_Group,
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(

    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = lme_stat_render,
    group = Patient_ID
  )

## Overall Summaries 
dat_cat <- dat_sum %>% 
  select(Patient_ID, where(is.character)) %>% 
  group_by(Patient_ID) %>% 
  slice(1) %>% 
  ungroup()

dat_num <- dat_sum %>% 
  select(Patient_ID, where(is.numeric)) %>% 
  select(-HipID) %>% 
  group_by(Patient_ID) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

dat_sum_overall <- merge(dat_cat, dat_num, by = "Patient_ID")         

dat_sum_overall %>% 
  select(-Patient_ID) %>%
  tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      
    )
  ) %>%
  bold_labels() %>%
  italicize_levels()
}

### 3. Comparison of Hip Morphology Measures Across Groups (n = no. hips)
{
## By Legal Sex 

dat_og %>%
  select(Patient_ID, Legal_Sex, LCEA:Combined_Version, Beighton) %>%
  tbl_summary(
    include = -Patient_ID,
    by = Legal_Sex,
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      
    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = lme_stat_render,
    group = Patient_ID
  )

## By Age Group 

dat_og %>%
  mutate(
    age_group = case_when(
      Age < 20 ~ "0-19",
      Age >= 20 & Age < 45 ~ "20-44",
      Age >= 45 ~ "45+"
    )
  ) %>% 
  select(Patient_ID, age_group, LCEA:Combined_Version, Beighton) %>%
  tbl_summary(
    include = -Patient_ID,
    by = age_group,
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      
    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = lme_stat_render,
    group = Patient_ID
  )


## By Simplified Race 
dat_og %>%
  mutate(
    race_group = case_when(
      Race != "White or Caucasian" ~ "Non-White or Caucasian",
      TRUE ~ "White or Caucasian"
    )
  ) %>% 
  select(Patient_ID, race_group, LCEA:Combined_Version, Beighton) %>%
  tbl_summary(
    include = -Patient_ID,
    by = race_group,
    type = all_continuous() ~ "continuous2",
    statistic = summary_render,
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      
    )
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_p(
    test = lme_stat_render,
    group = Patient_ID
  )
}

### 4. Beighton Association by Sex, Age, and Race
{
dat_og <- dat_og %>%
  mutate(
    age_group = case_when(
      Age < 20 ~ "0-19",
      Age >= 20 & Age < 45 ~ "20-44",
      Age >= 45 ~ "45+"
    ),
    race_group = case_when(
      Race != "White or Caucasian" ~ "Non-White or Caucasian",
      TRUE ~ "White or Caucasian"
    )
  ) 

# o	Acetabular version 3 o’clock
Acetabular_Version_3_mod2 <- lmer(Acetabular_Version_3 ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_3_mod2) # Beighton trend 
car::Anova(Acetabular_Version_3_mod2, type = "II") # Effects 

## Comparison of Legal Sex (p < 0.001)
emmeans(Acetabular_Version_3_mod2, specs = pairwise ~ Legal_Sex)

# o	Acetabular version 12 o’clock
Acetabular_Version_12_mod2 <- lmer(Acetabular_Version_12 ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_12_mod2) # Beighton trend 
car::Anova(Acetabular_Version_12_mod2, type = "II") # Effects 

## Comparison of Legal Sex (p < 0.001)
emmeans(Acetabular_Version_12_mod2, specs = pairwise ~ Legal_Sex)

# o	Acetabular version 2 o’clock
Acetabular_Version_2_mod2 <- lmer(Acetabular_Version_2 ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Acetabular_Version_2_mod2) # Beighton trend 
car::Anova(Acetabular_Version_2_mod2, type = "II") # Effects 

## Comparison of Legal Sex (p < 0.001)
emmeans(Acetabular_Version_2_mod2, specs = pairwise ~ Legal_Sex)

# o	Femoral Torsion
Femoral_Torsion_mod2 <- lmer(Femoral_Torsion ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Femoral_Torsion_mod2) # Beighton trend 
car::Anova(Femoral_Torsion_mod2, type = "II") # Effects

# o	Combined Version
Combined_Version_mod2 <- lmer(Combined_Version ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Combined_Version_mod2) # Beighton trend
car::Anova(Combined_Version_mod2, type = "II") # Effects

## Comparison of Legal Sex (p < 0.001)
emmeans(Combined_Version_mod2, specs = pairwise ~ Legal_Sex)

# o	Acetabular Surface Area
Acetabular_Surface_Area_mod2 <- lmer(Acetabular_Surface_Area ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
summary(Acetabular_Surface_Area_mod2) # Beighton trend
car::Anova(Acetabular_Surface_Area_mod2, type = "II") # Effects

## Comparison of Legal Sex (p < 0.001)
emmeans(Acetabular_Surface_Area_mod2, specs = pairwise ~ Legal_Sex)
}

### 5 Stacked and Side by Side Bar Charts
{
  dat_proportion <- dat_og %>%  
    count(Beighton, Instability_Group) %>%  
    group_by(Beighton) %>%  
    mutate(
      proportion = n/sum(n),  
      label = scales::percent(proportion, accuracy = 1)
    )
  
  # Stacked Instability 
  {
    stability_plot <- ggplot(
      dat_proportion, 
      aes(
        y = factor(Beighton), 
        x = proportion,
        fill = Instability_Group
      )
    ) +
      geom_col(color = "black") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        size = 5,
        color = "white",
        fontface = "bold",
        family = "Times New Roman"
      ) +
      scale_y_discrete(
        breaks = as.character(0:9), 
        limits = as.character(0:9)
      ) +
      scale_x_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c(
          "Hip with Instability" = RMNP_colors[4],
          "Hip without Instability" = RMNP_colors[5]
        ),
        breaks = c("Hip without Instability", "Hip with Instability")
      ) +
      labs(
        x = "Percent", 
        y = "Beighton Score",
        fill = NULL
      )
  }
  
  dat_proportion_sex <- dat_og %>%  
    count(Beighton, Legal_Sex) %>%  
    group_by(Beighton) %>%  
    mutate(
      proportion = n/sum(n), 
      label = scales::percent(proportion, accuracy = 1) 
    )

  # Stacked Sex 
  {
    sex_plot <- ggplot(
      dat_proportion_sex, 
      aes(
        y = factor(Beighton), 
        x = proportion,
        fill = Legal_Sex
      )
    ) +
      geom_col(color = "black") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5,
        fontface = "bold",
        family = "Times New Roman"
      ) +
      scale_y_discrete(
        breaks = as.character(0:9), 
        limits = as.character(0:9)
      ) +
      scale_x_continuous(labels = scales::percent) +
      scale_fill_manual(
        values = c(
          "Female" = RMNP_colors[2], 
          "Male" = RMNP_colors[3]
        ),
        breaks = c("Male", "Female")  # controls legend order
      ) +
      labs(
        x = "Percent", 
        y = "Beighton Score", 
        fill = NULL
      )
    }
    
  # Grid of stacked
  library(cowplot)
  
  plot_grid_main <- plot_grid(
    stability_plot, sex_plot,
    ncol = 2,
    align = "hv",
    axis = "tblr"
  )
  
  plot_grid_main
  
}

### 6. Tables
{
  # 1.	Hips with instability
  
  lcea_mod_sum_insta <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Coverage_mod_sum_insta <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Surface_Area_mod_sum_insta <- lmer(Acetabular_Surface_Area ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_12_mod_sum_insta <- lmer(Acetabular_Version_12 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_2_mod_sum_insta <- lmer(Acetabular_Version_2 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_3_mod_sum_insta <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Femoral_Torsion_mod_sum_insta <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  FNSA_mod_sum_insta <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Combined_Version_mod_sum_insta <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Tonnis_mod_sum_insta <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  sum_tables1 <- mget(ls(pattern = "_sum_insta"))
  
  sum_tables1 <- Map(function(tbl, name) modify_header(tbl, label = paste("**", name, "**")), 
                     sum_tables1, tab_labels)
  
  wide_tbl1 <- tbl_merge(
    tbls = sum_tables1,
    tab_spanner = tab_labels
  ) %>% 
    modify_header(label = "Hips with Instability")
  
  wide_tbl1
  
  
  # 2.	Hips without instability
  lcea_mod_sum_no_insta <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Acetabular_Coverage_mod_sum_no_insta <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Acetabular_Surface_Area_mod_sum_no_insta <- lmer(Acetabular_Surface_Area ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_12_mod_sum_no_insta <- lmer(Acetabular_Version_12 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_2_mod_sum_no_insta <- lmer(Acetabular_Version_2 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Acetabular_Version_3_mod_sum_no_insta <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Femoral_Torsion_mod_sum_no_insta <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  FNSA_mod_sum_no_insta <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Combined_Version_mod_sum_no_insta <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  Tonnis_mod_sum_no_insta <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip without Instability")) %>% 
    tbl_regression()
  
  sum_tables2 <- mget(ls(pattern = "_sum_no_insta"))
  
  sum_tables2 <- Map(function(tbl, name) modify_header(tbl, label = paste("**", name, "**")), 
                     sum_tables2, tab_labels)
  
  # Merge all tables side by side (wide format)
  wide_tbl2 <- tbl_merge(
    tbls = sum_tables2,
    tab_spanner = tab_labels
  ) %>% 
    modify_header(label = "Hips without Instability")
  
  wide_tbl2
  
  
  # 3.	Females
  lcea_mod_sum_female <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Acetabular_Coverage_mod_sum_female <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Acetabular_Surface_Area_mod_sum_female <- lmer(Acetabular_Surface_Area ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Acetabular_Version_12_mod_sum_female <- lmer(Acetabular_Version_12 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Acetabular_Version_2_mod_sum_female <- lmer(Acetabular_Version_2 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Acetabular_Version_3_mod_sum_female <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Femoral_Torsion_mod_sum_female <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  FNSA_mod_sum_female <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Combined_Version_mod_sum_female <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  Tonnis_mod_sum_female <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Female")) %>% 
    tbl_regression()
  
  sum_tables3 <- mget(ls(pattern = "_sum_female"))
  
  sum_tables3 <- Map(function(tbl, name) modify_header(tbl, label = paste("**", name, "**")), 
                     sum_tables3, tab_labels)
  
  # Merge all tables side by side (wide format)
  wide_tbl3 <- tbl_merge(
    tbls = sum_tables3,
    tab_spanner = tab_labels
  ) %>% 
    modify_header(label = "Female")
  
  wide_tbl3
  
  # 4.	Males
  lcea_mod_sum_male <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Acetabular_Coverage_mod_sum_male <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Acetabular_Surface_Area_mod_sum_male <- lmer(Acetabular_Surface_Area ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Acetabular_Version_12_mod_sum_male <- lmer(Acetabular_Version_12 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Acetabular_Version_2_mod_sum_male <- lmer(Acetabular_Version_2 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Acetabular_Version_3_mod_sum_male <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Femoral_Torsion_mod_sum_male <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  FNSA_mod_sum_male <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Combined_Version_mod_sum_male <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  Tonnis_mod_sum_male <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Legal_Sex == "Male")) %>% 
    tbl_regression()
  
  sum_tables4 <- mget(ls(pattern = "_sum_male"))
  
  sum_tables4 <- Map(function(tbl, name) modify_header(tbl, label = paste("**", name, "**")), 
                     sum_tables4, tab_labels)
  
  # Merge all tables side by side (wide format)
  wide_tbl4 <- tbl_merge(
    tbls = sum_tables4,
    tab_spanner = tab_labels
  ) %>% 
    modify_header(label = "Male")
  
  wide_tbl4
}

### 7. Combined Scatter plots
{
  # Acetabular Surface Area
  {
    p1 <- ggplot(data = dat_og, aes(x = Beighton, y = Acetabular_Surface_Area)) +
      
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      
      # Labels and theme
      labs(
        x = "Beighton Score",
        y = expression("Acetabular Surface Area (mm"^2*")"),
        color = NULL
      )+
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)
                         
  }
  
  # Acetabular Version 12 o'clock
  {
    p2 <- ggplot(dat_og, aes(x = Beighton, y = Acetabular_Version_12)) +
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      labs(x = "Beighton Score", y = "Acetabular Version 12 o'clock (°)", color = NULL) +
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)+
      scale_y_continuous(breaks = seq(-30, 50, by = 10))  
  }

  # Acetabular Version 2 o'clock
  {
    p3 <- ggplot(dat_og, aes(x = Beighton, y = Acetabular_Version_2)) +
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      labs(x = "Beighton Score", y = "Acetabular Version 2 o'clock (°)", color = NULL) +
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)+
      scale_y_continuous(breaks = seq(-20, 50, by = 10))  
  }

  # Acetabular Version 3 o'clock
  {
    p4 <- ggplot(dat_og, aes(x = Beighton, y = Acetabular_Version_3)) +
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      labs(x = "Beighton Score", y = "Acetabular Version 3 o'clock (°)", color = NULL) +
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)+
      scale_y_continuous(breaks = seq(-10, 50, by = 10))  
  }

  # Combined Version
  {
    p5 <- ggplot(dat_og, aes(x = Beighton, y = Combined_Version)) +
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      labs(x = "Beighton Score", y = "Combined Version (°)", color = NULL) +
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  }
  
  # Femoral Torsion
  {
    p6 <- ggplot(dat_og, aes(x = Beighton, y = Femoral_Torsion)) +
      # All points in gray
      geom_point(color = "gray65", alpha = 0.5, size = 2) +
      # Line: All Hips
      geom_smooth(
        method = "lm",
        se = FALSE,
        aes(color = "All Hips"),
        linewidth  = 1.2
      ) +
      # Line: Females
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Female"),
        method = "lm",
        se = FALSE,
        aes(color = "Females"),
        linewidth = 1.5
      ) +
      # Line: Males
      geom_smooth(
        data = dat_og %>% filter(Legal_Sex == "Male"),
        method = "lm",
        se = FALSE,
        aes(color = "Males"),
        linewidth = 1.5
      ) +
      # Line: Hips with Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip with Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips with Instability"),
        linewidth = 1.2
      ) +
      
      # Line: Hips without Instability (longdash)
      geom_smooth(
        data = dat_og %>% filter(Instability_Group == "Hip without Instability"),
        method = "lm",
        se = FALSE,
        linetype = "longdash",
        aes(color = "Hips without Instability"),
        linewidth = 1.2
      ) +
      # Color and label mapping
      scale_color_manual(
        values = c(
          "Females" = RMNP_colors[2],
          "Males" = RMNP_colors[3],
          "Hips with Instability" = RMNP_colors[4],
          "Hips without Instability" = RMNP_colors[5],
          "All Hips" = "grey65"
        ),
        labels = c(
          "Females" = "Females",
          "Males" = "Males",
          "Hips with Instability" = "Hips with Instability",
          "Hips without Instability" = "Hips without Instability",
          "All Hips" = "All Hips"
        )
      ) +
      labs(x = "Beighton Score", y = "Femoral Torsion (°)", color = NULL) +
      scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  }
  
  ggsave("p6.png", p6, width = 14, height = 12, dpi = 300)
  
  # Grid plot
  {
    library(cowplot)
    # Define the custom cleanup theme 
    tight_theme <- theme(
      axis.title.y = element_text(margin = margin(r = 14)),
      plot.margin = margin(t = 5, r = 5, b = 20, l = 5)
    )
    
    # Remove x-axis labels from top 4 plots and apply spacing tweaks
    p1_clean <- p1 + theme(axis.title.x = element_blank(), legend.position = "none") + tight_theme
    p2_clean <- p2 + theme(axis.title.x = element_blank(), legend.position = "none") + tight_theme
    p3_clean <- p3 + theme(axis.title.x = element_blank(), legend.position = "none") + tight_theme
    p4_clean <- p4 + theme(axis.title.x = element_blank(), legend.position = "none") + tight_theme
    
    # Keep x-axis on bottom plots, just remove legend and tighten spacing
    p5_clean <- p5 + theme(legend.position = "none") + tight_theme
    p6_clean <- p6 + theme(legend.position = "none") + tight_theme
    
    # Arrange the main 2x3 plot grid
    plot_grid_main <- plot_grid(
      p1_clean, p2_clean,
      p3_clean, p4_clean,
      p5_clean, p6_clean,
      ncol = 2,
      align = "hv",
      axis = "tblr" 
    )
    
    final_plot <- plot_grid(
      plot_grid_main,
      ncol = 1,
      rel_heights = c(1, 0.08)
    )
    
    final_plot
  }
}

