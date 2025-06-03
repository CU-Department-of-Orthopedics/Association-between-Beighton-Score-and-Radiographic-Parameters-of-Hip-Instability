## Analysis 

rm(list = ls())

library(tidyverse)
library(lme4)
library(lmerTest)
library(NatParksPalettes)
library(emmeans)
library(gtsummary)
library(gt)

dat_og <-...

### 1. Beighton Score relationship with 3D morphology measurements
{
  # color pallette
  olympic_palette <- natparks.pals("Olympic")
  
  # o	LCEA
  lcea_mod <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(lcea_mod)
  lce_mod_sum <- lcea_mod %>% 
    tbl_regression()
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = LCEA
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "LCEA",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 20,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 19, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 25,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 24, 
      size = 3,
      label = "Mild Instability"
    )+
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  
  # o	Acetabular coverage
  Acetabular_Coverage_mod <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(Acetabular_Coverage_mod)
  
  Acetabular_Coverage_mod_sum <- Acetabular_Coverage_mod %>% 
    tbl_regression()
  
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Acetabular_Coverage/100
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Acetabular Coverage",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = .50,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = .50-.01, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = .55,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = .55-.02, 
      size = 3,
      label = "Mild Instability"
    ) + 
    scale_y_continuous(labels = scales::percent)+
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  # o	Acetabular version 3 o’clock
  Acetabular_Version_3_mod <- lmer(Acetabular_Version_3 ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(Acetabular_Version_3_mod)
  
  Acetabular_Version_3_mod_sum <- Acetabular_Version_3_mod %>% 
    tbl_regression()
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Acetabular_Version_3
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Acetabular Version (3 o'clock)",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 25,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 25-1, 
      size = 3,
      label = "Anteversion"
    )+
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  # o	Tonnis
  Tonnis_mod <- lmer(Tonnis ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(Tonnis_mod)
  
  Tonnis_mod_sum <- Tonnis_mod %>% 
    tbl_regression()
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Tonnis
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Tönnis Angle",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 12,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 12+1, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 10,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 10+1, 
      size = 3,
      label = "Mild Instability"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  # o	Femoral Torsion
  Femoral_Torsion_mod <- lmer(Femoral_Torsion ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(Femoral_Torsion_mod)
  
  Femoral_Torsion_mod_sum <- Femoral_Torsion_mod %>% 
    tbl_regression() 
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Femoral_Torsion
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Femoral Torsion",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 20,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 20+1, 
      size = 3,
      label = "Antetorsion"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  # o	Femoral Neck Shaft Angle
  FNSA_mod <- lmer(FNSA ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(FNSA_mod)
  
  FNSA_mod_sum <- FNSA_mod %>% 
    tbl_regression()
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = FNSA
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Femoral Neck Shaft Angle",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 140,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 140+1, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 135,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 135+1, 
      size = 3,
      label = "Mild Instability"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  
  # o	Combined Version
  Combined_Version_mod <- lmer(Combined_Version ~ Beighton + (1|Patient_ID), data = dat_og)
  summary(Combined_Version_mod)
  
  Combined_Version_mod_sum <- Combined_Version_mod %>% 
    tbl_regression()
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Combined_Version
    )
  ) + 
    geom_point(
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Instability_Group
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Combined Version",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Hip with Instability" = olympic_palette[7],
        "Hip without Instability" = olympic_palette[9],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 40,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 40+2, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 35,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 35+2, 
      size = 3,
      label = "Mild Instability"
    )+
    scale_x_continuous(limits = c(0, 9), breaks = 0:9) 
  
  
  ## Combined Regression models 
  sum_tables <- mget(ls(pattern = "_sum"))
  
  tab_labels <- c(
    "Acetabular Coverage",
    "Acetabular Version (3 o'clock)",
    "Combined Version",
    "Femoral Torsion",
    "FNSA",
    "LCEA",
    "Tönnis"
  )
  
  sum_tables <- Map(function(tbl, name) modify_header(tbl, label = paste("**", name, "**")), 
                    sum_tables, tab_labels)
  
  wide_tbl <- tbl_merge(
    tbls = sum_tables,
    tab_spanner = tab_labels
  ) %>% 
    modify_header(label = " ")
  
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
  
  ## CIs for Differences (Instability vs No Instability)
  intervals(lme(Age ~ Instability_Group, random = ~ 1|Patient_ID, data = dat_sum))
  intervals(lme(Height ~ Instability_Group, random = ~ 1|Patient_ID, data = dat_sum, na.action = na.omit))
  intervals(lme(Weight ~ Instability_Group, random = ~ 1|Patient_ID, data = dat_sum, na.action = na.omit))
  
  confint(lmer(Age ~ Instability_Group + (1|Patient_ID), data = dat_sum))
  confint(lmer(Height ~ Instability_Group + (1|Patient_ID), data = dat_sum, na.action = na.omit))
  confint(lmer(Weight ~ Instability_Group + (1|Patient_ID), data = dat_sum, na.action = na.omit))
  
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
  
  # color pallette
  olympic_palette <- natparks.pals("Olympic")
  
  # o	Acetabular version 3 o’clock
  Acetabular_Version_3_mod2 <- lmer(Acetabular_Version_3 ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
  summary(Acetabular_Version_3_mod2) # Beighton trend 
  car::Anova(Acetabular_Version_3_mod2, type = "II") # Effects 
  
  ## Comparison of Legal Sex (p < 0.001)
  emmeans(Acetabular_Version_3_mod2, specs = pairwise ~ Legal_Sex)
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Acetabular_Version_3
    )
  ) + 
    geom_point(
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Acetabular Version (3 o'clock)",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Male" = olympic_palette[3],
        "Female" = olympic_palette[8],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 25,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 25-1, 
      size = 3,
      label = "Anteversion"
    )+
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  # o	Femoral Torsion
  Femoral_Torsion_mod2 <- lmer(Femoral_Torsion ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
  summary(Femoral_Torsion_mod2) # Beighton trend 
  car::Anova(Femoral_Torsion_mod2, type = "II") # Effects
  
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Femoral_Torsion
    )
  ) + 
    geom_point(
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Femoral Torsion",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Male" = olympic_palette[3],
        "Female" = olympic_palette[8],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 20,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 20+1, 
      size = 3,
      label = "Antetorsion"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  # o	Combined Version
  Combined_Version_mod2 <- lmer(Combined_Version ~ Beighton + Legal_Sex + age_group + race_group + (1|Patient_ID), data = dat_og)
  summary(Combined_Version_mod2) # Beighton trend
  car::Anova(Combined_Version_mod2, type = "II") # Effects
  
  ## Comparison of Legal Sex (p < 0.001)
  emmeans(Combined_Version_mod2, specs = pairwise ~ Legal_Sex)
  
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = Combined_Version
    )
  ) + 
    geom_point(
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "Combined Version",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Male" = olympic_palette[3],
        "Female" = olympic_palette[8],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 40,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 40+2, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 35,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 35+2, 
      size = 3,
      label = "Mild Instability"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
  
  
  # FNSA Graph
  ggplot(
    data = dat_og,
    aes(
      x = Beighton, 
      y = FNSA
    )
  ) + 
    geom_point(
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = Legal_Sex
      )
    ) + 
    geom_smooth(
      method = "lm",
      se = F,
      aes(
        color = "All Hips"
      )
    ) + 
    theme_bw(
      
    ) + 
    labs(
      x = "Beighton Score",
      y = "FNSA",
      color = NULL
    ) + 
    theme(
      legend.position = "bottom"
    ) + 
    scale_color_manual(
      values = c(
        "Male" = olympic_palette[3],
        "Female" = olympic_palette[8],
        "All Hips" = olympic_palette[1]
      )
    ) + 
    geom_hline(
      yintercept = 140,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 140+2, 
      size = 3,
      label = "Instability"
    ) + 
    geom_hline(
      yintercept = 135,
      linetype = "dashed"
    ) + 
    annotate(
      geom = "text",
      x = 1.5, 
      y = 135+2, 
      size = 3,
      label = "Mild Instability"
    ) +
    scale_x_continuous(limits = c(0, 9), breaks = 0:9)
  
}

### 5. Histograms by sex and hip instability status
{
  # 1. All Hips
  ggplot(dat_og, aes(x = factor(Beighton))) +
    geom_bar(aes(y = after_stat(prop), group = 1), fill = olympic_palette[1], color = "black") +
    scale_x_discrete(breaks = as.character(0:9), limits = as.character(0:9)) +
    scale_y_continuous(limits = c(0, 1.0), labels = scales::percent) +
    theme_bw() +
    labs(title = "All Hips", x = "Beighton Score", y = "Percent of Population")
  
  # 2. Hips with Instability
  ggplot(filter(dat_og, Instability_Group == "Hip with Instability"), aes(x = factor(Beighton))) +
    geom_bar(aes(y = after_stat(prop), group = 1), fill = olympic_palette[7], color = "black") +
    scale_x_discrete(breaks = as.character(0:9), limits = as.character(0:9)) +
    scale_y_continuous(limits = c(0, 1.0), labels = scales::percent) +
    theme_bw() +
    labs(title = "Hips with Instability", x = "Beighton Score", y = "Percent of Population")
  
  # 3. Hips without Instability
  ggplot(filter(dat_og, Instability_Group == "Hip without Instability"), aes(x = factor(Beighton))) +
    geom_bar(aes(y = after_stat(prop), group = 1), fill = olympic_palette[9], color = "black") +
    scale_x_discrete(breaks = as.character(0:9), limits = as.character(0:9)) +
    scale_y_continuous(limits = c(0, 1.0), labels = scales::percent) +
    theme_bw() +
    labs(title = "Hips without Instability", x = "Beighton Score", y = "Percent of Population")
  
  # 4. Female Hips
  ggplot(filter(dat_og, Legal_Sex == "Female"), aes(x = factor(Beighton))) +
    geom_bar(aes(y = after_stat(prop), group = 1), fill = olympic_palette[8], color = "black") +
    scale_x_discrete(breaks = as.character(0:9), limits = as.character(0:9)) +
    scale_y_continuous(limits = c(0, 1.0), labels = scales::percent) +
    theme_bw() +
    labs(title = "Females", x = "Beighton Score", y = "Percent of Population")
  
  # 5. Male Hips
  # Preprocess: calculate proportions manually
  male_data <- dat_og %>%
    filter(Legal_Sex == "Male") %>%
    count(Beighton) %>%
    complete(Beighton = 0:9, fill = list(n = 0)) %>%
    mutate(prop = n / sum(n),
           Beighton = factor(Beighton, levels = 0:9))
  
  # Plot
  ggplot(male_data, aes(x = Beighton, y = prop)) +
    geom_col(fill = olympic_palette[3], color = "black") +
    scale_y_continuous(limits = c(0, 1.0), labels = scales::percent) +
    theme_bw() +
    labs(title = "Males", x = "Beighton Score", y = "Percent of Population")
  
  
}

### 6. Tables
{
  # 1.	Hips with instability
  
  lcea_mod_sum_insta <- lmer(LCEA ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
    tbl_regression()
  
  Acetabular_Coverage_mod_sum_insta <- lmer(Acetabular_Coverage ~ Beighton + (1|Patient_ID), data = dat_og %>% filter(Instability_Group == "Hip with Instability")) %>% 
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