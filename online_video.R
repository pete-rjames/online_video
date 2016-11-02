# Online Video
# http://www.pewinternet.org/datasets/july-2013-online-video-omnibus/ 

# 1. Libraries
libs <- c("broom", "dplyr","foreign", "ggplot2", "knitr", "Hmisc", "survey")
lapply(libs, library, character.only = TRUE)
rm(libs)

# 2. Load data >> df_import

# mac: df_import <- read.spss("~/R/data/July-2013--Online-Video-(onmibus)/Omnibus_July_2013_Video_spss.sav", to.data.frame = TRUE)
# pc: 
df_import <- read.spss(".//July-2013--Online-Video-(onmibus)//Omnibus_July_2013_Video_spss.sav", to.data.frame = TRUE) # ignore warnings; 
df_import <- df_import %>% mutate(age = ifelse(age == 99, NA, age)) # convert 99 to NA for age

# 3. Add derived degmoraphic variables >> df_base

# 3.1 Lookup table for income midpoints

inc_midpoints <- data.frame(inc = as.factor(levels(df_import$inc)),
                            inc_midpoint = c(5000, 15000, 25000, 35000,45000,
                                             62500, 87500,125000, 150000,NA))

inc_midpoints$income_10k <- inc_midpoints$inc_midpoint/10000

# 3.2 Adding derived variables (see Omnibus_July_2013_Video_Crosstab.pdf for totals)

df_base <- df_import %>%
  data.frame() %>%
  mutate(white = factor((ifelse(hisp != "Yes" & race == "White", "White", "Not white")))) %>%
  mutate(black = factor((ifelse(hisp != "Yes" & race == "Black or African-American", "Black or African-American", "Not black or African-American")))) %>% 
  mutate(nonwhite = factor((ifelse(hisp == "Yes" | as.numeric(race) %in% 2:6, "Nonwhite", "White")))) %>%  
  mutate(race_group = relevel(nonwhite, "White")) %>%
  mutate(age_10yrs = age/10) %>%
  mutate(age_group = factor((ifelse(is.na(age), NA, cut2(age, cuts = c(30, 50, 65)))),
                       labels = c("18-29", "30-49", "50-64", "65+"))) %>% 
  mutate(education = factor((ifelse(as.numeric(educ2) < 4, 1,
                                ifelse(as.numeric(educ2) %in% 4:5, 2,
                                       ifelse(as.numeric(educ2) %in% 6:8, 3, NA)))),
                        labels = c("Hs or less", "Some coll", "Coll grad"))) %>% 
  mutate(income = factor((ifelse(as.numeric(inc) < 4, 1,
                               ifelse(as.numeric(inc) %in% 4:5, 2,
                                      ifelse(as.numeric(inc) == 6, 3,
                                             ifelse(as.numeric(inc) %in% 7:9, 4, NA))))),
                       labels = c("<$30K", "$30-49K", "$50-74K", "$75K+"))) %>%
  left_join(inc_midpoints[,c(1,3)], by = "inc") %>%
  mutate(iuser = ifelse(eminuse == "Yes" | intmob == "Yes", 1, 0)) %>%
  mutate(iuser_video = ifelse(act62 == "Yes" | act102 == "Yes" | act131 == "Yes", 1, 0)) %>%
  mutate(all_video = ifelse(is.na(iuser_video), 0, iuser_video))

# 4. Create svydesign object 

df <- df_base %>%
  select(psraid, sample, weight, iuser, iuser_video, all_video, sex, age, age_10yrs, age_group, race, race_group, inc, income, income_10k, educ2, education)

df_names <- data.frame(names(df))

des <- svydesign(id = ~ 1, strata = ~ sample, weights = ~ weight, data = df) 

rm(df_import, df_base, inc_midpoints)

# 5. Exploratory analysis 

# 5.1 Univariate analysis 

univar_dyn <- function(x) {
  variable_name <- names(des$variables[(x)])
  temp <- data.frame(round(prop.table(svytable(as.formula(paste("~", variable_name)), 
                design = des)),2))
  colnames(temp)[1:2] <- c("variable_level", "proportion")
  temp$variable_name = variable_name
  return(temp)
}

univar_list <- lapply(c(4:7, 10, 12, 14, 17), univar_dyn)
df_univar <- do.call(rbind.data.frame, univar_list) 
df_univar <- df_univar %>%
  select(variable_name, variable_level, proportion) 
rm(univar_dyn, univar_list)

# 5.2 Bivariate analysis

# 5.2.1 Summary of online and watching online video

iuser_mean <- svymean(~ iuser, design = des)
all_video_mean <- svymean(~ all_video, design = des)

df_summary <- data.frame(variable = c("online", "online video"),
                     mean = c(mean(iuser_mean), mean(all_video_mean)),
                     se = c(SE(iuser_mean), SE(all_video_mean)))

rm(iuser_mean, all_video_mean)

df_summary <- df_summary %>%
  mutate(lower = mean - (1.96*se)) %>%
  mutate(upper = mean + (1.96*se)) 

ggplot(df_summary, aes(x = variable, y = mean, fill = variable, label = round(mean,2))) +
  geom_bar(stat = "identity", width = 0.25) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = "dodge", color = "dark grey", size = 1, width = 0.25) +
  geom_text(nudge_y = -0.1) +
  ylim(0, 1) +
  xlab("") +
  ylab("proportion") +
  ggtitle("Proportion of US population online and watching online video, 2013")

# 5.2.2 Bivariate: internet usage and demographic predictors

iuser_dyn <- function(x) {
  variable_name <- names(des$variables[(x)])
  temp <- svyby(~ iuser,
                as.formula(paste("~", variable_name)), 
                design = des, FUN = svymean)  
  colnames(temp)[1:2] <- c("variable_level", "proportion")
  temp %>%
    mutate(upper = proportion+(1.96*se)) %>%
    mutate(lower = proportion-(1.96*se)) %>%
    mutate(variable_name = variable_name)
}

iuser_list <- lapply(c(7, 10, 12, 14, 17), iuser_dyn) 
df_iuser <- do.call(rbind.data.frame, iuser_list)  

rm(iuser_dyn, iuser_list) 

iuser_plot <- ggplot(df_iuser, aes(x = variable_level, y = proportion, color = variable_name)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 2) + 
  ylim(0,1) +
  xlab("") +
  ggtitle("Proportion of US population that is online, by demographic, 2013")

iuser_plot

# 5.2.3 Bivariate: online video and demographic predictors 

nrow(df[df$iuser == "0",]) == sum(is.na(df$iuser_video)) # defining NAs: non-online

iuser_video_dyn <- function(x) {
  variable_name <- names(des$variables[(x)])
  temp <- svyby(~ iuser_video,
                as.formula(paste("~", variable_name)), 
                design = des, FUN = svymean, na.rm = TRUE)  
  colnames(temp)[1:2] <- c("variable_level", "proportion")
  temp %>%
    mutate(upper = proportion+(1.96*se)) %>%
    mutate(lower = proportion-(1.96*se)) %>%
    mutate(variable_name = variable_name)
}

iuser_video_list <- lapply(c(7, 10, 12, 14, 17), iuser_video_dyn)
df_iuser_video <- do.call(rbind.data.frame, iuser_video_list) 

iuser_video_plot <- ggplot(df_iuser_video, aes(x = variable_level, y = proportion, color = variable_name)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 2) + 
  ylim(0,1) +
  xlab("") +
  ggtitle("Proportion of US online population that watches video, by demographic, 2013")

iuser_video_plot

rm(iuser_video_dyn, iuser_video_list)

# Overlaying plots: online (grey) and watches online(color)

overlay_plot <- iuser_video_plot +
  geom_point(data = df_iuser, size = 1, color = "dark grey") +
  geom_pointrange(data = df_iuser, aes(ymin = lower, ymax = upper), size = 1, color = "dark grey") + 
  ggtitle("Proportion of US population that is online (grey) and proportion of online population that watches online video (color), by demographic, 2013")

overlay_plot

# 5.2.4 Associations among predictors

bivar_dyn <- function(x,y) {  
  variable_x <- names(des$variables[(x)]) 
  variable_y <- names(des$variables[(y)])  
  table <- svytable(as.formula(paste("~", variable_x, "+", variable_y)),
                    design = des)
  table <- data.frame(prop.table(table, 1))
  table$variable_x <- names(table)[1]
  table$variable_y <- names(table)[2]
  colnames(table)[1:3] <- c("level_x", "level_y", "proportion")
  table <- table %>% select(variable_x, variable_y, level_x, level_y, proportion)
  return(table)
}  

var_comb <- data.frame(expand.grid(c(7, 10, 12, 14, 17), c(7, 10, 12, 14, 17)))  # all permuations 
var_comb <- var_comb %>% filter(Var1 != Var2)
tab_list <- mapply(bivar_dyn, var_comb[,1], var_comb[,2], SIMPLIFY = FALSE)
df_assoc <- do.call(rbind.data.frame, tab_list)

tab_plot <- function(x,y) { 
  ggplot(df_assoc[df_assoc$variable_x ==(x) & df_assoc$variable_y == (y),],
         aes(x = level_x, y = level_y, label = round(proportion, 2), color = level_x)) +
    geom_count(aes(size = proportion^2)) +
    geom_text(nudge_x = 0.1, color = "black") +
    ylab("") +
    xlab("") +
    guides(size = "none", variable_x = "none") +
    ggtitle(paste0(x,": ", "row percentages by ", y)) 
} 

tab_plot("age_group", "income") # example plot: run locally to explore different combinations
tab_plot("age_group", "education") # example plot: run locally to explore different combinations

rm(list = setdiff(ls(), c("df", "df_names", "des")))

# 6. Build logistic regression model 

# 6.1 Single predictor models

uni_mod <- function(x) {
  predictor <- names(des$variables[(x)])
  fit <- svyglm(as.formula(paste("all_video ~", predictor)),
                design = des, family = quasibinomial)
  fit <- tidy(fit) %>%
    filter(term != "(Intercept)")
  return(fit)
} 

uni_fit_list <- lapply(c(7,9,12,15,17), uni_mod)
df_uni_fit <- do.call(rbind.data.frame, uni_fit_list)

df_uni_fit <- df_uni_fit %>%
  rowwise() %>%
  mutate(term = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", term)) %>%
  mutate_each(funs(round(.,3)), - term)  

rm(uni_mod, uni_fit_list) 

# 6.2 Multivariate model

# 6.2.1 Partitioning dataset 

set.seed(1000) 

df_mod <- df %>%
  select(psraid, weight, sample, all_video, age_10yrs, race_group, education, income_10k) %>%
  filter(complete.cases(psraid, weight, sample, all_video, age_10yrs, race_group, education, income_10k))

df_train <- df_mod %>%
  mutate(split = rbinom(n = n(), size = 1, prob = (2/3))) %>%
  filter(split == 1)    

df_test <- anti_join(df_mod, df_train, by = "psraid") 

des_train <- svydesign(id = ~ 1, strata = ~ sample, weights = ~ weight, data = df_train) 

# 6.2.2 Model 1

fit_1 <- svyglm(all_video ~ age_10yrs + race_group + education + income_10k,
               design = des_train,
               family = quasibinomial)

df_fit_1 <- fit_1 %>%
  tidy() %>%
  mutate(lower = estimate - 1.96*(std.error)) %>%
  mutate(upper = estimate + 1.96*(std.error)) %>%
  mutate_each(funs(round(.,3)), - term)  


# 6.2.3 Model 2

fit_2 <- svyglm(all_video ~ age_10yrs + education + income_10k,
              design = des_train,
              family = quasibinomial)

df_fit_2 <- fit_2 %>%
  tidy() %>%
  mutate(lower = estimate - 1.96*(std.error)) %>%
  mutate(upper = estimate + 1.96*(std.error)) %>%
  mutate_each(funs(round(.,3)), - term) 


fit_table <- left_join(df_fit_1[,c(1,2,5)], df_fit_2[,c(1,2,5)], by = "term") %>%
  mutate(term = gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", term))
  colnames(fit_table)[2:5] <- c("estimate (m1)", "p.value (m1)", 
                              "estimate (m2)", "p.value (m2)")  

  
# 6.2.4 Testing multivariate model (Fit_2)

df_test$mean_prob <- svymean(~ all_video, design = des_train, na.rm = TRUE)
df_test$mean_outcome <-  ifelse(df_test$mean_prob > 0.5, 1, 0)
df_test$mean_error <- ifelse(df_test$mean_outcome != df_test$all_video, 1, 0)

df_test$mod_prob <- predict(fit_2, newdata = df_test, type = "response")
df_test$mod_outcome <- ifelse(df_test$mod_prob > 0.5, 1, 0)
df_test$mod_error <- ifelse(df_test$mod_outcome != df_test$all_video, 1, 0)

null_error <- round(mean(na.omit(df_test$mean_error)),2)
model_error <- round(mean(na.omit(df_test$mod_error)),2)

# 7. Using the model 

age_inc_sim <- function(x) { 
  temp <- data.frame(age_10yrs = seq.int(from = 1.8, to = 8.0, by = 0.1),   
                            education = "Hs or less",
                            income_10k = svyquantile(~ income_10k, design = des, quantiles = (x), na.rm = TRUE)[1],
                            decile = as.factor(x))
  return(temp)
}  

quantiles <- seq(from = 0.1, to = 0.5, by = 0.1)
age_sim <- lapply(quantiles, age_inc_sim)
df_age <- do.call(rbind.data.frame, age_sim)
df_age$prob <- predict.glm(fit_2, newdata = df_age, type = "response") 

ggplot(df_age, aes(x = (age_10yrs*10), y = prob, color = decile)) +
  geom_freqpoly(stat = "identity", size = 1) +
  ylim(0,1) +
  ylab("Probability") + 
  xlab("Age") +
  geom_hline(yintercept = 0.5, linetype = 2, color = "dark grey") + 
  ggtitle("Probability of watching online video as predicted by age for 5 income deciles")


