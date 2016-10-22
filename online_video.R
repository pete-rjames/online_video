# Online Video
# http://www.pewinternet.org/datasets/july-2013-online-video-omnibus/ 

# Libraries
libs <- c("Hmisc", "foreign", "survey", "dplyr", "tidyr", "ggplot2", "broom")
lapply(libs, library, character.only = TRUE)
rm(libs)

# Load data
# MAC: df <- read.spss("~/R/data/July-2013--Online-Video-(onmibus)/Omnibus_July_2013_Video_spss.sav", to.data.frame = TRUE)
# PC: 
df_import <- read.spss(".//July-2013--Online-Video-(onmibus)//Omnibus_July_2013_Video_spss.sav", to.data.frame = TRUE)

# Add derived variables: selected demographics 

df_base <- df_import %>%
  mutate(white = factor((ifelse(hisp != "Yes" & race == "White", "White", "Not white")))) %>%
  mutate(black = factor((ifelse(hisp != "Yes" & race == "Black or African-American", "Black or African-American", "Not black or African-American")))) %>% 
  mutate(nonwhite = factor((ifelse(hisp == "Yes" | as.numeric(race) %in% 2:6, "Nonwhite", "White")))) %>%  
  mutate(age2 = factor((ifelse(age == 99, NA, cut2(age, cuts = c(30, 50, 65)))),
                       labels = c("18-29", "30-49", "50-64", "65+"))) %>%  
  mutate(educ3 = factor((ifelse(as.numeric(educ2) < 4, 1,
                                ifelse(as.numeric(educ2) %in% 4:5, 2,
                                       ifelse(as.numeric(educ2) %in% 6:8, 3, NA)))),
                        labels = c("hs or less", "some coll", "coll grad"))) %>% 
  mutate(inc2 = factor((ifelse(as.numeric(inc) < 4, 1,
                               ifelse(as.numeric(inc) %in% 4:5, 2,
                                      ifelse(as.numeric(inc) == 6, 3,
                                             ifelse(as.numeric(inc) %in% 7:9, 4, NA))))),
                       labels = c("Under $30K", "$30K - $49.9K", "$50K - $74.9K", "$75K +")))  

summary(df_base[56:61]) 

# Add derived variables: survey questions 

df_base <- df_base %>%
  mutate(iuser = ifelse(eminuse == "Yes" | intmob == "Yes", 1, 0)) %>%
  mutate(iuser_video = ifelse(act62 == "Yes" | act102 == "Yes" | act131 == "Yes", 1, 0)) %>%
  mutate(all_video = ifelse(is.na(iuser_video), 0, iuser_video)) %>%
  mutate(iuser_edu = ifelse(iuser == 0, NA,
                            ifelse(is.na(vid01h), 0, 
                                   ifelse(vid01h == "Yes", 1, 0)))) %>%
  mutate(video_edu = ifelse(is.na(vid01h), NA, 
                            ifelse(vid01h == "Yes", 1, 0))) %>%
  mutate(all_edu = ifelse(is.na(vid01h), 0, 
                          ifelse(vid01h == "Yes", 1, 0)))  

# Add descriptions for derived variables (survey questions)

# df_ana_names <- data.frame(variable = names(df_ana))
# df_ana_names$description[df_ana_names$variable == "iuser"] <- "If online, including cell; zero NA."
# df_ana_names$description[df_ana_names$variable == "iuser_video"] <- "If video user - online only; NA if not online."
# df_ana_names$description[df_ana_names$variable == "all_video"] <- "If video user - full sample; zero NA." 
# df_ana_names$description[df_ana_names$variable == "iuser_edu"] <- "If watches educational video - online only; NA if not online"                         
# df_ana_names$description[df_ana_names$variable == "video_edu"] <- "If watches educational video - video only; NA if not online or not video user"              
# df_ana_names$description[df_ana_names$variable == "all_edu"] <- "If watches educational video - full sample; zero NA." 

# Create svydesign object 

df <- df_base %>%
  select(psraid, sample, weight, iuser, iuser_video, all_video, age, educ2, inc, sex, age2, nonwhite, educ3, inc2)
df_names <- data.frame(names(df))

des <- svydesign(id = ~ 1, strata = ~ sample, weights = ~ weight, data = df) 

# Univariate analysis of all_video variable

iuser_mean <- svymean(~ iuser, design = des)
all_video_mean <- svymean(~ all_video, design = des)
str(iuser_mean)
mean(iuser_mean)

df_uni <- data.frame(variable = c("online", "online video"),
                     mean = c(mean(iuser_mean), mean(all_video_mean)),
                     se = c(SE(iuser_mean), SE(all_video_mean)))

df_uni <- df_uni %>%
  mutate(lower = mean - (1.96*se)) %>%
  mutate(upper = mean + (1.96*se))

ggplot(df_uni, aes(x = variable, y = mean, fill = variable)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(limits, position = "dodge", color = "dark grey", size = 1, width = 0.5) +
  ylim(0, 1) +
  ylab("Proportion of US population") +
  xlab("") +
  ggtitle("Plot 1: Estimated proportion of US population online and watching online video, 2013")


# Associations among predictors

tab_func <- function(x,y) {  
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

tab_func(8,9)

var_comb <- data.frame(t(combn(8:12,2))) 
tab_list <- mapply(tab_func, var_comb[,1], var_comb[,2], SIMPLIFY = FALSE)
df_assoc <- do.call(rbind.data.frame, tab_list)

tab_plot <- function(x,y) {
  ggplot(df_assoc[df_assoc$variable_x ==(x) & df_assoc$variable_y == (y),],
     aes(x = level_x, y = level_y, label = round(proportion, 2), color = level_x)) +
  geom_count(aes(size = proportion^2)) +
  geom_text(nudge_x = 0.1, color = "black") +
  ylab("") +
  xlab("") +
  guides(size = "none", variable_x = "none") +
  ggtitle(paste("Row percentages for", x, "by", y))
}

tab_plot("sex", "age2")
tab_plot("nonwhite", "inc2")

# Descriptives 

svyby_dyn <- function(x) {
  variable_name <- names(des$variables[(x)])
  temp <- svyby(~ all_video,
        as.formula(paste("~", variable_name)), 
        design = des, FUN = svymean)  
  colnames(temp)[1:2] <- c("variable_level", "proportion")
  temp %>%
    mutate(upper = proportion+(1.96*se)) %>%
    mutate(lower = proportion-(1.96*se)) %>%
    mutate(variable_name = variable_name)
}

df_descr_list <- lapply(8:12, svyby_dyn)
df_descr <- do.call(rbind.data.frame, df_descr_list)  

# Overview of bivariate cross-tabulations
# Consider whether to use weights or simple counts 
ggplot(df_descr, aes(x = variable_level, y = proportion, color = variable_name)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 2) + 
  ylim(0,1) +
  xlab("") +
  ggtitle("Estimated proportion consuming online video, by demographic")

# For specific bivariate cross-tabulations (if neccessary)

ggplot(df_descr[df_descr$variable_name == "inc2",],
       aes(x = variable_level, y = proportion, color = variable_level)) +
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 2) + 
  ylim(0,1) 

# Logistic regression model: 2 (using more aggregated variables)

# Hypotheses 
# Age buggest impact, then educ, then inc, then nonwhite, then gender

# Set-up

set.seed(0) 

df_train <- df %>%
  mutate(split = rnorm(nrow(df))) %>%
  filter(split > 0)

df_test <- anti_join(df, df_train, by = "psraid")
df_test <- df_test %>% 
  filter(complete.cases(sex, nonwhite, age2, educ3, inc2))

# find a way to deal with NAs in logistic model 


df_train <- df_train[1:9]

des_train <- svydesign(id = ~ 1, strata = ~ sample, weights = ~ weight, data = df_train) 

# model

fit <- svyglm(all_video ~ sex + nonwhite + age2 + educ3 + inc2, 
              design = des,
              family = quasibinomial)


round3 <- function(x) { round(x, 3) }

tidy_fit <- fit %>%
  tidy() %>%
  mutate(lower = estimate - 1.96*(std.error)) %>%
  mutate(upper = estimate + 1.96*(std.error)) 

# visualize
# add in confidence intervals

ggplot(tidy_fit, aes(x = term, y = estimate)) +
  geom_point() +
  ylim(-4, 4)

# refit to only include significant predictors

fit2 <- svyglm(all_video ~ age2 + educ3 + inc2, 
              design = des,
              family = quasibinomial)

# residuals analysis 
# TBC

# confirmatory  

df_test$mean_prob <- svymean(~ all_video, design = des_train)
df_test$mean_outcome <-  ifelse(df_test$mean_prob > 0.5, 1, 0)
df_test$mean_error <- ifelse(df_test$mean_outcome != df_test$all_video, 1, 0)

df_test$mod_prob <- predict(fit2, newdata = df_test, type = "response")
df_test$mod_outcome <- ifelse(df_test$mod_prob > 0.5, 1, 0)
df_test$mod_error <- ifelse(df_test$mod_outcome != df_test$all_video, 1, 0)

mean(df_test$all_video) 
mean(df_test$mod_outcome) # a ~ 3% difference between actual and modelled mean

mean(df_test$mean_error)
mean(df_test$mod_error) # error rate of 20 %; half error rate of using mean from train

# the error includes false positives and false negatives
# so it's different from the discrepancy in means

# probabilities table






