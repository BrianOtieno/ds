# JIKINGE: COVID-19 and economic insecurity - UPenn
# Otieno B.G - MBCS CITP
# Kavanagh, N.M. -  MPH
# September 28, 2020

# Direct questions about this file to gebryo@intelligencia.com

library(readstata13) # Import dataset
library(ggplot2)     # Graphing tools
library(dplyr)       # Analysis tools
library(psych)       # Analysis tools
library(reshape2)    # Analysis tools
library(table1)      # Table tools
library(cowplot)     # Graphing tools

##############################################################################
# Dataset preparation
##############################################################################

# Set working directory (for exporting figures)
setwd("/C/Jikinge/COVID impacts on high-risk women/")

# Read datasets into R
# jikinge <- read.csv("/C/Jikinge/COVID impacts on high-risk women/IRDO _ JIKINGE COVID_19 SURVEY 7.6.20_For Nolan, PHI removed.csv")
jikinge <- read.csv("Jikinge 2020 COVID survey_Clean.csv")

# Total number of enrolled participants
nrow(jikinge)

# Remove unnecessary columns
jikinge <- select(jikinge, -c(note_logo, note_introduction))

# Rename variables
jikinge <- jikinge %>% 
  rename(
    # Identification
    interviewer_id         = identificationinterviewer_id,
    interviewer_date       = identificationinterviewer_date,                                  
    interview_time         = identificationinterview_time,
    participant_id         = identificationparticipant_id,
    cluster_id             = identificationcluster_id,
    cluster_name           = identificationcluster_name,
    hotspot                = identificationhotspot,
    
    # Demographics and socioeconomics
    age                    = demographicandsocioeconomicage,
    living_alone           = lives_alone,
    living_with_parents    = liveswithparents,
    living_with_spouse     = liveswithspouse,
    living_with_children   = liveswithchildren,
    living_with_sib_or_rel = liveswithsiblingorrelatives,
    living_with_friends    = liveswithfriends,
    current_residence      = demographicandsocioeconomiccurr,
    housing_condition      = demographicandsocioeconomichous,
    
    # Life and pandemic
    num_face_talked        = sectionblifeandpandemicnumberfa,
    last_physical_contact  = sectionblifeandpandemiclastphys,
    diff_obtain_food       = sectionblifeandpandemicdifficul,
    worried_about_food     = sectionblifeandpandemicworrieda,
    substandard_food       = sectionblifeandpandemicsubstand,
    slept_hungry           = sectionblifeandpandemicslepthun,
    violence               = sectionblifeandpandemicviolence,
    
    # Outcomes
    primary_occupation     = sectioncoutcomesprimary_occupat,
    specify_pri_occupation = sectioncoutcomesspecify_primary,
    working_conditions     = sectioncoutcomesworkingconditio,
    weekly_hours_worked    = sectioncoutcomesweeklyworkingho,
    actual_hours_worked    = sectioncoutcomesactualhourswork,
    money_earned_past_week = sectioncoutcomesmoneyearnedinpa,
    earning_vs_typical     = sectioncoutcomescompareearningt,
    economic_status_in_6m  = sectioncoutcomeseconomicstatusi,
    work_from_home_today   = sectioncoutcomesworkingfromhome,
    
    # Prevention measures
    gathering_with_5_plus  = sectiondpreventionmeasuresgathe,
    attended_social_event  = attendedsocialevent,
    attended_religus_gath  = attendedreligiousgathering,
    attended_wedding       = attendedweddinginpastfortnight,
    attended_funeral       = attendedfuneralinpastfortnight,
    in_public_transport    = inpublictransportinpastweek,
    went_beauty_parlor     = wenttobeautyparlorinpastweek,
    went_market            = wenttomarketinpastweek,
    worry_hlth_loved_ones  = sectiondpreventionmeasuresworry,
    
    # Personal health
    health_rating          = sectionepersonalhealthhealthrat,
    health_problems        = sectionepersonalhealthhadhealth,
    household_hlth_prblms  = sectionepersonalhealthanyhouseh,
    worry_about_health     = sectionepersonalhealthworriedab,
    clinic_past_week       = beentoclinicinpastweek,
    clinic_dice_past_month = beentoclinicordiceinpastmonth,
    diff_obtain_medicine   = sectionepersonalhealthhaddiffic,
    diff_meds_next_month   = sectionepersonalhealthdifficult,
    num_sex_partners       = sectionepersonalhealthsexualpar,
    num_paid_for_sex       = sectionepersonalhealthnumberpai,
    covid_biggest_challnge = sectionepersonalhealthcovidshut,
    months_to_recover      = sectionepersonalhealthmonthstor,
    
    # End
    interview_end_time     = sectionepersonalhealthinterview,
    instance_ID            = metainstanceID,
    KEY                    = KEY
  )

# Remove non-user responses
jikinge <- subset(jikinge, intervention_arm %in% c(0,1))

# Remove outliers/errors
jikinge <- jikinge %>% mutate(
  weekly_hours_worked = case_when( # Hours worked
    weekly_hours_worked > 97 ~ NA_real_,
    TRUE ~ as.numeric(weekly_hours_worked))
)
jikinge <- jikinge %>% mutate(
  actual_hours_worked = case_when( # Hours worked
    actual_hours_worked > 97 ~ NA_real_,
    TRUE ~ as.numeric(actual_hours_worked))
)

# Reformat dates
jikinge$interviewer_date <- as.Date(as.character(jikinge$interviewer_date), format="%d-%b-%y")

# Convert shillings to USD
jikinge$money_earned_past_week_us <- jikinge$money_earned_past_week*0.0094
# Exchange rate: 0.0094 KES:USD for most of May 13 to June 29, 2020

# Calculate change in hours
jikinge$change_in_hours <- jikinge$actual_hours_worked - jikinge$weekly_hours_worked

# Determine if working hours declined
jikinge <- jikinge %>% mutate(
  hours_declined = case_when(
    actual_hours_worked <  weekly_hours_worked ~ "Yes",
    actual_hours_worked >= weekly_hours_worked ~ "No",
    TRUE                                       ~ NA_character_)
)
jikinge$hours_declined <- factor(jikinge$hours_declined, levels = c("Yes", "No"))

# Calculate transactional sex
jikinge <- jikinge %>% mutate(
  transact_sex_cov = case_when(
    num_sex_partners == 0 ~ 0,
    num_sex_partners != 0 ~ as.numeric(num_paid_for_sex))
)

##############################################################################
# Assign labels
##############################################################################

# Food insecurity variables
jikinge <- jikinge %>%
  mutate_at(.vars = vars(worried_about_food, substandard_food, slept_hungry),
            .funs = funs(. = case_when(
              . == 1 ~ "Never",
              . == 2 ~ "Rarely (1-2 times)",
              . == 3 ~ "Sometimes (3-10 times)",
              . == 4 ~ "Often (10+ times)",
              TRUE ~ NA_character_
            )))
jikinge$worried_about_food_. <- factor(jikinge$worried_about_food_.,levels = c("Never", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (10+ times)"))
jikinge$substandard_food_. <- factor(jikinge$substandard_food_., levels = c("Never", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (10+ times)"))
jikinge$slept_hungry_. <- factor(jikinge$slept_hungry_., levels = c("Never", "Rarely (1-2 times)", "Sometimes (3-10 times)", "Often (10+ times)"))

# Difficulty obtaining food
jikinge <- jikinge %>% mutate(
  diff_obtain_food = case_when(
    diff_obtain_food == 1 ~ "Yes",
    diff_obtain_food == 2 ~ "No",
    TRUE ~ NA_character_)
)
jikinge$diff_obtain_food <- factor(jikinge$diff_obtain_food, levels = c("Yes", "No"))

# Earnings compared to typical week
jikinge <- jikinge %>% mutate(
  earning_vs_typical = case_when(
    earning_vs_typical == 1 ~ "Higher than usual",
    earning_vs_typical == 2 ~ "Lower than usual",
    earning_vs_typical == 3 ~ "Similar",
    TRUE ~ NA_character_)
)
jikinge$earning_vs_typical <- factor(jikinge$earning_vs_typical, levels = c("Lower than usual", "Similar", "Higher than usual"))

# Economic status in 6 months
jikinge <- jikinge %>% mutate(
  economic_status_in_6m = case_when(
    economic_status_in_6m == 1 ~ "Much\nbetter off",
    economic_status_in_6m == 2 ~ "Somewhat\nbetter off",
    economic_status_in_6m == 3 ~ "Same",
    economic_status_in_6m == 4 ~ "Somewhat\nworse off",
    economic_status_in_6m == 5 ~ "Much\nworse off",
    TRUE ~ NA_character_)
)
jikinge$economic_status_in_6m <- factor(jikinge$economic_status_in_6m, levels = c("Much\nbetter off", "Somewhat\nbetter off", "Same", "Somewhat\nworse off", "Much\nworse off"))

# Self-reported health
jikinge <- jikinge %>% mutate(
  health_rating = case_when(
    health_rating == 1 ~ "Very good",
    health_rating == 2 ~ "Good",
    health_rating == 3 ~ "Fair",
    health_rating == 4 ~ "Poor",
    TRUE ~ NA_character_)
)
jikinge$health_rating <- factor(jikinge$health_rating, levels = c("Very good", "Good", "Fair", "Poor"))

# Worried about health
jikinge <- jikinge %>%
  mutate_at(.vars = vars(worry_about_health, worry_hlth_loved_ones),
            .funs = funs(. = case_when(
              . == 1 ~ "Not at all worried",
              . == 2 ~ "Slightly worried",
              . == 3 ~ "Moderately worried",
              . %in% c(4,5) ~ "Extremely worried",
              TRUE ~ NA_character_
            )))
jikinge$worry_about_health_. <- factor(jikinge$worry_about_health_., levels = c("Not at all worried", "Slightly worried", "Moderately worried", "Extremely worried"))
jikinge$worry_hlth_loved_ones_. <- factor(jikinge$worry_hlth_loved_ones_., levels = c("Not at all worried", "Slightly worried", "Moderately worried", "Extremely worried"))

# Missed clinic/DICE appointment
jikinge <- jikinge %>% mutate(
  clinic_dice_past_month = case_when(
    clinic_dice_past_month == 1 ~ "Yes",
    clinic_dice_past_month == 2 ~ "No",
    TRUE ~ NA_character_)
)
jikinge$clinic_dice_past_month <- factor(jikinge$clinic_dice_past_month, levels = c("Yes", "No"))

# Prevention behaviors
jikinge <- jikinge %>%
  mutate_at(.vars = vars(gathering_with_5_plus, attended_social_event, attended_religus_gath, attended_wedding, attended_funeral, in_public_transport, went_beauty_parlor, went_market),
            .funs = funs(. = case_when(
              . == 1 ~ "Yes",
              . == 2 ~ "No",
              TRUE ~ NA_character_
            )))

##############################################################################
# Merge with baseline/follow-up data
##############################################################################

# Read baseline data into R
baseline <- read.dta13("Baseline_enrolled_WithdrawExcluded.dta", nonint.factors = T)

# Select relevant variables
baseline <- baseline %>% select(pid, arm, date, age, dem02, dem03, dem05, other_sex, dem07, gsa03, trx05)

# Read follow-up data into R
followup <- read.dta13("FollowUp_Clean_RM.dta", nonint.factors = T)

# Select relevant variables
followup <- followup %>% select(pid, date, dem03, dem07, gsa03, trx03_6, trx05)

# Select latest date
last_fup <- followup %>%
  group_by(pid) %>%
  slice(which.max(as.Date(date, '%Y-%m-%d')))

# Treat participant ID as numeric
jikinge$participant_id <- as.numeric(as.character(jikinge$participant_id))

# Merge datasets
merged <- full_join(jikinge, baseline, by = c("participant_id" = "pid"))
merged <- full_join(merged,  last_fup, by = c("participant_id" = "pid"))

# Eliminate participants w/o COVID response
merged <- subset(merged, !is.na(interviewer_date))

# Convert shillings to USD
merged$weekly_income <- (as.numeric(merged$dem07.y)*0.0094)/4
# Exchange rate: 0.0094 KES:USD for most of May 13 to June 29, 2020

# Calculate transactional sex partners
merged <- merged %>% mutate(
  transact_sex_fu = case_when(
    trx03_6 == "No"  ~ 0,
    trx03_6 == "Yes" ~ as.numeric(trx05.y))
)

# Remove outliers
merged <- merged %>% mutate(
  weekly_income_out = case_when( # Weekly income
    weekly_income > 500 ~ NA_real_,
    TRUE ~ weekly_income)
)
merged <- merged %>% mutate(
  sex_partners_fu = case_when( # Sexual partners
    as.numeric(gsa03.y) > 100 ~ NA_real_,
    TRUE ~ as.numeric(gsa03.y))
)
merged <- merged %>% mutate(
  transact_sex_fu = case_when( # Transactional sex
    transact_sex_fu > 20 ~ NA_real_,
    TRUE ~ transact_sex_fu)
)

# Calculate changes in...
# Weekly income
merged$change_in_income <- merged$money_earned_past_week_us - merged$weekly_income_out
# Sexual partners
merged$change_in_sex <- merged$num_sex_partners - merged$sex_partners_fu
# Transactional sex
merged$change_in_transact <- merged$transact_sex_fu - merged$transact_sex_cov

# Determine if decline in...
merged <- merged %>% mutate(
  income_declined = case_when( # Weekly income
    money_earned_past_week_us <  weekly_income_out ~ "Yes",
    money_earned_past_week_us >= weekly_income_out ~ "No",
    TRUE ~ NA_character_)
)
merged$income_declined <- factor(merged$income_declined, levels = c("Yes", "No"))

merged <- merged %>% mutate(
  partner_declined = case_when( # Sexual partners
    num_sex_partners <  sex_partners_fu ~ "Yes",
    num_sex_partners >= sex_partners_fu ~ "No",
    TRUE ~ NA_character_)
)
merged$partner_declined <- factor(merged$partner_declined, levels = c("Yes", "No"))

merged <- merged %>% mutate(
  sex_work_declined = case_when( # Transactional sex
    transact_sex_cov <  transact_sex_fu ~ "Yes",
    transact_sex_cov >= transact_sex_fu ~ "No",
    TRUE ~ NA_character_)
)
merged$sex_work_declined <- factor(merged$sex_work_declined, levels = c("Yes", "No"))

# Dichotomize education
merged <- merged %>% mutate(
  edu_cat = case_when(
    is.na(dem02) ~ NA_character_,
    dem02 %in% c("None", "Some Primary", "Primary") ~ "Primary or less",
    TRUE ~ "Some secondary or more")
)
merged$edu_cat <- factor(merged$edu_cat, levels = c("Some secondary or more", "Primary or less"))

# Dichotomize sex workers
merged <- merged %>% mutate(
  sex_worker = case_when(
    dem05 == "Sex Work" ~ "Sex work income",
    other_sex == "Yes"  ~ "Sex work income",
    dem05 != "Sex Work" & other_sex == "No" ~ "No sex work income")
)
merged$sex_worker <- factor(merged$sex_worker, levels = c("No sex work income", "Sex work income"))

# Re-categorize martial status
merged <- merged %>% mutate(
  marital_cat = case_when(
    dem03.x %in% c("Married,Cohab", "Married,Not Cohab", "Not Married, Cohab", "Casual, Not Cohab") ~ "Married/cohabitating",
    dem03.x %in% c("Divorced", "Widowed") ~ "Divorced/widowed",
    dem03.x %in% c("Single") ~ "Single")
)

# Re-categorize education
merged <- merged %>% mutate(
  edu_cat_5 = case_when(
    dem02 %in% c("None", "Some Primary") ~ "Some primary or less",
    dem02 %in% c("Primary") ~ "Primary",
    dem02 %in% c("Some Second") ~ "Some secondary",
    dem02 %in% c("Second/High") ~ "Secondary",
    dem02 %in% c("Post-Second/Training", "University") ~ "Post-secondary")
)

##############################################################################
# Tables
##############################################################################

# COVID-19 interview dates
table(jikinge$interviewer_date)

# Demographics table
table1(~ age.x + edu_cat_5 + marital_cat + dem05 + other_sex, data=merged, overall=T, droplevels=T)

# Prevention behaviors table
table1(~ gathering_with_5_plus_. + attended_social_event_. + attended_religus_gath_. + attended_wedding_. + attended_funeral_. + in_public_transport_. + went_beauty_parlor_. + went_market_., data=merged, overall=T, droplevels=F)

# Food insecurity and health table
table1(~ diff_obtain_food + worried_about_food_. + substandard_food_. + slept_hungry_. + health_rating + worry_about_health_. + worry_hlth_loved_ones_., data=merged, overall=T, droplevels=F)

# Appendix table
table1(~ health_problems + clinic_past_week + clinic_dice_past_month, data=merged, overall=T, droplevels=F)

##############################################################################
# Graph of hours worked
##############################################################################

# One-sample t-test
t.test(jikinge$change_in_hours, mu=0, alternative="two.sided")

# Check hours worked vs. declining income
prop.table(table(jikinge$hours_declined, jikinge$earning_vs_typical))

# Melt relevant variables
hours_df <- melt(jikinge, id.vars = c("participant_id", "hours_declined"),
                 measure.vars = c("weekly_hours_worked", "actual_hours_worked"))

# Rename variables
hours_df <- hours_df %>% mutate(
  variable = case_when(
    variable == "actual_hours_worked" ~ "During COVID-19",
    variable == "weekly_hours_worked" ~ "Before COVID-19",
    TRUE                              ~ NA_character_)
)

# Complete pre- and post
hours_df <- hours_df %>% filter_at(vars("hours_declined"), all_vars(!is.na(.)))

# Summarize hours
means_df <- hours_df %>%
  group_by(variable) %>%
  summarise(
    mean   = mean(value,   na.rm=T),
    median = median(value, na.rm=T)
  )

# Plot
hours_plot <- ggplot(hours_df, aes(x=value, fill=variable)) +
  geom_vline(data=means_df, aes(xintercept=mean, color=variable), linetype="solid") +
  geom_vline(data=means_df, aes(xintercept=median, color=variable), linetype="dashed") +
  geom_density(alpha=0.5) +
  annotate("text", x=22, y=0.035, label=paste0("Mean    = ", round(means_df[2,"mean"], digit=1), "\nMedian = ", means_df[2,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=39, y=0.025, label=paste0("Mean    = ", round(means_df[1,"mean"], digit=1), "\nMedian = ", means_df[1,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  theme_test() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.title.x  = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Hours Worked Per Week") +
  ylab("Density of Respondents") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous()

# Print figure
ggsave(plot=hours_plot, file="Hours working.pdf", width=4, height=4, units='in', dpi=600)

##############################################################################
# Graphs of income
##############################################################################

# Summarize income
income_sum_df <- jikinge %>%
  summarise(
    mean_income   = mean(money_earned_past_week_us,   na.rm=T),
    median_income = median(money_earned_past_week_us, na.rm=T),
    sd_income     = sd(money_earned_past_week_us,     na.rm=T),
  )

# Plot
income_plot <- ggplot(jikinge, aes(x=money_earned_past_week_us)) + 
  geom_histogram(binwidth = 5, color="black", fill="#00BFC4") +
  geom_vline(data=income_sum_df, aes(xintercept=mean_income), linetype="solid") +
  annotate("text", x=15, y=625, label="Mean\nMedian\nSD", fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=45, y=625,
           label=paste0("=   $", round(income_sum_df$mean_income, 2),
                        "\n=   $", round(income_sum_df$median_income, 2),
                        "\n= $", format(round(income_sum_df$sd_income, 2), nsmall=2)),
           fontface=2, size=3, hjust=0, vjust=0.5) +
  theme_test() +
  theme(legend.position = c(0.55, 0.855),
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Weekly Earnings (USD)") +
  ylab("Number of Respondents") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous()

# Plot
earnings_plot <- ggplot(subset(jikinge, !is.na(earning_vs_typical)),
                        aes(x=earning_vs_typical)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), color="black", fill="#00BFC4") +
  theme_test() +
  theme(legend.position = "none",
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_text(face="bold", angle=90, hjust=1, vjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("\nEarnings vs. Typical") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.7), breaks=c(0,0.2,0.4,0.6,0.8,1))

# Plot
future_plot <- ggplot(subset(jikinge, !is.na(economic_status_in_6m)),
                      aes(x=economic_status_in_6m)) + 
  geom_bar(aes(y=(..count..)/sum(..count..)), color="black", fill="#00BFC4") +
  theme_test() +
  theme(legend.position = "none",
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_text(face="bold", angle=90, hjust=1, vjust=0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Expected Household Economic\nStatus in 6 Months") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits=c(0,0.7), breaks=c(0,0.2,0.4,0.6,0.8,1))

# Compile figures into object
composite <- plot_grid(income_plot, earnings_plot, future_plot, ncol = 3, rel_widths = c(2,1,1.5))

# Print figure
ggsave(plot=composite, file="Economic status.pdf", width=9, height=3.5, units='in', dpi=600)

##############################################################################
# Graph of face-to-face talking
##############################################################################

# Plot
talked_plot <- ggplot(jikinge, aes(x=num_face_talked, group=date_period, color=date_period)) + 
  geom_density() +
  theme_test() +
  theme(legend.position = c(0.55, 0.855),
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Number Talked Face-to-Face") +
  ylab("Percentage of Respondents") +
  scale_y_continuous(labels = scales::percent_format(accuracy=1), limits = c(0,0.1))

# Print figure
ggsave(plot=talked_plot, file="Face-to-face.pdf", width=3.5, height=3.5, units='in', dpi=600)

##############################################################################
# Subgroup analyses
##############################################################################

# Collapse categorical variables
merged <- merged %>% mutate(
  diff_food_cat = case_when(
    diff_obtain_food == "Yes" ~ 1,
    diff_obtain_food == "No"  ~ 0)
)
merged <- merged %>% mutate(
  worried_food_cat = case_when(
    worried_about_food %in% c(1,2) ~ 0,
    worried_about_food %in% c(3,4) ~ 1)
)

# Comparison tables
table1(~diff_obtain_food + worried_about_food_. + factor(worried_food_cat) + change_in_hours | sex_worker, data=merged)
table1(~diff_obtain_food + worried_about_food_. + worried_food_cat + change_in_hours | edu_cat, data=merged)

# Regressions for difficulty obtaining food
regression <- lm(diff_food_cat ~ sex_worker, merged)
summary(regression); round(confint(regression), 3); length(resid(regression))
regression <- lm(diff_food_cat ~ edu_cat, merged)
summary(regression); round(confint(regression), 3); length(resid(regression))

# Regressions for worry about food
regression <- lm(worried_food_cat ~ sex_worker, merged)
summary(regression); round(confint(regression), 3); length(resid(regression))
regression <- lm(worried_food_cat ~ edu_cat, merged)
summary(regression); round(confint(regression), 3); length(resid(regression))

# Regressions for change in hours
regression <- lm(change_in_hours ~ sex_worker, merged)
summary(regression); round(confint(regression), 1); length(resid(regression))
regression <- lm(change_in_hours ~ edu_cat, merged)
summary(regression); round(confint(regression), 1); length(resid(regression))

##############################################################################
# Longitudinal data preparation
##############################################################################

# Eliminate participants w/o recent follow-up
merged <- subset(merged, date.y >= as.Date("2019-09-01"))

##############################################################################
# Remaining subgroup analyses
##############################################################################

# Comparison tables
table1(~change_in_income | sex_worker, data=merged)
table1(~change_in_income | edu_cat, data=merged)

# Regressions for income
regression <- lm(change_in_income ~ sex_worker, merged)
summary(regression); round(confint(regression), 2); length(resid(regression))
regression <- lm(change_in_income ~ edu_cat, merged)
summary(regression); round(confint(regression), 2); length(resid(regression))

##############################################################################
# Longitudinal income
##############################################################################

# One-sample t-test
t.test(merged$change_in_income, mu=0, alternative="two.sided")

# Melt relevant variables
income_df <- melt(merged, id.vars = c("participant_id", "income_declined"),
                  measure.vars = c("weekly_income_out", "money_earned_past_week_us"))

# Rename variables
income_df <- income_df %>% mutate(
  variable = case_when(
    variable == "money_earned_past_week_us" ~ "During COVID-19",
    variable == "weekly_income_out"         ~ "Before COVID-19",
    TRUE                                    ~ NA_character_)
)

# Complete pre- and post
income_df <- income_df %>% filter_at(vars("income_declined"), all_vars(!is.na(.)))

# Summarize income
means_df <- income_df %>%
  group_by(variable) %>%
  summarise(
    mean   = mean(value,   na.rm=T),
    sd     = sd(value,     na.rm=T),
    median = median(value, na.rm=T)
  )

# Plot
income_plot_2 <- ggplot(income_df, aes(x=value, fill=variable)) +
  geom_vline(data=means_df, aes(xintercept=mean, color=variable), linetype="solid") +
  geom_vline(data=means_df, aes(xintercept=median, color=variable), linetype="dashed") +
  geom_density(alpha=0.5) +
  annotate("text", x=15, y=0.175, label=paste0("Mean    = $", round(means_df[2,"mean"], digit=2), "\nMedian = $", means_df[2,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=15, y=0.125, label=paste0("Mean    = $", round(means_df[1,"mean"], digit=2), "\nMedian = $", means_df[1,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  theme_test() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.title.x  = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Income Earned Per Week") +
  ylab("Density of Respondents") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous()

# Print figure
ggsave(plot=income_plot_2, file="Income 2.pdf", width=4, height=4, units='in', dpi=600)

# Compile figures into object
composite <- plot_grid(hours_plot, income_plot_2, ncol = 2)

# Print figure
ggsave(plot=composite, file="Economic changes.pdf", width=8, height=4, units='in', dpi=600)

##############################################################################
# Longitudinal sexual behavior
##############################################################################

# One-sample t-tests
t.test(merged$change_in_sex,      mu=0, alternative="two.sided")
t.test(merged$change_in_transact, mu=0, alternative="two.sided")

# Melt relevant variables
partner_df <- melt(merged, id.vars = c("participant_id", "partner_declined"),
                   measure.vars = c("num_sex_partners", "sex_partners_fu"))

# Rename variables
partner_df <- partner_df %>% mutate(
  variable = case_when(
    variable == "num_sex_partners" ~ "During COVID-19",
    variable == "sex_partners_fu"  ~ "Before COVID-19",
    TRUE ~ NA_character_)
)

# Complete pre- and post
partner_df <- partner_df %>% filter_at(vars("partner_declined"), all_vars(!is.na(.)))

# Summarize partners
means_df <- partner_df %>%
  group_by(variable) %>%
  summarise(
    mean   = mean(value,   na.rm=T),
    sd     = sd(value,     na.rm=T),
    median = median(value, na.rm=T)
  )

# Plot
sex_plot_3 <- ggplot(partner_df, aes(x=value, fill=variable)) +
  geom_vline(data=means_df, aes(xintercept=mean, color=variable), linetype="solid") +
  geom_vline(data=means_df, aes(xintercept=median, color=variable), linetype="dashed") +
  geom_density(alpha=0.5, adjust=3) +
  annotate("text", x=2.5, y=0.45, label=paste0("Mean    = ", round(means_df[2,"mean"], digit=1), "\nMedian = ", means_df[2,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=2.5, y=0.35, label=paste0("Mean    = ", round(means_df[1,"mean"], digit=1), "\nMedian = ", means_df[1,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  theme_test() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.title.x  = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Sexual Partners") +
  ylab("Density of Respondents") +
  scale_x_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10)) +
  scale_y_continuous(limits=c(0,0.5))

# Melt relevant variables
sex_work_df <- melt(merged, id.vars = c("participant_id", "sex_work_declined"),
                    measure.vars = c("transact_sex_cov", "transact_sex_fu"))

# Rename variables
sex_work_df <- sex_work_df %>% mutate(
  variable = case_when(
    variable == "transact_sex_cov" ~ "During COVID-19",
    variable == "transact_sex_fu"  ~ "Before COVID-19",
    TRUE ~ NA_character_)
)

# Complete pre- and post
sex_work_df <- sex_work_df %>% filter_at(vars("sex_work_declined"), all_vars(!is.na(.)))

# Summarize transactional partners
means_df <- sex_work_df %>%
  group_by(variable) %>%
  summarise(
    mean   = mean(value,   na.rm=T),
    sd     = sd(value,     na.rm=T),
    median = median(value, na.rm=T)
  )

# Plot
sex_plot_4 <- ggplot(sex_work_df, aes(x=value, fill=variable)) +
  geom_vline(data=means_df, aes(xintercept=mean, color=variable), linetype="solid") +
  geom_vline(data=means_df, aes(xintercept=median, color=variable), linetype="dashed") +
  geom_density(alpha=0.5, adjust=3) +
  annotate("text", x=1.5, y=0.55, label=paste0("Mean    = ", round(means_df[2,"mean"], digit=1), "\nMedian = ", means_df[2,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=1.5, y=0.45, label=paste0("Mean    = ", round(means_df[1,"mean"], digit=1), "\nMedian = ", means_df[1,"median"]), fontface=2, size=3, hjust=0, vjust=0.5) +
  theme_test() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.title.x  = element_text(face="bold"),
        panel.grid.major.x = element_line(color="gray", size=0.25),
        panel.grid.major.y = element_line(color="gray", size=0.25)) +
  xlab("Transactional Sex Partners") +
  ylab("Density of Respondents") +
  scale_x_continuous(limits=c(0,10), breaks=c(0,2,4,6,8,10)) #+
#scale_y_continuous(limits=c(0))

# Compile figures into object
composite <- plot_grid(sex_plot_3, sex_plot_4, ncol = 2)

# Print figure
ggsave(plot=composite, file="Sexual behavior 2.pdf", width=8, height=4, units='in', dpi=600)