library(tidyverse)
library(janitor)
work <- read_csv(file.choose())
head(work)
work <- work %>% janitor::clean_names()
head(work)
sum(is.na(work))
colnames(work)
summary(work$attrition)
str(work)
hist(work$attrition)
work %>% ggplot(aes(x = job_role)) + geom_histogram(stat = 'count') + coord_flip()
work %>% ggplot(aes(x = relationship_satisfaction, y = attrition)) + geom_boxplot()
work %>% ggplot(aes(x = attrition, y = )) + geom_boxplot() + facet_grid()
head(work)
work.gathered <- work %>%
  as_data_frame() %>%
  gather(key = "variable", value = "value",
         -attrition)
head(work.gathered, 3)
ggplot(work.gathered, aes(x = value, y = attrition)) + geom_boxplot() + facet_wrap(~variable) + scale_color_viridis_c()
work.gathered %>% ggplot(aes(x = value, y = attrition)) + geom_bar(aes(color = attrition), stat = 'identity') + facet_wrap(~variable)
work %>% ggplot(aes(x = gender, y = attrition)) + geom_bar(aes(color = attrition), stat = 'identity')
head(work.gathered)
View(work.gathered)

work %>% ggplot(aes(x = education_field, y = attrition, color = attrition)) + geom_bar(stat = 'identity')
work %>% select(attrition, education_field) %>% group_by(education_field) %>% arrange(education_field) %>% pivot_wider(names_from=education_field)
work %>% select(attrition, education_field) %>% pivot_wider(names_from = education_field, values_from = attrition)
work %>% select(attrition, education_field) %>% group_by(education_field) %>% arrange(education_field) %>% filter(attrition == "Yes") %>% pivot_wider(names_from = education_field, )
work %>% pivot_wider(names_from = education_field, values_from = attrition)
work %>% select(education_field, attrition) %>% pivot_wider(names_from = education_field, values_from = attrition)
work$attrition <- as.factor(work$attrition)
fit <- aov(attrition~education_field, data = work)
total <- work %>% select(education_field, attrition) %>% 
  group_by(education_field) %>% arrange(education_field) %>% 
  count(education_field) %>% as.data.frame()
yes_count <- work %>% select(education_field, attrition) %>% 
  group_by(education_field) %>% arrange(education_field) %>% 
  filter(attrition == "Yes") %>% count(education_field) %>% 
  rename(yes = n) %>% as.data.frame()
head(yes_count)
head(total)

merge(total, yes_count, by = "education_field")
join <- full_join(total, yes_count, by = "education_field") %>% mutate(perc = (yes/n)*100)
join$education_field <- as.factor(join$education_field)
str(join)
head(join)
fit = aov(perc~education_field, data = join)
gfit = glht(fit, linfct = mcp(education_field="Dunnett"))
summary(gfit)
summary(fit)
work %>% colnames()
head(work$employee_count)
work$employee_count <- NULL
work$over18 <- NULL
colnames()
work %>% filter(standard_hours=="80")
work$standard_hours <- NULL
work$id <- NULL
work$employee_number <- NULL
head(work)
work$business_travel <- as_factor(work$business_travel)
work$attrition <- as.factor(work$attrition)
work %>% ggplot(aes(x = attrition, y = daily_rate)) + geom_point()
View(work)
is.numeric(work$age)
join %>% ggplot(aes(x = education_field, y = perc)) + geom_bar(stat = 'identity')
is.factor(work$attrition)
work$attrition <- as.character(work$attrition)
is.character(work$attrition)
work <- work %>% mutate(attrition = if_else(attrition == "No", 0, 1))
total <- work %>% select(age, attrition) %>% group_by(age) %>% arrange(age) %>% count(age) %>% as.data.frame()
yes_count <- work %>% select(age, attrition) %>% group_by(age) %>% arrange(age) %>% filter(attrition == "1") %>% count(age) %>% rename(yes = n) %>% as.data.frame()
join <- full_join(total, yes_count, by = "age")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = age, y = perc)) + geom_bar(stat = 'identity')

###travel
work.age <- aggregate(x = work$attrition, by = list(cut(work$age, 7)), mean)
names(work.age) <- c("age", "attrition")
work.age
work.age %>% ggplot(aes(x = age, y = attrition)) + geom_bar(stat = 'identity')
range(work$age)
60-18
42/7
total <- work %>% select(attrition, business_travel) %>% group_by(business_travel) %>% arrange(business_travel) %>% count(business_travel) %>% as.data.frame()
yes_count <- work %>% select(business_travel, attrition) %>% group_by(business_travel) %>% arrange(business_travel) %>% filter(attrition == "1") %>% count(business_travel) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "business_travel")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = business_travel, y = perc)) + geom_bar(stat = 'identity')


### department
total <- work %>% select(attrition, department) %>% group_by(department) %>% arrange(department) %>% count(department) %>% as.data.frame()
yes_count <- work %>% select(department, attrition) %>% group_by(department) %>% arrange(department) %>% filter(attrition == "1") %>% count(department) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "department")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = department, y = perc)) + geom_bar(stat = 'identity')

### distance from home
total <- work %>% select(attrition, distance_from_home) %>% group_by(distance_from_home) %>% arrange(distance_from_home) %>% count(distance_from_home) %>% as.data.frame()
yes_count <- work %>% select(distance_from_home, attrition) %>% group_by(distance_from_home) %>% arrange(distance_from_home) %>% filter(attrition == "1") %>% count(distance_from_home) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
range(work$distance_from_home)
join <- full_join(total, yes_count, by = "distance_from_home")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = distance_from_home, y = perc)) + geom_bar(stat = 'identity')

### education
total <- work %>% select(attrition, education) %>% group_by(education) %>% arrange(education) %>% count(education) %>% as.data.frame()
yes_count <- work %>% select(education, attrition) %>% group_by(education) %>% arrange(education) %>% filter(attrition == "1") %>% count(education) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "education")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = education, y = perc)) + geom_bar(stat = 'identity')

### education field
total <- work %>% select(attrition, education_field) %>% group_by(education_field) %>% arrange(education_field) %>% count(education_field) %>% as.data.frame()
yes_count <- work %>% select(education_field, attrition) %>% group_by(education_field) %>% arrange(education_field) %>% filter(attrition == "1") %>% count(education_field) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "education_field")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = education_field, y = perc)) + geom_bar(stat = 'identity')

### environment satisfaction

total <- work %>% select(attrition, environment_satisfaction) %>% group_by(environment_satisfaction) %>% arrange(environment_satisfaction) %>% count(environment_satisfaction) %>% as.data.frame()
yes_count <- work %>% select(environment_satisfaction, attrition) %>% group_by(environment_satisfaction) %>% arrange(environment_satisfaction) %>% filter(attrition == "1") %>% count(environment_satisfaction) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "environment_satisfaction")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = environment_satisfaction, y = perc)) + geom_bar(stat = 'identity')

### Gender
total <- work %>% select(attrition, gender) %>% group_by(gender) %>% arrange(gender) %>% count(gender) %>% as.data.frame()
yes_count <- work %>% select(gender, attrition) %>% group_by(gender) %>% arrange(gender) %>% filter(attrition == "1") %>% count(gender) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "gender")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = gender, y = perc)) + geom_bar(stat = 'identity')

### hourly rate
total <- work %>% select(attrition, hourly_rate) %>% group_by(hourly_rate) %>% arrange(hourly_rate) %>% count(hourly_rate) %>% as.data.frame()
yes_count <- work %>% select(hourly_rate, attrition) %>% group_by(hourly_rate) %>% arrange(hourly_rate) %>% filter(attrition == "1") %>% count(hourly_rate) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "hourly_rate")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = hourly_rate, y = perc)) + geom_bar(stat = 'identity')

### job involvement
total <- work %>% select(attrition, job_involvement) %>% group_by(job_involvement) %>% arrange(job_involvement) %>% count(job_involvement) %>% as.data.frame()
yes_count <- work %>% select(job_involvement, attrition) %>% group_by(job_involvement) %>% arrange(job_involvement) %>% filter(attrition == "1") %>% count(job_involvement) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "job_involvement")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = job_involvement, y = perc)) + geom_bar(stat = 'identity')


### job level
total <- work %>% select(attrition, job_role) %>% group_by(job_role) %>% arrange(job_role) %>% count(job_role) %>% as.data.frame()
yes_count <- work %>% select(job_role, attrition) %>% group_by(job_role) %>% arrange(job_role) %>% filter(attrition == "1") %>% count(job_role) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "job_role")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = job_role, y = perc)) + geom_bar(stat = 'identity')

### job role
total <- work %>% select(attrition, job_role) %>% group_by(job_role) %>% arrange(job_role) %>% count(job_role) %>% as.data.frame()
yes_count <- work %>% select(job_role, attrition) %>% group_by(job_role) %>% arrange(job_role) %>% filter(attrition == "1") %>% count(job_role) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "job_role")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = job_role, y = perc)) + geom_bar(stat = 'identity') + coord_flip()

### job satisfaction
total <- work %>% select(attrition, job_satisfaction) %>% group_by(job_satisfaction) %>% arrange(job_satisfaction) %>% count(job_satisfaction) %>% as.data.frame()
yes_count <- work %>% select(job_satisfaction, attrition) %>% group_by(job_satisfaction) %>% arrange(job_satisfaction) %>% filter(attrition == "1") %>% count(job_satisfaction) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "job_satisfaction")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = job_satisfaction, y = perc)) + geom_bar(stat = 'identity')

### marital status
total <- work %>% select(attrition, marital_status) %>% group_by(marital_status) %>% arrange(marital_status) %>% count(marital_status) %>% as.data.frame()
yes_count <- work %>% select(marital_status, attrition) %>% group_by(marital_status) %>% arrange(marital_status) %>% filter(attrition == "1") %>% count(marital_status) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "marital_status")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = marital_status, y = perc)) + geom_bar(stat = 'identity')

### monthly income
total <- work %>% select(attrition, monthly_income) %>% group_by(monthly_income) %>% arrange(monthly_income) %>% count(monthly_income) %>% as.data.frame()
yes_count <- work %>% select(monthly_income, attrition) %>% group_by(monthly_income) %>% arrange(monthly_income) %>% filter(attrition == "1") %>% count(monthly_income) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "monthly_income")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = monthly_income, y = perc)) + geom_bar(stat = 'identity')
agg_inc <- aggregate(x = work$att_factor, by = list(cut(work$monthly_income, 6)), mean)
names(agg_inc) <- c("monthly_income", "attrition")
agg_inc %>% ggplot(aes(x = monthly_income, y = attrition)) + geom_bar(stat = 'identity')
is.numeric(work$attrition)
work <- work %>% mutate(att_factor = as.factor(work$attrition))
is.factor(work$att_factor)
discretize(work$monthly_income, breaks = 6)
install.packages("arules")
library(arules)
total <- work %>% select(attrition, monthly_income) %>%
  mutate(monthly_income = discretize(monthly_income, breaks = 6)) %>% 
  group_by(monthly_income) %>% arrange(monthly_income) %>% 
  count(monthly_income) %>% as.data.frame()
yes_count <- work %>% select(attrition, monthly_income) %>% 
  mutate(monthly_income = discretize(monthly_income, breaks = 6)) %>% filter(attrition == "1") %>% 
  group_by(monthly_income) %>% arrange(monthly_income) %>% 
  count(monthly_income) %>% rename(yes = n) %>% as.data.frame()
yes_count
join <- full_join(total, yes_count, by = "monthly_income")
join
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = monthly_income, y = perc)) + geom_bar(stat = 'identity') + coord_flip()


### number of companies worked
total <- work %>% select(attrition, num_companies_worked) %>% group_by(num_companies_worked) %>% arrange(num_companies_worked) %>% count(num_companies_worked) %>% as.data.frame()
yes_count <- work %>% select(num_companies_worked, attrition) %>% group_by(num_companies_worked) %>% arrange(num_companies_worked) %>% filter(attrition == "1") %>% count(num_companies_worked) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "num_companies_worked")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = num_companies_worked, y = perc)) + geom_bar(stat = 'identity')

### over time
total <- work %>% select(attrition, over_time) %>% group_by(over_time) %>% arrange(over_time) %>% count(over_time) %>% as.data.frame()
yes_count <- work %>% select(over_time, attrition) %>% group_by(over_time) %>% arrange(over_time) %>% filter(attrition == "1") %>% count(over_time) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "over_time")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = over_time, y = perc)) + geom_bar(stat = 'identity')

### percent salary hike
total <- work %>% select(attrition, percent_salary_hike) %>% group_by(percent_salary_hike) %>% arrange(percent_salary_hike) %>% count(percent_salary_hike) %>% as.data.frame()
yes_count <- work %>% select(percent_salary_hike, attrition) %>% group_by(percent_salary_hike) %>% arrange(percent_salary_hike) %>% filter(attrition == "1") %>% count(percent_salary_hike) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "percent_salary_hike")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = percent_salary_hike, y = perc)) + geom_bar(stat = 'identity')

### performance rating
total <- work %>% select(attrition, performance_rating) %>% group_by(performance_rating) %>% arrange(performance_rating) %>% count(performance_rating) %>% as.data.frame()
yes_count <- work %>% select(performance_rating, attrition) %>% group_by(performance_rating) %>% arrange(performance_rating) %>% filter(attrition == "1") %>% count(performance_rating) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "performance_rating")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = performance_rating, y = perc)) + geom_bar(stat = 'identity')


### relationship satisfaction
total <- work %>% select(attrition, relationship_satisfaction) %>% group_by(relationship_satisfaction) %>% arrange(relationship_satisfaction) %>% count(relationship_satisfaction) %>% as.data.frame()
yes_count <- work %>% select(relationship_satisfaction, attrition) %>% group_by(relationship_satisfaction) %>% arrange(relationship_satisfaction) %>% filter(attrition == "1") %>% count(relationship_satisfaction) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "relationship_satisfaction")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = relationship_satisfaction, y = perc)) + geom_bar(stat = 'identity')


### stock option level
total <- work %>% select(attrition, stock_option_level) %>% group_by(stock_option_level) %>% arrange(stock_option_level) %>% count(stock_option_level) %>% as.data.frame()
yes_count <- work %>% select(stock_option_level, attrition) %>% group_by(stock_option_level) %>% arrange(stock_option_level) %>% filter(attrition == "1") %>% count(stock_option_level) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "stock_option_level")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = stock_option_level, y = perc)) + geom_bar(stat = 'identity')

### total working years
total <- work %>% select(attrition, total_working_years) %>% 
  mutate(total_working_years = discretize(total_working_years, method = 'interval', breaks = 5)) %>% 
  group_by(total_working_years) %>% arrange(total_working_years) %>% count(total_working_years) %>% 
  as.data.frame()
yes_count <- work %>% select(total_working_years, attrition) %>% filter(attrition == "1") %>% 
  mutate(total_working_years = discretize(total_working_years, method = 'interval', breaks = 5)) %>% 
  group_by(total_working_years) %>% arrange(total_working_years) %>% 
  count(total_working_years) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "total_working_years")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = total_working_years, y = perc)) + geom_bar(stat = 'identity')

### training times last year
total <- work %>% select(attrition, training_times_last_year) %>% group_by(training_times_last_year) %>% arrange(training_times_last_year) %>% count(training_times_last_year) %>% as.data.frame()
yes_count <- work %>% select(training_times_last_year, attrition) %>% group_by(training_times_last_year) %>% arrange(training_times_last_year) %>% filter(attrition == "1") %>% count(training_times_last_year) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "training_times_last_year")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = training_times_last_year, y = perc)) + geom_bar(stat = 'identity')

### work life balance
total <- work %>% select(attrition, work_life_balance) %>% group_by(work_life_balance) %>% arrange(work_life_balance) %>% count(work_life_balance) %>% as.data.frame()
yes_count <- work %>% select(work_life_balance, attrition) %>% group_by(work_life_balance) %>% arrange(work_life_balance) %>% filter(attrition == "1") %>% count(work_life_balance) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "work_life_balance")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = work_life_balance, y = perc)) + geom_bar(stat = 'identity')

### years at company
total <- work %>% select(attrition, years_at_company) %>% 
  mutate(years_at_company = discretize(years_at_company, method = 'interval', breaks = 4)) %>% 
  group_by(years_at_company) %>% arrange(years_at_company) %>% count(years_at_company) %>% as.data.frame()
yes_count <- work %>% select(years_at_company, attrition) %>% 
  mutate(years_at_company = discretize(years_at_company, method = 'interval', breaks = 4)) %>% 
  filter(attrition == "1") %>% group_by(years_at_company) %>% arrange(years_at_company) %>% 
  count(years_at_company) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "years_at_company")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = years_at_company, y = perc)) + geom_bar(stat = 'identity')

### years at current role
total <- work %>% select(attrition, years_in_current_role) %>% 
  mutate(years_in_current_role = discretize(years_in_current_role, method = 'interval', breaks = 6)) %>% 
  group_by(years_in_current_role) %>% arrange(years_in_current_role) %>% count(years_in_current_role) %>% as.data.frame()
yes_count <- work %>% select(years_in_current_role, attrition) %>% 
  mutate(years_in_current_role = discretize(years_in_current_role, method = 'interval', breaks = 6)) %>% 
  filter(attrition == "1") %>% group_by(years_in_current_role) %>% arrange(years_in_current_role) %>% 
  count(years_in_current_role) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "years_in_current_role")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = years_in_current_role, y = perc)) + geom_bar(stat = 'identity')


### years since last promotion
total <- work %>% select(attrition, years_since_last_promotion) %>% 
  mutate(years_since_last_promotion = discretize(years_since_last_promotion, method = 'interval', breaks = 5)) %>% 
  group_by(years_since_last_promotion) %>% arrange(years_since_last_promotion) %>% count(years_since_last_promotion) %>% as.data.frame()
yes_count <- work %>% select(years_since_last_promotion, attrition) %>% 
  mutate(years_since_last_promotion = discretize(years_since_last_promotion, method = 'interval', breaks = 5)) %>% 
  filter(attrition == "1") %>% group_by(years_since_last_promotion) %>% arrange(years_since_last_promotion) %>% 
  count(years_since_last_promotion) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "years_since_last_promotion")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = years_since_last_promotion, y = perc)) + geom_bar(stat = 'identity')

### years with current manager
total <- work %>% select(attrition, years_with_curr_manager) %>% 
  mutate(years_with_curr_manager = discretize(years_with_curr_manager, method = 'interval', breaks = 6)) %>% 
  group_by(years_with_curr_manager) %>% arrange(years_with_curr_manager) %>% count(years_with_curr_manager) %>% as.data.frame()
yes_count <- work %>% select(years_with_curr_manager, attrition) %>% 
  mutate(years_with_curr_manager = discretize(years_with_curr_manager, method = 'interval', breaks = 6)) %>% 
  filter(attrition == "1") %>% group_by(years_with_curr_manager) %>% arrange(years_with_curr_manager) %>% 
  count(years_with_curr_manager) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "years_with_curr_manager")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = years_with_curr_manager, y = perc)) + geom_bar(stat = 'identity')

remove(wave_theta)
## Significance
### Internal
#### - Work/Life Balance
#### - Percent Salary Hike
#### - Overtime
#### - Income
#### - Job involvement
### worth exploring
#### - job satisfaction
#### - job role
### External
#### - Work/Life Balance
#### - Distance from home
#### - Age
### worth exploring
#### - total working years

## percent_salary_hike has highest AIC, so out

model

predict(model, test1)
dim(model)

head(test1)

att_step <- ols_step_both_p(attrition~., data = work, pent = 0.01, prem = 0.01)

pct <- C(work$attrition, treatment)
ot <- C(factor(work$over_time), treatment)
is.factor(work$attrition)

attributes(pct)
summary(lm(work_life_balance ~ pct, data = work))
logit <- glm(attrition ~ factor(work_life_balance), data = work, family = 'binomial')
wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 2:4)
exp(coef(logit))
nd1 <- data.frame(work_life_balance = factor(1:4))
nd1
nd1$wlbp <- predict(logit, newdata = nd1, type = 'response') 
nd1
with(logit, null.deviance - deviance)
with(logit, df.null - df.residual)
with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(logit)
summary(glm(attrition ~ percent_salary_hike, data = work, family = 'binomial'))
summary(glm(attrition ~ factor(over_time), data = work, family = 'binomial'))
summary(glm(attrition ~ monthly_income, data = work, family = 'binomial'))
summary(glm(attrition ~ factor(job_involvement), data = work, family = 'binomial'))
install.packages("aod")
library(aod)
perc_sal_hike.mod <- glm(attrition ~ percent_salary_hike, data = work, family = 'binomial')
over_time.mod <- glm(attrition ~ factor(over_time), data = work, family = 'binomial')
monthly_inc.mod <- glm(attrition ~ monthly_income, data = work, family = 'binomial')
job_inv.mod <- glm(attrition ~ factor(job_involvement), data = work, family = 'binomial')
wlb.mod <- glm(attrition ~ factor(work_life_balance), data = work, family = 'binomial')
psh.ot.mod <- glm(attrition ~ percent_salary_hike +  factor(over_time), data = work, family = 'binomial')
psh.mi.mod <- glm(attrition ~ percent_salary_hike +  monthly_income, data = work, family = 'binomial')
psh.ji.mod <- glm(attrition ~ percent_salary_hike +  factor(job_involvement), data = work, family = 'binomial')
psh.wlb.mod <- psh.ot.mod <- glm(attrition ~ percent_salary_hike + factor(work_life_balance), data = work, family = 'binomial')
ot.mi.mod <- glm(attrition ~ factor(over_time) + monthly_income, data = work, family = 'binomial')
ot.ji.mod <- glm(attrition ~ factor(over_time) + factor(job_involvement), data = work, family = 'binomial')
ot.wlb.mod <- glm(attrition ~ factor(over_time) + factor(work_life_balance), data = work, family = 'binomial')
mi.ji.mod <- glm(attrition ~ monthly_income + factor(job_involvement), data = work, family = 'binomial')
mi.wlb.mod <- glm(attrition ~ monthly_income + factor(work_life_balance), data = work, family = 'binomial')
ji.wlb.mod <- glm(attrition ~ factor(job_involvement) + factor(work_life_balance), data = work, family = 'binomial')
mi.ot.int.mod <- glm(attrition ~ factor(over_time)*monthly_income, data = work, family = 'binomial')
mi.ji.int.mod <- glm(attrition ~ factor(over_time)*factor(job_involvement), data = work, family = 'binomial')
mi.wlb.int.mod <- glm(attrition ~ monthly_income*factor(work_life_balance), data = work, family = 'binomial')
ot.ji.wlb.mod <- glm(attrition ~ factor(over_time) + factor(job_involvement) + factor(work_life_balance), data = work, family = 'binomial')
ot.ji.wlb.int.mod <- glm(attrition ~ factor(over_time) + factor(job_involvement)*factor(work_life_balance), data = work, family = 'binomial')
ot.wlb.int.ji.mod <- glm(attrition ~ factor(over_time)*factor(work_life_balance) + factor(job_involvement), data = work, family = 'binomial')
ot.ji.mi.mod <- glm(attrition ~ factor(over_time) + factor(job_involvement) + monthly_income, data = work, family = 'binomial')
ot.ji.mi.int.mod <- glm(attrition ~ factor(over_time) + factor(job_involvement)*monthly_income, data = work, family = 'binomial')
ot.ji.int.mi.mod <- glm(attrition ~ factor(over_time)*factor(job_involvement) + monthly_income, data = work, family = 'binomial')
wlb.ji.yicr.mod <- glm(attrition ~ factor(work_life_balance) + factor(job_involvement) + years_in_current_role, data = work, family = 'binomial')
wlb.yicr.mod <- glm(attrition ~ factor(work_life_balance) + years_in_current_role, data = work, family = 'binomial')
yicr.mod <- glm(attrition ~ years_in_current_role, data = work, family = 'binomial')
ji.yicr.mood <- glm(attrition ~ years_in_current_role + factor(job_involvement), data = work, family = 'binomial')
install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(over_time.mod, monthly_inc.mod, job_inv.mod, wlb.mod, psh.ji.mod, ot.mi.mod, ot.ji.mod, ot.wlb.mod, mi.ji.mod, mi.wlb.mod, ji.wlb.mod, mi.ot.int.mod, mi.ji.int.mod, mi.wlb.int.mod, ot.ji.wlb.mod, ot.ji.wlb.int.mod, ot.wlb.int.ji.mod, ot.ji.mi.mod, ot.ji.mi.int.mod, ot.ji.int.mi.mod, wlb.ji.yicr.mod, wlb.yicr.mod, yicr.mod, ji.yicr.mood)
model.names <- c('over_time.mod', 'monthly_inc.mod', 'job_inv.mod', 'wlb.mod', 'psh.ji.mod', 'ot.mi.mod', 'ot.ji.mod', 'ot.wlb.mod', 'mi.ji.mod', 'mi.wlb.mod', 'ji.wlb.mod', 'mi.ot.int.mod', 'mi.ji.int.mod', 'mi.wlb.int.mod', 'ot.ji.wlb.mod', 'ot.ji.wlb.int.mod', 'ot.wlb.int.ji.mod', 'ot.ji.mi.mod', 'ot.ji.mi.int.mod', 'ot.ji.int.mi.mod', 'wlb.ji.yicr.mod', 'wlb.yicr.mod', 'yicr.mod', 'ji.yicr.mood')
aictab(cand.set = models, modnames = model.names)


#----- job role trends
is.factor(work$job_satisfaction)
js <- work %>% select(job_role, job_satisfaction) %>% group_by(job_role) %>% mutate(job_role = factor(job_role))
js
work %>% ggplot(aes(x = factor(job_role), y = factor(job_satisfaction))) + geom_bar(stat = 'summary', fun = 'mean')
work %>% ggplot(aes(x = factor(job_role), y = factor(job_satisfaction))) + stat_summary(fun = 'mean', geom = 'bar')

js2 <- ddply(js, .(job_role), summarize, mean = mean(job_satisfaction))
js2 %>% ggplot(aes(x = job_role, y = mean)) + geom_bar(stat = 'identity') + coord_flip()


total <- work %>% select(job_role, over_time) %>% mutate(over_time = if_else(over_time == "No", 0, 1)) %>% 
  group_by(job_role) %>% arrange(job_role) %>% count(job_role) %>% as.data.frame()
yes_count <- work %>% select(job_role, over_time) %>% 
  mutate(over_time = if_else(over_time == "No", 0, 1)) %>% 
  group_by(job_role) %>% arrange(job_role) %>% filter(over_time == "1") %>% 
  count(job_role) %>% rename(yes = n) %>% as.data.frame()
total
yes_count
join <- full_join(total, yes_count, by = 'job_role') %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = job_role, y = perc)) + geom_bar(stat = 'identity') + coord_flip()



fit = lm(monthly_income~., data = work)
summary(fit)

set.seed(1)

splitPerc = .75
mi_model <-  work %>% dplyr::select(everything(), -YearsAtCompany, -TotalWorkingYears, -JobLevel, -PercentSalaryHike )
trainInd <- sample(1:dim(work)[1],round(splitPerc * dim(work)[1]))
work_model_Train = work[trainInd,]
work_model_train2 <- work_model_Train %>% select(everything(), -monthly_income)
work_model_Test = work[-trainInd,]
work_model_test2 <- work_model_Test %>% select(everything(), -monthly_income)
colnames(work_model_Test)


mi_lm <- lm(monthly_income~., data = work_model_Train)
sqrt(mean(mi_lm$residuals^2))
mi_lm_2 <- lm(monthly_income~., data = work)
sqrt(mean(mi_lm_2$residuals^2))
no_salary <- read.csv(file.choose())
is.data.frame(no_salary)
predict(mi_lm_2, newdata = no_salary)
no_salary <- no_salary %>% janitor::clean_names()
no_salary$employee_count <- NULL
no_salary$over18 <- NULL
no_salary$standard_hours <- NULL
no_salary<- NULL
no_salary$employee_number <- NULL
predict <- predict.lm(mi_lm, newdata = work_model_Test, type = 'response')
error <- work_model_Test$monthly_income-predict
rmse <- sqrt(mean(error^2))
rmse



model <- lm(y~x+x1, data=train)
p.test <- predict(model, newdata=test, type='response')
error <- test$MonthlyIncome-p.test
rmse <- sqrt(mean(error^2))
c(RMSE = rmse, R2=summary(model)$r.squared)

### Naive-Bayes
nb <- naiveBayes(work_model_train2, work_model_Train$monthly_income)
pred <- predict(nb, work_model_Test)
pred
error <- work_model_Test$monthly_income-as.numeric(pred)
sqrt(mean(error^2))

install.packages("olsrr")
library(olsrr)
olsrr::ols_step_forward_p(mi_lm_2, penter = 0.05, details = TRUE)
xyz <- lm(monthly_income~job_level +  job_role + total_working_years + business_travel, data = work)
error <- work_model_Test$monthly_income-predict
rmse <- sqrt(mean(error^2))
rmse
sqrt(mean(xyz$residuals^2))
ols_step_both_p(mi_lm_2, penter = 0.05, prem = 0.05, details = TRUE)
ols_step_backward_p(mi_lm_2, prem = 0.05, details = TRUE)

lp <- glm(attrition~., data = work, family = 'binomial')
library(leaps)
summary(leaps::regsubsets(attrition~., data = work, method = 'exhaustive'))
?regsubsets()

test_model <- glm(attrition~., data = work, family = 'binomial', maxit = 100)
att_imp_vars <- varImp(test_model) %>% as.data.frame()
att_imp_vars %>% names(att_imp_vars)[1] = "Name"
att_imp_vars[[1]]
df_att_rank <- data.frame(Name = c("age", "business_travelTravel_Frequently", "business_travelNon-Travel", 'daily_rate', 'departmentResearch&Development', 'departmentSales', 'distance_from_home', 'education', 'education_fieldLife_Sciences', 'education_fieldMarketing', 'education_fieldMedical', 'education_fieldOther', 'education_fieldTechnical_Degree', 'environment_satisfaction', 'genderMale', 'hourly_rate', 'job_involvement', 'job_level', 'job_roleHuman_Resources', 'job_roleLaboratory Technician', 'job_roleManager', 'job_roleManufacturing_Director', 'job_roleResearch_Director', 'job_roleResearch_Scientist', 'job_roleSales Executive', 'job_roleSales Representative', 'job_satisfaction', 'marital_statusMarried', 'marital_statusSingle', 'monthly_income', 'monthly_rate', 'num_companies_worked', 'over_timeYes', 'percent_salary_hike', 'performance_rating', 'relationship_satisfaction', 'stock_option_level', 'total_working_years', 'training_times_last_year', 'work_life_balance', 'years_at_company', 'years_in_current_role', 'years_since_last_promotion', 'years_with_curr_manager', 'att_factor1'), Overall = att_imp_vars$Overall)
df_att_rank %>% arrange(desc(Overall))
olsrr::ols_step_forward_p(test_model, penter = 0.05, details = TRUE)
olsrr::ols_step_best_subset(mi_lm_2)
models <- regsubsets(monthly_income~., data = work, nvmax = 20)
res.sum <- summary(models)
res.sum
data.frame(
  Adj.R2 = which.max(lp_mod$adjr2),
  CP = which.min(lp_mod$cp),
  BIC = which.min(lp_mod$bic)
)
is.factor(work$over_time)
work$department <- as.factor(work$department)
work$education_field <- as.factor(work$education_field)
work$gender <- as.factor(work$gender)
work$marital_status <- as.factor(work$marital_status)
work$over_time <- as.factor(work$over_time)

levels(work$education_field)
work$education_field <- recode_factor(work$education_field,"Human Resources" = 'human_resources',"Marketing" = 'marketing', "Medical" = 'medical',"Other" = 'other', "Technical Degree" = 'technical_degree', "Life Sciences" = 'life_sciences')
levels(work$education_field)
levels(work$department)
work$department <- recode_factor(work$department, "Human Resources" = 'human_resources', "Research & Development" = 'research_and_development', "Sales" = 'sales')
levels(work$job_role)
work$job_role <- recode_factor(work$job_role, "Healthcare Representative" = 'healthcare_representative', "Human Resources" = 'human_resources', "Laboratory Technician" = 'laboratory_technician', "Manager" = 'manager', "Manufacturing Director" = 'manufacturing_director', "Research Director" = 'research_director', "Research Scientist" = 'research_scientist', "Sales Executive" = 'sales_executive', "Sales Representative" = 'sales_representative')


?naiveBayes

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}


get_model_formula(15, models, "monthly_income")
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}
model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, models, "monthly_income") %>%
  map(get_cv_error, data = work) %>%
  unlist()
cv.errors

coef(models, 10)
models

leaps_mod <- lm(monthly_income ~ business_travel + daily_rate + department + 
                 + job_level + job_role + job_role  + 
                  job_role + monthly_rate + total_working_years + 
                  years_since_last_promotion + years_with_curr_manager, data = work_model_Train)

pred <- predict.lm(leaps_mod, newdata = work_model_Test, type = 'response')
error <- work_model_Test$monthly_income-pred
rmse <- sqrt(mean(error^2))
rmse
### train/test RMSE: 1053.978
### AIC: 10947.86
AIC(leaps_mod)


leaps_mod_2 <- lm(monthly_income ~ business_travel + daily_rate + department + 
                    + job_level + job_role + job_role  + 
                    job_role + monthly_rate + total_working_years + 
                    years_since_last_promotion + years_with_curr_manager, data = work)
ols_press(leaps_mod_2)
### PRESS : 976591613
sqrt(mean(leaps_mod_2$residuals^2))
### RMSE: 1038.094
ols_regress(leaps_mod_2)
### adj r2: 0.948
ols_aic(leaps_mod_2)
### AIC: 14593.5

library(olsrr)
ols_step_both_p(mi_lm_2, pent = 0.01, prem = 0.01, details = TRUE)
stepwise_mod <- lm(monthly_income~ job_level + job_role + total_working_years + business_travel, data = work)
ols_press(leaps_mod_2)
### PRESS: 983282198
ols_coll_diag(stepwise_mod)
ols_aic(stepwise_mod)
ols_regress(stepwise_mod)
sqrt(mean(stepwise_mod$residuals^2))
### RMSE: 1048.192
### adj. r2 : .947
### AIC: 14598.34

stepwise_mod <- lm(monthly_income~ job_level + job_role + total_working_years + business_travel, data = work_model_Train)
pred <- predict.lm(stepwise_mod, newdata = work_model_Test, type = 'response')
error <- work_model_Test$monthly_income-pred
rmse <- sqrt(mean(error^2))
rmse
#### train/test RMSE: 1049.652
AIC(stepwise_mod)
### AIC: 10951.16
ols_mallows_cp(stepwise_mod, work_mod)
work_mod <- lm(monthly_income~., data = work)

colnames(no_salary)
library(here)
here()
comp <- tribble(
  ~Name, ~MallowsCp, ~ adjr2, ~AIC, ~PRESS,
  "forward_mod", -4.608704, 0.947, 14598.34, 983282198,
  "leaps_mod_2", -11.01006, 0.948, 14593.5, 976591613,
  "stepwise_mod", -4.608704, 0.947, 14598.34, 983282198,
)

comp %>% ggplot(aes(x = Name, y = MallowsCp))+
  geom_bar(stat = 'identity', fill = 'skyblue4') + 
  theme_minimal() + ggtitle("Model Comparison (Mallows Cp)")

comp %>% ggplot(aes(x = Name, y = adjr2))+
  geom_bar(stat = 'identity', fill = 'skyblue4') + coord_cartesian(ylim = c(0.94, 0.95)) + 
  theme_minimal() + ggtitle("Model Comparison (Adjusted R^2)")


comp %>% ggplot(aes(x = Name, y = AIC))+
  geom_bar(stat = 'identity', fill = 'skyblue4') + coord_cartesian(ylim = c(14575, 14600)) +
  theme_minimal() + ggtitle("Model Comparison (AIC)")


comp %>% ggplot(aes(x = Name, y = PRESS))+
  geom_bar(stat = 'identity', fill = 'skyblue4') + coord_cartesian(ylim = c(975000000, 984000000)) +
  theme_minimal() + ggtitle("Model Comparison (PRESS)")

conflicts()
rename
work %>% select(total_working_years) %>% filter(total_working_years == 1:10)
