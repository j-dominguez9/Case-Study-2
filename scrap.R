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
total <- work %>% select(attrition, total_working_years) %>% group_by(total_working_years) %>% arrange(total_working_years) %>% count(total_working_years) %>% as.data.frame()
yes_count <- work %>% select(total_working_years, attrition) %>% group_by(total_working_years) %>% arrange(total_working_years) %>% filter(attrition == "1") %>% count(total_working_years) %>% rename(yes = n) %>% as.data.frame()
head(total)
yes_count
join <- full_join(total, yes_count, by = "total_working_years")
join <- join %>% mutate(perc = (yes/n)*100)
join %>% ggplot(aes(x = total_working_years, y = perc)) + geom_bar(stat = 'identity')


