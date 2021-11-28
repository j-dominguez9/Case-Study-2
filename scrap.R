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
ggplot(work) +
  lapply(names(work)[2:length(work)], FUN = function(i) geom_line(aes_string(x = time, y = i)))
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
work$attrition %>% str_replace_all(c("Yes" = 1, "No" = 0))
work %>% select(education_field, attrition) %>% str_replace_all(attrition, c("Yes" = 1, "No" = 0))
total <- work %>% select(education_field, attrition) %>% 
  group_by(education_field) %>% arrange(education_field) %>% 
  count(education_field) %>% as.data.frame()
yes_count <- work %>% select(education_field, attrition) %>% 
  group_by(education_field) %>% arrange(education_field) %>% 
  filter(attrition == "Yes") %>% count(education_field) %>% 
  rename(yes = n) %>% as.data.frame()
head(yes_count)
head(total)
remove(x)
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
?inner_joinjj

