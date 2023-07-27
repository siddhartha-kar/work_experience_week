# Work Experience Week - Kar lab

# https://rdrr.io/snippets/

library(tidyverse)

data(esoph)

glimpse(esoph)

summary(esoph)

stripchart(ncases ~ agegp, data=esoph)

stripchart(ncases ~ tobgp, data=esoph)

stripchart(ncases ~ alcgp, data=esoph)

esoph %>% 
  select(-agegp, -ncontrols) %>%
  group_by(tobgp, alcgp) %>%
  summarize(total_cases = sum(ncases)) %>%
  group_by(tobgp) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = tobgp, y = percentage, fill = alcgp)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Alcohol Consumption by Tobacco Groups", subtitle = "Data Source: `esoph`", x = "Tobacco Consumption", y = "% of Cancer Cases", fill = "Alcohol Consumption")

esoph %>% 
  select(-agegp, -ncontrols) %>%
  group_by(alcgp, tobgp) %>%
  summarize(total_cases = sum(ncases)) %>%
  group_by(alcgp) %>%
  mutate(percentage = 100 * total_cases / sum(total_cases)) %>%
  filter(percentage != "NaN" & percentage != 0) %>%
  ggplot(., aes(x = alcgp, y = percentage, fill = tobgp)) +
  geom_col(stat = "identity", position = "fill") +
  theme_minimal() +
  geom_text(aes(label = paste(format(percentage,digits=1), "%")), size=4, position = "fill", hjust = 0.5, vjust = 1.1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Stacked Bar Chart of Case Distribution of Tobacco Consumption by Alcohol Groups", subtitle = "Data Source: `esoph`", x = "Alcohol Consumption", y = "% of Cancer Cases", fill = "Tobacco Consumption")

model <- glm(cbind(ncases, ncontrols) ~ agegp + unclass(tobgp)
                                         + unclass(alcgp),
              data = esoph, family = binomial())

summary(model)
