library(tidyverse)
pp <- 1000
n <- 500
cas_p <- 0.0005
con_p <- 0.0001
clu_size <- 100
clu_start <- 100

con <- rbinom(pp,n, con_p)
cas <- rbinom(pp,n, con_p)
cas[clu_start:(clu_start+clu_size-1)] <- rbinom(clu_size, n, cas_p)
dd <- data.frame("cases"=cas, "controls"=con, "position"=1:pp)

example_plot <- dd %>%
  mutate("cases"=cumsum(cases)/sum(cases), "controls"=cumsum(controls)/sum(controls)) %>%
  gather("Group", "value", -position) %>%
  ggplot(., aes(y=value, x=position, color=Group)) +
  geom_step() +
  ylab("Cumulative Probability") +
  theme_classic()

#ggsave("./plots/ks_example.pdf", example_plot)

dd %>%
  sample_n(100) %>%
  ggplot(., aes(x=position, y=controls)) + geom_rug()
