# national wpp ------------------------------------------------------------


# add qx to living child
child_colombia_2018 <- kin_colombia_2018$kin_full %>% 
     filter(kin == "d") %>%
     summarise(living = sum(living), .by = c(age_focal, sex_kin, age_kin)) %>% 
     left_join(bind_rows(qx_females %>% mutate(sex_kin = "f"),
                         qx_males %>% mutate(sex_kin = "m")))

# wieghted probability of lossing a child in next age
prob_childloss_colombia_2018 <- child_colombia_2018 %>% 
     summarise(prob_loss_child = sum(living * qx)/sum(living), 
               .by = c(age_focal, sex_kin))

# plot prob loss
prob_childloss_colombia_2018 %>% 
     ggplot(aes(age_focal, prob_loss_child, col=sex_kin)) +
     geom_line() +
     labs(y = "Probability of lossing a child", x = "Age Focal woman", 
          title = "Probability of losing a child by sex. A theoretical woman in Colombia 2018",
          subtitle = "Rates from UN-WPP2022") +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10)) +
     theme_bw()

# plot child
kin_colombia_2018$kin_full %>% 
     filter(kin == "d") %>%
     summarise(living = sum(living), .by = c(age_focal, sex_kin)) %>% 
     ggplot(aes(age_focal, living, col=sex_kin)) +
     geom_line() +
     labs(y = "Living child", x = "Age Focal woman", 
          title = "Living child by sex. A theoretical woman in Colombia 2018",
          subtitle = "Rates from UN-WPP2022") +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10)) +
     theme_bw()

# data from UN api
base_url <- 'https://population.un.org/dataportalapi/api/v1'
target <- paste0(base_url,'/indicators/?format=csv')
codes <- read.csv(target, sep='|', skip=1); # View(codes)
target <- paste0(base_url,'/data/indicators/',68,'/locations/',170,'/start/',2018,'/end/',2018, '/?format=csv')
asfr <- read.csv(target, sep='|', skip=1) 
asfr <- c(rep(0,15),asfr$Value, rep(0,51))
target <- paste0(base_url,'/data/indicators/',82,'/locations/',170,'/start/',2018,'/end/',2018, '/?format=csv')
qx <- read.csv(target, sep='|', skip=1) 
qx_males <- qx %>% filter(SexId==1) %>% select(age_kin = AgeStart, qx = Value)
qx_females <- qx %>% filter(SexId==2) %>% select(age_kin = AgeStart, qx = Value)
length(0:100);nrow(qx_males);length(asfr)

# kin network
kin_colombia_2018 <- kin2sex(pf = 1-qx_females$qx, pm = 1-qx_males$qx, 
                             ff = asfr/1000, fm = asfr/1000, birth_female = .5)

# add qx to living child
child_colombia_2018 <- kin_colombia_2018$kin_full %>% 
     filter(kin == "d") %>%
     summarise(living = sum(living), .by = c(age_focal, sex_kin, age_kin)) %>% 
     left_join(bind_rows(qx_females %>% mutate(sex_kin = "f"),
                         qx_males %>% mutate(sex_kin = "m")))

# wieghted probability of lossing a child in next age
prob_childloss_colombia_2018 <- child_colombia_2018 %>% 
     summarise(prob_loss_child = sum(living * qx)/sum(living), 
               .by = c(age_focal, sex_kin))

# plot prob loss
prob_childloss_colombia_2018 %>% 
     ggplot(aes(age_focal, prob_loss_child, col=sex_kin)) +
     geom_line() +
     labs(y = "Probability of lossing a child", x = "Age Focal woman", 
          title = "Probability of losing a child by sex. A theoretical woman in Colombia 2018",
          subtitle = "Rates from UN-WPP2022") +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10)) +
     theme_bw()

# plot child
kin_colombia_2018$kin_full %>% 
     filter(kin == "d") %>%
     summarise(living = sum(living), .by = c(age_focal, sex_kin)) %>% 
     ggplot(aes(age_focal, living, col=sex_kin)) +
     geom_line() +
     labs(y = "Living child", x = "Age Focal woman", 
          title = "Living child by sex. A theoretical woman in Colombia 2018",
          subtitle = "Rates from UN-WPP2022") +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10)) +
     theme_bw()