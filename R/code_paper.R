############# script for paper ################

library(tidyverse)
library(DemoKin)
library(DemoTools)
library(kableExtra)
library(quadprog)
source("R/QOsplit.R") # split fertility 5-y groups in single

# read mort rates by group
mx <- readxl::read_xlsx("data/tasas.xlsx", sheet = "mx")

# plot rates
plot_mort_rates <- mx %>% 
     pivot_longer(Ind:NoMestizo) %>% 
     mutate(Sexo = ifelse(Sexo == "Hombre", "Male", "Female")) %>% 
     mutate(name = ifelse(name == "Ind", "Indigenous", ifelse(name == "NoMestizo", "White-Mestizos", name))) %>% 
     ggplot(aes(Edad, value, col = name)) +
     geom_step(size = 1) +
     scale_y_log10() +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10), 
                        expand = c(0, 0), limits = c(0, 85)) +
     theme_bw() +
     labs(color = "Group", x = "Age", y = "m(x)") +
     facet_wrap(~Sexo)
ggsave(plot = plot_mort_rates, filename = "plots/plot_mort_rates_colombia_2018.pdf")

# extend and graduate to single ages mortality rates
method_law <- "kannisto"
mx <- data.frame(Edad_orig = 0:85) %>% 
     mutate(Edad = ifelse(Edad_orig == 0, 0, ifelse(Edad_orig %in% 1:4, 1, trunc(Edad_orig/5)*5))) %>% 
     left_join(mx)
mx_ind_hombre    <- lt_single_mx(nMx = mx$Ind[mx$Sexo=="Hombre"], mx$Edad_orig[mx$Sexo=="Hombre"], 
                              a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)
mx_ind_mujer     <- lt_single_mx(nMx = mx$Ind[mx$Sexo=="Mujer"], mx$Edad_orig[mx$Sexo=="Mujer"], 
                             a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)
mx_afro_hombre   <- lt_single_mx(nMx = mx$Afro[mx$Sexo=="Hombre"], mx$Edad_orig[mx$Sexo=="Hombre"], 
                               a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)
mx_afro_mujer    <- lt_single_mx(nMx = mx$Afro[mx$Sexo=="Mujer"], mx$Edad_orig[mx$Sexo=="Mujer"], 
                              a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)
mx_nomest_hombre <- lt_single_mx(nMx = mx$NoMestizo[mx$Sexo=="Hombre"], mx$Edad_orig[mx$Sexo=="Hombre"], 
                                 a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)
mx_nomest_mujer  <- lt_single_mx(nMx = mx$NoMestizo[mx$Sexo=="Mujer"], mx$Edad_orig[mx$Sexo=="Mujer"], 
                                a0rule = "cd", Sex = "m", OAnew = 100, extrapLaw = method_law)

# get life tables together, and check e(0)
lts <- bind_rows(mx_ind_hombre %>% mutate(Sexo = "Male", Grupo = "Indigenous"),
                 mx_ind_mujer %>% mutate(Sexo = "Female", Grupo = "Indigenous"),
                 mx_afro_hombre %>% mutate(Sexo = "Male", Grupo = "Afro"),
                 mx_afro_mujer %>% mutate(Sexo = "Female", Grupo = "Afro"),
                 mx_nomest_hombre %>% mutate(Sexo = "Male", Grupo = "White-Mestizos"),
                 mx_nomest_mujer %>% mutate(Sexo = "Female", Grupo = "White-Mestizos"))
lts %>% filter(Age == 0) %>% select(Grupo, Sexo, ex)
lts %>% 
     ggplot(aes(Age, lx, col = Grupo)) +
     geom_line(size = 1) +
     theme_bw() +
     facet_wrap(~Sexo)

# read asfr
asfr <- readxl::read_xlsx("data/tasas.xlsx", sheet = "asfr")
plot_fert_rates <- asfr %>% 
     pivot_longer(Ind:NoMestizo) %>% 
     mutate(name = ifelse(name == "Ind", "Indigenous", ifelse(name == "NoMestizo", "White-Mestizos", name))) %>% 
     ggplot(aes(Edad, value, col = name))+ 
     geom_step(size = 1) +
     labs(y = "asfr", col = "Group", x = "Age") +
     theme_bw()
ggsave(plot = plot_fert_rates, filename = "plots/plot_fert_rates_colombia_2018.pdf")

# check level and mac
colSums(asfr[,-1]) * 5
colSums(asfr[,-1] * seq(12.5, 47.5, 5))/colSums(asfr[,-1])

# graduate asfr to single age
asfr_ind <- QOSplit(asfr$Ind, asfr$Edad, rep(5,8))
asfr_afro <- QOSplit(asfr$Afro, asfr$Edad, rep(5,8))
asfr_white <- QOSplit(asfr$NoMestizo, asfr$Edad, rep(5,8))
asfrs <- bind_rows(asfr_ind %>% mutate(Grupo = "Indigenous"),
                   asfr_afro %>% mutate(Grupo = "Afro"),
                   asfr_white %>% mutate(Grupo = "White-Mestizos"))
asfrs %>% 
     ggplot(aes(Age, ASFR, col=Grupo))+
     geom_line() + 
     theme_bw()

# buil kin network
kin_ind <- kin2sex(pf = 1-lts$nqx[lts$Sexo == "Female" & lts$Grupo == "Indigenous"], 
                   pm = 1-lts$nqx[lts$Sexo == "Male" & lts$Grupo == "Indigenous"], 
                   ff = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "Indigenous"], rep(0, 51)),
                   fm = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "Indigenous"], rep(0, 51)), 
                   birth_female = .5)
kin_afro <- kin2sex(pf = 1-lts$nqx[lts$Sexo == "Female" & lts$Grupo == "Afro"], 
                   pm = 1-lts$nqx[lts$Sexo == "Male" & lts$Grupo == "Afro"], 
                   ff = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "Afro"], rep(0, 51)),
                   fm = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "Afro"], rep(0, 51)),
                   birth_female = .5)
kin_nomest <- kin2sex(pf = 1-lts$nqx[lts$Sexo == "Female" & lts$Grupo == "White-Mestizos"], 
                    pm = 1-lts$nqx[lts$Sexo == "Male" & lts$Grupo == "White-Mestizos"], 
                    ff = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "White-Mestizos"], rep(0, 51)),
                    fm = c(rep(0, 10), asfrs$ASFR[asfrs$Grupo == "White-Mestizos"], rep(0, 51)),
                    birth_female = .5)

# diagrams
plot_diagram(kin_ind$kin_summary %>% filter(age_focal == 30) %>% 
                  summarise(count = sum(count_living), .by = kin))
plot_diagram(kin_afro$kin_summary %>% filter(age_focal == 30) %>% 
                  summarise(count = sum(count_living), .by = kin))
plot_diagram(kin_nomest$kin_summary %>% filter(age_focal == 30) %>% 
                  summarise(count = sum(count_living), .by = kin))

# living child
living_child_colombia_2018 <- 
     bind_rows(kin_ind$kin_full %>% mutate(Grupo = "Indigenous"),
               kin_afro$kin_full %>% mutate(Grupo = "Afro"),
               kin_nomest$kin_full %>% mutate(Grupo = "White-Mestizos")) %>% 
     filter(kin == "d") %>%
     mutate(sex_kin = ifelse(sex_kin == "f", "Female", "Male")) %>% 
     summarise(living = sum(living), .by = c(Grupo, age_focal, sex_kin, age_kin)) %>% 
     left_join(lts %>% select(sex_kin = Sexo, Grupo, age_kin = Age, qx = nqx))

# plot living child
plot_living_child_colombia_2018 <- living_child_colombia_2018 %>%
     summarise(living = sum(living, na.rm =T), .by = c(Grupo, age_focal)) %>% 
     ggplot(aes(age_focal, living, col=Grupo)) +
     geom_line() +
     labs(y = "Living child", x = "Age Focal woman", col = "Group") +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10), 
                        expand = c(0, 0), limits = c(15, 90)) +
     theme_bw()
ggsave(plot = plot_living_child_colombia_2018, filename = "plots/plot_living_child_colombia_2018.pdf")

# death child
death_child_colombia_2018 <-  bind_rows(
          kin_ind$kin_full %>% mutate(Grupo = "Indigenous"),
          kin_afro$kin_full %>% mutate(Grupo = "Afro"),
          kin_nomest$kin_full %>% mutate(Grupo = "White-Mestizos")) %>% 
     mutate(sex_kin = ifelse(sex_kin == "f", "Female", "Male")) %>% 
     filter(kin == "d") %>%
     summarise(dead_child = sum(dead), 
               ma_dead_child = sum(dead * age_kin, na.rm = T) / sum(dead, na.rm = T),
               .by = c(Grupo, sex_kin, age_focal)) 

# plot death child
plot_death_child_colombia_2018 <- death_child_colombia_2018 %>% 
     ggplot(aes(age_focal, dead_child, col=Grupo)) +
     geom_line() +
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10), 
                        expand = c(0, 0), limits = c(15, 90)) +
     scale_y_continuous(limits = c(0, .06)) +
     labs(y = "Mean age of child dead", x = "Age Focal woman", col = "Group") + 
     theme_bw() + 
     facet_wrap(~sex_kin) +
     theme()
ggsave(plot = plot_death_child_colombia_2018, filename = "plots/plot_death_child_colombia_2018.pdf")

# plot mean age death
plot_mean_age_death_child_colombia_2018 <- death_child_colombia_2018 %>% 
     ggplot(aes(age_focal, ma_dead_child, col=Grupo)) +
     geom_line()+
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10), 
                        expand = c(0, 0), limits = c(15, 90)) +
     scale_y_continuous(limits = c(0, 65)) +
     labs(y = "Mean age at death", x = "Age Focal woman", col = "Group") +
     theme_bw() +
     facet_wrap(~sex_kin) +
     theme()
ggsave(plot = plot_mean_age_death_child_colombia_2018, filename = "plots/plot_mean_age_death_child_colombia_2018.pdf")

# weighted probability of lossing a child in next age
prob_childloss_colombia_2018 <- living_child_colombia_2018 %>% 
     summarise(prob_loss_child = sum(living * qx, na.rm = T)/sum(living, na.rm = T), 
               .by = c(Grupo, age_focal, sex_kin))

# plot prob loss
plot_prob_childloss_colombia_2018 <- prob_childloss_colombia_2018 %>% 
     ggplot(aes(age_focal, prob_loss_child, col=Grupo)) +
     geom_line() +
     labs(y = "Probability of lossing a child", x = "Age Focal woman", col = "Group") + 
     scale_x_continuous(labels = seq(0,100,10), breaks = seq(0,100,10), 
                        expand = c(0, 0), limits = c(15, 90)) +
     scale_y_continuous(limits = c(0, .035)) +
     scale_y_log10()+
     theme_bw() +
     facet_wrap(~sex_kin)
ggsave(plot = plot_prob_childloss_colombia_2018, filename = "plots/plot_prob_childloss_colombia_2018.pdf")

# results table
table1 <- living_child_colombia_2018 %>% 
     summarise(living = sum(living), .by = c(Grupo, age_focal, sex_kin)) %>% 
     left_join(
          death_child_colombia_2018 %>% 
               select(Grupo, sex_kin, age_focal, ma_dead_child)
          ) %>% 
     left_join(
          prob_childloss_colombia_2018 %>% 
               select(Grupo, sex_kin, age_focal, prob_loss_child)
          ) %>% 
     filter(age_focal %in% seq(15, 90,5)) %>%
     pivot_longer(living:prob_loss_child) %>% 
     pivot_wider(names_from = c(Grupo, name), values_from = value) %>% 
     select(2, 1, 3:11) %>% 
     as.data.frame()

# table in latex to copy and paste
table1 %>% 
     select(-sex_kin) %>% 
     kbl(booktabs=TRUE, escape=FALSE, 
         col.names = c("Age", rep(c("l", "mad", "c"), 3)),
         format = "latex", digits = 3) %>% 
     pack_rows("Female", 1, 16) %>%
     pack_rows("Male", 17, 32) %>%
     add_header_above(c(" " = 1, 
                        "Indigenous" = 3, "Afro" = 3, "White-Mestizos" = 3))

# end


