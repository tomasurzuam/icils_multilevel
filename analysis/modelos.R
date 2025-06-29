# ---- Resultados ----

rm(list=ls())     
options(scipen=999)

pacman::p_load(lme4, reghelper, haven, stargazer, ggplot2, texreg, dplyr,
               foreign, lattice, sjPlot, sjlabelled, skimr, influence.ME,
               patchwork, kableExtra)
library(ggeffects) # carga de librerías 

icils_final <- readRDS("input/proc_data/icils_macros.rds") # cargar base 

#base sin NA para armonizar el N de los modelos 
icils <- icils_final %>%
  na.omit()

# agregar label a las variables nivel 1


# convertir la variable de desigualdad de género a numérica 
icils$gender_ineq <- as.numeric(icils$gender_ineq)

# base agregada 
icils_lvl_2 <- icils %>% group_by(CNTRY) %>% summarise_all(funs(mean))

# agregar labels a variables nivel 2
icils_lvl_2$admin_dig <- set_label(icils_lvl_2$admin_dig, "Administración digital")
icils_lvl_2$gender_ineq <- set_label(icils_lvl_2$gender_ineq, "Desigualdad de género")
icils_lvl_2$logpib <- set_label(icils_lvl_2$logpib, "Logaritmo de PIB")
icils_lvl_2$pib_education <- set_label(icils_lvl_2$pib_education, "PIB en educación")

# centrado de variables nivel 1

icils_c <- icils %>% 
  group_by(CNTRY) %>% 
  mutate(sexo.cmc = sexo-mean(sexo),
         aprendizaje_escuela.cmc = aprendizaje_escuela-mean(aprendizaje_escuela),
         autoeffgen.cmc = autoeffgen-mean(autoeffgen),
         educ_padres.cmc = educ_padres-mean(educ_padres),
         libros_hogar.cmc = libros_hogar-mean(libros_hogar))%>% 
  ungroup()

# Etiquetar variables para visualización de la tabla

icils$sexo.cmc <- set_label(icils$sexo.cmc, "Sexo de estudiante")
icils$aprendizaje_escuela.cmc <- set_label(icils$aprendizaje_escuela.cmc, "Tareas relacionadas al aprendizaje online en la escuela")
icils$autoeffgen.cmc <- set_label(icils$autoeffgen.cmc, "Autoeficacia digital general del estudiante")
icils$alf_digital <- set_label(icils$alf_digital, "Puntaje de alfabetización digital")
icils$educ_padres.cmc <- set_label(icils$educ_padres.cmc, "Padres con título universitario")
icils$libros_hogar.cmc <- set_label(icils$libros_hogar.cmc, "Libros en el hogar")

# ---- Modelos ----

# Modelo nulo
null = lmer(alf_digital ~ 1 + (1 | CNTRY), data = icils_c)
reghelper::ICC(null)
# ICC 0.19

# Modelo nivel 1 acorde a hipótesis 
results_1 = lmer(alf_digital ~ 1 + sexo.cmc + aprendizaje_escuela.cmc + 
                 educ_padres.cmc + libros_hogar.cmc + autoeffgen.cmc + (1 | CNTRY), data = icils_c)
screenreg(results_1)

# Modelo nivel 2 
results_2 = lmer(alf_digital ~ 1 + gender_ineq + admin_dig + pib_education +
                 logpib + (1 | CNTRY), data = icils_c)
screenreg(results_2)

results_2a = lmer(alf_digital ~ 1 + pib_education +
                   logpib + (1 | CNTRY), data = icils_c)
screenreg(results_2a)

results_2a = lmer(alf_digital ~ 1 + pib_education +
                    logpib + (1 | CNTRY), data = icils_c)
screenreg(results_2a)

# Modelo completo 

results_3 = lmer(alf_digital ~ 1 + sexo.cmc + aprendizaje_escuela.cmc + 
                   educ_padres.cmc + libros_hogar.cmc + gender_ineq + 
                   admin_dig + pib_education + logpib + (1 | CNTRY), data = icils_c)

screenreg(results_3)

# Regresiones para generar la tabla 

