#Script exportations des tables econometriques en LATEX


#Le seuil est a changer dans le scipt 08. 

#En log et patrimoine

dd <- lm(data = panel_isf_DiD,part_patrimoine_financier_log ~
           an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)
dd1 <- lm(data = panel_isf_DiD,part_patrimoine_immobilier_log ~
an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd1)

panel_isf_DiD$pat_immo_log[panel_isf_DiD$pat_immo_log==-Inf]<-0
panel_isf_DiD$part_patrimoine_professionnel[panel_isf_DiD$part_patrimoine_professionnel==-Inf]<-0
panel_isf_DiD$part_patrimoine_professionnel_log[panel_isf_DiD$part_patrimoine_professionnel_log==-Inf]<-0


dd2 <- lm(data = panel_isf_DiD,part_patrimoine_professionnel_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)

dd3 <- lm(data = panel_isf_DiD,pat_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd3)


dd4 <- lm(data = panel_isf_DiD,pat_fi_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


dd5 <- lm(data = panel_isf_DiD,pat_immo_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


stargazer(dd, dd1, dd2, dd3, dd4, dd5, dep.var.labels = c("Part de patrimoine financier (en log)", "Part de patrimoine immobilier (en log)", "Patrimoine brut (en log)", "Part de patrimoine professionnel (en log)", "Patrimoine financier (en log)", "Patrimoine immobilier (en log)" ),style = "aer", digits=5, out="models.txt", covariate.labels=c("2014 X Traitement", "2021 X Traitement", "2014","2021", "Tranche d'age", "Tranche de patrimoine", "Revenu déclaré" ))



#En normalise et patrimoine


nn <- lm(data = panel_isf_DiD,part_patrimoine_financier ~
           an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn)
nn1 <- lm(data = panel_isf_DiD,part_patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn1)

nn2 <- lm(data = panel_isf_DiD,part_patrimoine_professionnel ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)

nn3 <- lm(data = panel_isf_DiD,pat_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn3)


nn4 <- lm(data = panel_isf_DiD,patrimoine_financier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


nn5 <- lm(data = panel_isf_DiD,patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


stargazer(nn, nn1,nn2, nn3, nn4,nn5, dep.var.labels = c("Part de patrimoine financier (normalisé)", "Part de patrimoine immobilier (normalisé)","Part de patrimoine professionnel (normalisé)", "Patrimoine brut (normalisé)", "Patrimoine financier (normalisé)", "Patrimoine immobilier (normalisé)" ),style = "aer", digits=5, out="models.txt", covariate.labels=c("2014 X Traitement", "2021 X Traitement", "2014","2021", "Tranche d'age", "Tranche de patrimoine", "Revenu déclaré" ))


#sans valeur extreme et patrimoine

gg <- lm(data = panel_isf_DiD_robustesse,part_patrimoine_financier ~
           an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )

gg1 <- lm(data = panel_isf_DiD_robustesse,part_patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )


gg2 <- lm(data = panel_isf_DiD_robustesse_robustesse,part_patrimoine_professionnel ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )


gg3 <- lm(data = panel_isf_DiD_robustesse,pat_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(gg1)


gg4 <- lm(data = panel_isf_DiD_robustesse,patrimoine_financier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )



gg5 <- lm(data = panel_isf_DiD_robustesse,patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )



stargazer(gg, gg1, gg2, gg3, gg4,gg5, dep.var.labels = c("Part de patrimoine financier (sans valeur extrême)", "Part de patrimoine immobilier (sans valeur extrême)", "Patrimoine brut (sans valeur extrême)", "Patrimoine financier (sans valeur extrême)", "Patrimoine immobilier (sans valeur extrême)" ),style = "aer", digits=5, out="models.txt", covariate.labels=c("2014 X Traitement", "2021 X Traitement", "2014","2021", "Tranche d'age", "Tranche de patrimoine", "Revenu déclaré" ))


#normal

nn <- lm(data = panel_isf_DiD,part_patrimoine_financier ~
           an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn)
nn1 <- lm(data = panel_isf_DiD,part_patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn1)

nn2 <- lm(data = panel_isf_DiD,part_patrimoine_professionnel ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)

nn3 <- lm(data = panel_isf_DiD,pat_log ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(nn3)


nn4 <- lm(data = panel_isf_DiD,patrimoine_financier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


nn5 <- lm(data = panel_isf_DiD,patrimoine_immobilier ~
            an2014_t + an2021_t + an2014 + an2021 + tranche_age + tranche_pat_bis + revenu_declare , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(dd)


stargazer(nn, nn1, nn2, nn3, nn4,nn5, dep.var.labels = c("Part de patrimoine financier", "Part de patrimoine immobilier", "Patrimoine brut (en log)", "Patrimoine financier", "Patrimoine immobilier" ),style = "aer", digits=5, out="models.txt", covariate.labels=c("2014 X Traitement", "2021 X Traitement", "2014","2021", "Tranche d'age", "Tranche de patrimoine", "Revenu déclaré" ))

#Autre: reg sans controles

sc <- lm(data = panel_isf_DiD,part_patrimoine_financier ~
            an2014_t + an2021_t + an2014 + an2021 , weights = ponderation_longitudinal_indiv_2017_bis_2021 )
summary(sc)
