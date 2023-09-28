##############################Tentative d'un controle synthetique ########################


library(gsynth)
require(Rcpp)
require(ggplot2)
require(GGally)
require(foreach)
require(future)
require(doParallel)
require(abind)
require(lfe)
#test

system.time(
  g<-gsynth(formula= pat_log ~ controle1 + rev_log  , D = controle1, data=panel_isf_DiD, na.rm=TRUE,index =c("unit.id", "annee"), force="two-way",weight="ponderation_longitudinal_indiv_2017_bis_2021")      
)
#pb de bis à vérifier

did_2014<-panel_isf_DiD[panel_isf_DiD$annee==2014,]
did_2017<-panel_isf_DiD[panel_isf_DiD$annee==2017,]
did_2021<-panel_isf_DiD[panel_isf_DiD$annee==2021,]
base_id<-panel_isf_DiD$identifiant_longitudinal_individuel
base_id<-as.data.frame(base_id)
unique(base_id)
sort(base_id$identifiant_longitudinal_individuel)
unique(panel)

duplicated(panel_isf_DiD$identifiant_longitudinal_individuel)

#a priori pas de probleme de doublons 

#Autre test pour passer de character à numérique

a<-panel_isf_DiD$identifiant_longitudinal_individuel
a<-as.data.frame(a)
#selectdataframe sur colonne des identifiants
b<-distinct(a)
b<-b |> mutate(unit.id=seq(1, nrow(b), 1))
setnames(a, c("a"))
c<- left_join(a,b,by="a")
c<-as.data.frame(unique(c))
setnames(c, c("identifiant_longitudinal_individuel", "unit.id"))
panel_isf_DiD<-inner_join(c, panel_isf_DiD, by=c("identifiant_longitudinal_individuel"))
panel_isf_DiD$time<-1
panel_isf_DiD$time[panel_isf_DiD$annee==2017]<-2
panel_isf_DiD$time[panel_isf_DiD$annee==2021]<-3
summary(panel_isf_DiD$time)

#Test e PanelView

library(panelView)
panelview(pat_log~tranche_age + tranche_pat_bis + controle1, data=panel_isf_DiD, index=c("unit.id","time"))

#export de la base en xls pour tester la dernière chance sur Stata

write_xlsx(panel_isf_DiD, "panel_isf_DiD.xlsx")