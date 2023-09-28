#####################################################################################
##############################II.CREATION BASES #####################################
#####################################################################################

#I. Base ménages 2014


menage2014_brut <- read_csv("menage2014.csv", col_types =c(IDENT="character"))



#View(menage2014_brut)

#II. Base ménages 2017

menage2017_brut <- read_csv("mesnages2017.csv", col_types =c(IDENT="character"))

#III. Base individus 2014

individu2014_brut <- read_csv("individu2014.csv",  col_types =c(IDENTINDL="character", IDENT="character", IDENTIND14="character"))


#IV. Base individus 2017

individu2017_brut <- read_csv("individu2017.csv", col_types =c(IDENTINDL="character", IDENT="character", IDENTIND17="character"))

# V. Base produits 2014

produit2014_brut<-read_csv("produit2014.csv", col_types =c(IDENT="character", IDENTPOS="character", IDENTPROD="character"))

# VI. Base produits 2017

produit2017_brut <- read_csv("produits2017.csv", col_types =c(IDENT="character", IDENTPOS="character", IDENTPROD="character", IDENTLOG="character"))

# VII. Base menages 2021

menage2021_brut <- read_csv("menages2021.csv", col_types =c(IDENT="character"))


# VIII. Base individus 2021

individu2021_brut<- read_csv("individu2021.csv",  col_types =c(IDENTINDL="character", IDENT="character", IDENTIND20="character"))


# IX. Base produits 2021

produit2021_brut<- read_csv("produits2021.csv",  col_types =c(IDENT="character", IDENTPOS="character", IDENTPROD="character", IDENTLOG="character"))

# IX. Base entreprises 2014

entreprise2014_brut<- read_csv("entreprises2014.csv",  col_types =c(IDENT="character"))

# X. Base entreprises 2017

entreprise2017_brut<- read_csv("entreprises2017.csv",  col_types =c(IDENT="character"))

# XI. Base entreprises 2021

entreprise2021_brut<- read_csv("entreprises2021.csv",  col_types =c(IDENT="character"))

