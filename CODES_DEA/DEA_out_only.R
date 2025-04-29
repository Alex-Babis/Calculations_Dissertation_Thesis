### ANALYZA LONG pozicia
# DEA
Y <- rbind(good.agr(long1[1:3], 0.05),
           good.agr(long2.5[1:3], 0.05),
           good.agr(long5[1:3], 0.05))

Y <- t(apply(Y,1,function(z) z/max(z)))

efektivity.out.only.l <- c()
for(j in 1:28)
{
  print(j)
  efektivity.out.only.l <- rbind(efektivity.out.only.l,RusselDEA.out.only(j))
}

rownames(efektivity.out.only.l) <- colnames(Y)

# for(i in 1:4)
# {
  
#   efekt.df <- data.frame(modely = colnames(Y),
#                          efektivity = efektivity.out.only.l[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(colnames(efektivity.out.only.l)[i]) 
  
  
#   print(g)
# }

# efektvine vzory
eff.image.l <- c()
for(j in 1:28)
{
  print(j)
  eff.image.l <- cbind(eff.image.l,Y%*%efektivity.out.only.l[j,5:32])
}
rownames(eff.image.l) <- c("stat_1","stat_2.5","stat_5")
colnames(eff.image.l) <- colnames(Y)

# surplas
surplas.out.only.l <- eff.image.l - Y
rownames(surplas.out.only.l) <- c("stat_1","stat_2.5","stat_5")
colnames(surplas.out.only.l) <- colnames(Y)

write.csv(surplas.out.only.l,"surplas")


which(round(efektivity.out.only.l[,1],digits = 5) == 1)

# superefektivita pre efektivne 
seff.out.only.l <- c()
for(j in c(9,23,26,28))
{
  print(j)
  seff.out.only.l <- rbind(seff.out.only.l,supeff.out.only(j))
}

rownames(seff.out.only.l) <- colnames(Y)[c(9,23)]


# kolko krat boli sucast ref mnoziny
cnt.ref.out.only.l <- apply(efektivity.out.only.l[,5:32], 2, function(x) sum(x != 0))
names(cnt.ref.out.only.l) <- colnames(Y)
cnt.ref.out.only.l <- cnt.ref.out.only.l[c(9,23,26,28)]

write.csv(cbind(seff.out.only.l,cnt.ref.out.only.l),"super_eff")

##########################################################################################
### ANALYZA SHORT pozicia

Y <- rbind(good.agr(short1[1:3], 0.05),
           good.agr(short2.5[1:3], 0.05),
           good.agr(short5[1:3], 0.05))


Y <- t(apply(Y,1,function(z) z/max(z)))


efektivity.out.only.s <- c()
for(j in 1:28)
{
  print(j)
  efektivity.out.only.s <- rbind(efektivity.out.only.s,RusselDEA.out.only(j))
}

rownames(efektivity.out.only.s) <- colnames(Y)


# for(i in 1:4)
# {
  
#   efekt.df <- data.frame(modely = colnames(Y),
#                          efektivity = efektivity.out.only.s[,i])
  
#   # vykreslenie Hyperbolic vs Russel 
#   g <-  ggplot(data=efekt.df, aes(x=modely, y=efektivity)) +
#     geom_bar(stat="identity", color="black", position=position_dodge())+
#     theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1, size = 15),
#           axis.text.y = element_text(size = 22),
#           axis.title.x = element_text(size = 22),
#           axis.title.y = element_text(size = 22),
#           legend.text = element_text(size=20))+
#     ggtitle(colnames(efektivity.out.only.s)[i]) 
  
  
#   print(g)
# }

# efektvine vzory
eff.image.s <- c()
for(j in 1:28)
{
  print(j)
  eff.image.s <- cbind(eff.image.s,Y%*%efektivity.out.only.s[j,5:32])
}
rownames(eff.image.s) <- c("stat_1","stat_2.5","stat_5")
colnames(eff.image.s) <- colnames(Y)


# surplas
surplas.out.only.s <- eff.image.s - Y
rownames(surplas.out.only.s) <- c("stat_1","stat_2.5","stat_5")
colnames(surplas.out.only.s) <- colnames(Y)

write.csv(surplas.out.only.s,"surplas")


which(round(efektivity.out.only.s[,1],digits = 5) == 1)

# superefektivita pre efektivne 
seff.out.only.s <- c()
for(j in c(1,8,12,20,22,27))
{
  print(j)
  seff.out.only.s <- rbind(seff.out.only.s,supeff.out.only(j))
}

rownames(seff.out.only.s) <- colnames(Y)[c(1,8,12,20,22,27)]

# kolko krat boli sucast ref mnoziny
cnt.ref.out.only.s <- apply(efektivity.out.only.s[,5:32], 2, function(x) sum(x != 0))
names(cnt.ref.out.only.s) <- colnames(Y)
cnt.ref.out.only.s <- cnt.ref.out.only.s[c(1,8,12,20,22,27)]

write.csv(cbind(seff.out.only.s,cnt.ref.out.only.s),"super_eff")


#### LATEX TABLES
library(xtable)
library(Hmisc)
#long-table
long.tab <- efektivity.out.only.l[,1:4]
long.sup <- as.data.frame(cbind(seff.out.only.l,cnt.ref.out.only.l))


long.tab <- cbind(signif(long.tab, digits = 3),signif(long.sup, digits = 4)[match(rownames(long.tab),
                                                                                     rownames(long.sup)),]) 
colnames(long.tab) <- c("Eff","Parc1","Parc2","Parc3",
                        "Supeff","Supparc1","Supparc1","Supparc1",
                        "#image")

rownames(long.tab) <- 1:28

long.tab <- long.tab[c(c(1,8,15,22),
                         3 + c(1,8,15,22),
                         4 + c(1,8,15,22),
                         5 + c(1,8,15,22),
                         2 + c(1,8,15,22),
                         6 + c(1,8,15,22),
                         1 + c(1,8,15,22)),]
rownames(long.tab) <- 1:28


#short-table
short.tab <- efektivity.out.only.s[,1:4]
short.sup <- as.data.frame(cbind(seff.out.only.s,cnt.ref.out.only.s))


short.tab <- cbind(signif(short.tab, digits = 3),signif(short.sup, digits = 4)[match(rownames(short.tab),
                                                                                     rownames(short.sup)),]) 
colnames(short.tab) <- c("Eff","Parc1","Parc2","Parc3",
                        "Supeff","Supparc1","Supparc1","Supparc1",
                        "#image")

#print(xtable(short.tab, type = "latex"), file = "Dea_short_out.tex")

rownames(short.tab) <- 1:28

short.tab <- short.tab[c(c(1,8,15,22),
                         3 + c(1,8,15,22),
                         4 + c(1,8,15,22),
                         5 + c(1,8,15,22),
                         2 + c(1,8,15,22),
                         6 + c(1,8,15,22),
                         1 + c(1,8,15,22)),]
rownames(short.tab) <- 1:28


out.tab <- cbind(long.tab,short.tab) 

latex(out.tab, file="Dea_out.tex",cgroup = c("Long position", "Short position"),
      n.cgroup = c(9,9),
      rgroup = c("GARCH", "iGARCH","fiGARCH","csGARCH","eGARCH","tGARCH","gjrGARCH"),
      n.rgroup = c(4,4,4,4,4,4,4),
      rowname = rep(c("Norm","sNorm","Std","sStd"),7),
      landscape = TRUE, na.blank = FALSE, na = 2,booktabs=TRUE,
      colheads = rep(c("Eff","@ 1%","@ 2.5%","@ 5%","SEff","@ 1%",
                       "@ 2.5%","@ 5%","#EI"),2))

