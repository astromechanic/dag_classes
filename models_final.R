#Final script
library(mlogit) #for mlogit()
library(effects) #for allEffects plot
library(arm) #for bayesglm()
library(tidyverse)


str(full)


#------------------------------------------Andi-----
#create Andi subset
and <- subset(full, lang == "and" & class != "NA")
and <- subset(and, class != "PL")
str(and)
and$class <- factor(and$class)
levels(and$class)

#Andi semantics
and3 <- subset(and, class == "3")
x <- table(and3$semantics_KK)
and4 <- subset(and, class == "4")
y <- table(and4$semantics_KK)
and5 <- subset(and, class == "5")
w <- table(and5$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Andi semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        cex.names=0.8)

#Andi consonants
and3 <- subset(and, class == "3")
and4 <- subset(and, class == "4")
and5 <- subset(and, class == "5")
w <- table(and3$cons_type)
x <- table(and4$cons_type)
y <- table(and5$cons_type)
cons <- rbind(w, x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency",
        main = "Andi initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(and5$cons_type), 
        args.legend = list("dodge", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#drop 3 class - model contains only class 4 and 5
and.no.third <- subset(and, class != "3")
and.no.third$class <- factor(and.no.third$class)
levels(and.no.third$class)
and.no.third$semantics_KK <- droplevels(and.no.third$semantics_KK)
levels(and.no.third$semantics_KK)


summary(model.and.no.third <- bayesglm(class ~ cons_type + semantics_KK,
                                family = binomial, data = and.no.third))
drop1(model.and.no.third, test="Chisq") #drop semantics

summary(model.and.no.third1 <- bayesglm(class ~ cons_type,
                                       family = binomial, data = and.no.third))
BIC(model.and.no.third, model.and.no.third1) #2nd model is not worse

#final model - model.and.no.third1
#significant features
#anterior 0.00478 (intercept)
#labial 0.01990
#liquid 0.00623
plot(allEffects(model.and.no.third1), main = "Andi, Effect plot: Class IV and Class V",
     xlab = "consonant type")

#predict cl_sg on class
summary(model.and.cl <- bayesglm(cl_sg ~ class,family = binomial, data = and.no.third))

plot(allEffects(model.and.cl)) #чот не очень убедительно и я пока не понимаю


#------------------------------------------Cham------
cham <- subset(full, lang == "cham")
cham <- subset(cham, class != "NA")
cham <- subset(cham, class != "PL")
cham$class <- factor(cham$class)

#визуализируем семантику 3 4 5 классов
cham3 <- subset(cham, class == "3")
x <- table(cham3$sem_modif)
cham4 <- subset(cham, class == "4")
y <- table(cham4$sem_modif)
cham5 <- subset(cham, class == "5")
w <- table(cham5$sem_modif)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency",
        main = "Chamalal (N.Gakvari) semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        cex.names=0.8)

# жвотни - в основном 3 класс, но 3 класс - не только животные
#визуализируем начальный согласный
cham3 <- subset(cham, class == "3")
cham4 <- subset(cham, class == "4")
cham5 <- subset(cham, class == "5")
x <- table(cham3$cons_type)
y <- table(cham4$cons_type)
z <- table(cham5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Chamalal (N. Gakvari) initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(cham5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#вроде как b- имеет тенденцию к 3 классу (b- КП 3 кл) - мб значимо отличается от 5

#визуализиуем начальный сегмент БЕЗ ЖИВОТНЫХ- нет никакой разницы по сравнению с графиком выше
#исключить животных- из-за мультиколлинеарности
cham.mod <- mlogit.data(cham, shape = "wide", choice = "class")
summary(m.cham.mod <- mlogit(class ~ 1|sem_modif + cons_type, data = cham.mod, 
                         reflevel = 1))


cham.no.anim <- subset(cham, anim == 0)
cham.no.anim$semantics_KK <- droplevels(cham.no.anim$semantics_KK)

cham1 <- mlogit.data(cham.no.anim, shape = "wide", choice = "class")
summary(m.cham <- mlogit(class ~ 1|semantics_KK + cons_type, data = cham1, 
                         reflevel = 1))

summary(m.cham2 <- mlogit(class ~ 1|cons_type, data = cham1, 
                          reflevel = 1))
waldtest(m.cham, m.cham2) #исключение из модели семантики не сделало лучше

#final model - m.cham
#significant features
#5:time 0.045778  - в 5 классе больше слов, по сравнению с 3 классом
# 4:labial 0.002856 - значимо больше в 3, чем в 4
#5: labial: 0.036374 - значимо больше в 3, чем в 5

#ref_level 4 class:
cham3 <- mlogit.data(cham.no.anim, shape = "wide", choice = "class")
summary(m.cham3 <- mlogit(class ~ 1|semantics_KK + cons_type, data = cham3, 
                         reflevel = 2))
# 3:labial 0.002856 **

#----------------------------CHAMG----------------------
chamg <- subset(full, lang == "chamg")
chamg <- subset(chamg, class != "NA")
chamg <- subset(chamg, class != "PL")
chamg$class <- factor(chamg$class)

#визуализируем семантику 3 4 5 классов
chamg3 <- subset(chamg, class == "3")
x <- table(chamg3$sem_modif)
chamg4 <- subset(chamg, class == "4")
y <- table(chamg4$sem_modif)
chamg5 <- subset(chamg, class == "5")
w <- table(chamg5$sem_modif)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency",
        main = "Chamalal (Gigatli) semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        cex.names=0.8)

# жвотни - в основном 3 класс, но 3 класс - не только животные - так же, как в cham

#визуализируем начальный согласный
chamg3 <- subset(chamg, class == "3")
chamg4 <- subset(chamg, class == "4")
chamg5 <- subset(chamg, class == "5")
x <- table(chamg3$cons_type)
y <- table(chamg4$cons_type)
z <- table(chamg5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Chamalal (Gigatli) initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(cham5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#визуализируем начальный согласных без животных
str(chamg)
chamg.no.anim <- subset(chamg, semantics_KK != "dom.anim")
chamg.no.anim <- subset(chamg.no.anim, semantics_KK != "wild.anim")
chamg.no.anim$semantics_KK <- factor(chamg.no.anim$semantics_KK)
str(chamg.no.anim)

chamg3 <- subset(chamg.no.anim, class == "3")
chamg4 <- subset(chamg.no.anim, class == "4")
chamg5 <- subset(chamg.no.anim, class == "5")
x <- table(chamg3$cons_type)
y <- table(chamg4$cons_type)
z <- table(chamg5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Chamalal (Gigatli) initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(chamg5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#исключим животных чтобы избежать мультиколлинеарности
chamg <- subset(chamg, semantics_KK != "dom.anim")
chamg <- subset(chamg, semantics_KK != "wild.anim")
chamg$semantics_KK <- factor(chamg$semantics_KK)

chamg1 <- mlogit.data(chamg, shape = "wide", choice = "class")
summary(m.chamg <- mlogit(class ~ 1|semantics_KK + cons_type, data = chamg1, 
                          reflevel = 1))

summary(m.chamg2 <- mlogit(class ~ 1|cons_type, data = chamg1, 
                          reflevel = 1))
waldtest(m.chamg, m.chamg2) #удаление семантики не делает лучше

#final model m.chamg
#4:cons_typelabial         -1.649943   0.478461 -3.4484 0.0005638 ***
#5:cons_typelabial         -1.497342   0.534694 -2.8004 0.0051044 ** 
#4:cons_typeliquid          1.730059   0.439207  3.9391  8.18e-05 ***

#ref_level 4 class
chamg4 <- mlogit.data(chamg, shape = "wide", choice = "class")
summary(m.chamg4 <- mlogit(class ~ 1|semantics_KK + cons_type, data = chamg4, 
                          reflevel = 2))
#5:semantics_KKsubst       -2.6904171  1.2580701 -2.1385 0.0324740 *  
#3:cons_typelabial          1.6499428  0.4784606  3.4484 0.0005638 ***
#3:cons_typeliquid         -1.7300590  0.4392071 -3.9391  8.18e-05 ***
#5:cons_typeliquid         -1.0109268  0.4776701 -2.1164 0.0343133 * 


#--------------------------------------------ИНХОКВАРИНСКИЙ-----------
inh <- subset(full, lang == "inh" & class != "NA")
inh <- subset(inh, class != "PL")
inh$class <- factor(inh$class)

#визуализируем семантику 3 4 5 классов
inh3 <- subset(inh, class == "3")
x <- table(inh3$semantics_KK)
inh4 <- subset(inh, class == "4")
y <- table(inh4$semantics_KK)
inh5 <- subset(inh, class == "5")
w <- table(inh5$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Khwarshi semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(and3$semantics_KK), cex.names=0.8)

#визуализируем начальный согласный
inh3 <- subset(inh, class == "3")
inh4 <- subset(inh, class == "4")
inh5 <- subset(inh, class == "5")
x <- table(inh3$cons_type)
y <- table(inh4$cons_type)
z <- table(inh5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Khwarshi consonant initial", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(inh5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)


#3 класс - самый большой, но также он как правило сильно отличается от 5 класса

inh1 <- mlogit.data(inh, shape = "wide", choice = "class")
summary(m.inh <- mlogit(class ~ 1|semantics_KK + cons_type, data = inh1, 
                        reflevel = 1))

#удалим диких животных
inh.no.wild <- subset(inh, semantics_KK != "wild.anim")
inh.no.wild$semantics_KK <-  factor(inh.no.wild$semantics_KK)

inh2 <- mlogit.data(inh.no.wild, shape = "wide", choice = "class")
summary(m.inh2 <- mlogit(class ~ 1|semantics_KK + cons_type, data = inh2, 
                        reflevel = 2))
#ref_level 3 class
#significant
#4:semantics_KKdom.anim    -3.521206   1.194731 -2.9473  0.003206 **
#4:semantics_KKgeo.astro   -1.696468   0.841654 -2.0156  0.043838 * 
#4:cons_typelabial         -1.928798   0.469950 -4.1043 4.056e-05 ***
#5:cons_typelabial         -1.213678   0.441741 -2.7475  0.006005 ** 

#ref_level 4 class
#5:semantics_KKdom.anim     3.5838743  1.2738968  2.8133  0.004903 **
#5:semantics_KKtime         2.5140980  1.1400992  2.2052  0.027443 *
#5:cons_typeliquid         -1.5932502  0.6021725 -2.6458  0.008149 ** 


#-----------------------------------------БЕЖТИНСКИЙ (Тлядал)------------
bezht <- subset(full, lang == "bezht")
bezht <- subset(bezht, class != "PL")
bezht <- subset(bezht, class != "NA")
bezht$class <- factor(bezht$class)

#визуализируем семантику 3 4 5 классов
bezht3 <- subset(bezht, class == "3")
x <- table(bezht3$semantics_KK)
bezht4 <- subset(bezht, class == "4")
y <- table(bezht4$semantics_KK)
bezht5 <- subset(bezht, class == "5")
w <- table(bezht5$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Bezhta (Tlyadal) semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(bezht3$semantics_KK), cex.names=0.8)

#визуализируем начальный согласный #без животных
bezht.no <- subset(bezht, semantics_KK != "dom.anim")
bezht.no <- subset(bezht.no, semantics_KK != "wild.anim")
bezht.no$semantics_KK <- factor(bezht.no$semantics_KK)

bezht3 <- subset(bezht.no, class == "3")
bezht4 <- subset(bezht.no, class == "4")
bezht5 <- subset(bezht.no, class == "5")
x <- table(bezht3$cons_type)
y <- table(bezht4$cons_type)
z <- table(bezht5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Bezhta (Tlyadal) initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(bezht5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#модель без животных, уровень - 3 класс
bezht1 <- mlogit.data(bezht.no, shape = "wide", choice = "class")
summary(m.bezht <- mlogit(class ~ 1|semantics_KK + cons_type, data = bezht1, 
                          reflevel = 2)) 
#ref_level 3 class
#4:semantics_KKhomeware    -2.5180e+00  7.8915e-01 -3.1908 0.001419 **
#4:cons_typelabial         -1.0248e+00  3.4325e-01 -2.9857 0.002830 **
#5:cons_typelabial         -1.3540e+00  5.1659e-01 -2.6211 0.008763 **
#5:cons_typeNA              1.1027e+00  5.2612e-01  2.0960 0.036085 *

#ref lvl 4 class
#5:cons_typeliquid           -1.813475    0.790813 -2.2932 0.021838 * 

#---------------------------Бежтинский (Хашархота)----------
bezhh <- subset(full, lang == "bezhh")
bezhh <- subset(bezhh, class != "PL")
bezhh <- subset(bezhh, class != "NA")
bezhh$class <- factor(bezhh$class)

#визуализируем семантику 3 4 5 классов
bezhh3 <- subset(bezhh, class == "3")
x <- table(bezhh3$semantics_KK)
bezhh4 <- subset(bezhh, class == "4")
y <- table(bezhh4$semantics_KK)
bezhh5 <- subset(bezhh, class == "5")
w <- table(bezhh5$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Bezhta (Khasharkhota) semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(bezhh3$semantics_KK), cex.names=0.8)

#визуализируем начальный согласный #без животных
bezhh.no <- subset(bezhh, semantics_KK != "dom.anim")
bezhh.no <- subset(bezhh.no, semantics_KK != "wild.anim")
bezhh.no$semantics_KK <- factor(bezhh.no$semantics_KK)

bezhh3 <- subset(bezhh.no, class == "3")
bezhh4 <- subset(bezhh.no, class == "4")
bezhh5 <- subset(bezhh.no, class == "5")
x <- table(bezhh3$cons_type)
y <- table(bezhh4$cons_type)
z <- table(bezhh5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Bezhta (Khasharkhota) initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(bezhh5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#модель без животных, уровень - 3 класс
bezhh1 <- mlogit.data(bezhh.no, shape = "wide", choice = "class")
summary(m.bezhh <- mlogit(class ~ 1|semantics_KK + cons_type, data = bezhh1, 
                          reflevel = 2)) 

#ref_level 3 class
#4:semantics_KKhomeware      -2.312306    0.731442 -3.1613 0.001571 **
#4:cons_typelabial           -1.074857    0.343087 -3.1329 0.001731 **
#5:cons_typelabial           -1.404191    0.493076 -2.8478 0.004402 **
 # 4:cons_typeliquid            0.901039    0.422465  2.1328 0.032940 * 

#ref level 4 class
#5:cons_typeliquid           -2.010899    0.784425 -2.5635 0.010361 * 
#5:cons_typeplosive.post     -1.119232    0.559061 -2.0020 0.045286 * 


#----------------------------HUNZIB---------------------------------------
gunz <- subset(full, lang == "gunz" & class != "NA")
gunz <- subset(gunz, class != "PL")
gunz$class <- factor(gunz$class)

#визуализируем семантику 3 4 5 классов
gunz3 <- subset(gunz, class == "3")
x <- table(gunz3$semantics_KK)
gunz4 <- subset(gunz, class == "4")
y <- table(gunz4$semantics_KK)
gunz5 <- subset(gunz, class == "5")
w <- table(gunz5$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Hunzib semantics", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(gunz3$semantics_KK), cex.names=0.8)

#выкидываем животных
gunz.no.anim <- subset(gunz, semantics_KK != "dom.anim")
gunz.no.anim <- subset(gunz.no.anim, semantics_KK != "wild.anim")
gunz.no.anim$semantics_KK <- factor(gunz.no.anim$semantics_KK)
#визуализируем начальный согласный
gunz3 <- subset(gunz.no.anim, class == "3")
gunz4 <- subset(gunz.no.anim, class == "4")
gunz5 <- subset(gunz.no.anim, class == "5")
x <- table(gunz3$cons_type)
y <- table(gunz4$cons_type)
z <- table(gunz5$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4", "5"),
        main = "Hunzib initial consonant", las=2, col = c("#FFFF00", "#FF0000", "#00008B"),
        names.arg=rownames(gunz5$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)


#первая модель - референс левел самый большой класс - 3 кл
gunz1 <- mlogit.data(gunz.no.anim, shape = "wide", choice = "class")
summary(m.gunz <- mlogit(class ~ 1|semantics_KK + cons_type, data = gunz1, 
                         reflevel = 2))

#------------------------------------TSEZ---------------------
tsez <- subset(full, lang == "tsez" & class != "NA")
tsez <- subset(tsez, class != "PL")
tsez <- subset(tsez, class != "5")
tsez$class <- factor(tsez$class)

#визуализируем семантику 2 3 4  классов
tsez2 <- subset(tsez, class == "2")
x <- table(tsez2$semantics_KK)
tsez3 <- subset(tsez, class == "3")
y <- table(tsez3$semantics_KK)
tsez4 <- subset(tsez, class == "4")
w <- table(tsez4$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency",
        main = "Tsez semantics", las=2, col = c("#008000", "#FFFF00", "#FF0000"),
        names.arg=rownames(tsez3$semantics_KK), cex.names=0.8)
# животные = 3 класс

#визуализируем начальный согласный без животных
tsez.n <- subset(tsez, semantics_KK != "dom.anim")
tsez.n <- subset(tsez.n, semantics_KK != "wild.anim")
tsez.n$semantics_KK <- factor(tsez.n$semantics_KK)

tsez2 <- subset(tsez.n, class == "2")
x <- table(tsez2$cons_type)
tsez3 <- subset(tsez.n, class == "3")
y <- table(tsez3$cons_type)
tsez4 <- subset(tsez.n, class == "4")
w <- table(tsez4$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("2", "3", "4"),
        main = "Tsez initial consonant", las=2, col = c("#008000", "#FFFF00", "#FF0000"),
        names.arg=rownames(tsez2$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#убираем животных

tsez1 <- mlogit.data(tsez.n, shape = "wide", choice = "class")
summary(m.tsez <- mlogit(class ~ 1| semantics_KK + cons_type, data = tsez1, 
                         reflevel = 2)) 

#--------------------------------ГИНУХСКИЙ--------------------
gin <- subset(full, lang == "gin" & class != "NA")
gin <- subset(gin, class != "PL")
gin <- subset(gin, class != "5")
gin$class <- factor(gin$class)

#визуализируем семантику 2 3 4  классов
gin2 <- subset(gin, class == "2")
x <- table(gin2$semantics_KK)
gin3 <- subset(gin, class == "3")
y <- table(gin3$semantics_KK)
gin4 <- subset(gin, class == "4")
w <- table(gin4$semantics_KK)
sem <- rbind(x, y, w) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Hinuq semantics", las=2, col = c("#008000", "#FFFF00", "#FF0000"),
        names.arg=rownames(gin3$semantics_KK), cex.names=0.8)
#животные = 3 класс

#визуализируем начальный согласный - без животных
gin.n <- subset(gin, semantics_KK != "dom.anim")
gin.n <- subset(gin.n, semantics_KK != "wild.anim")
gin.n$semantics_KK <- factor(gin.n$semantics_KK)

gin2 <- subset(gin.n, class == "2")
x <- table(gin2$cons_type)
gin3 <- subset(gin.n, class == "3")
y <- table(gin3$cons_type)
gin4 <- subset(gin.n, class == "4")
w <- table(gin4$cons_type)
cons <- rbind(x, y, z) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("2", "3", "4"),
        main = "Hinuq initial consonant", las=2, col = c("#008000", "#FFFF00", "#FF0000"),
        names.arg=rownames(gin2$cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)


gin1 <- mlogit.data(gin.n, shape = "wide", choice = "class")
summary(m.gin <- mlogit(class ~ 1|semantics_KK + cons_type, data = gin1, 
                        reflevel = 2)) 


#-------------------------------ARCHI---------
arch <- subset(full, lang == "arch")
arch <- subset(arch, class != "NA")
arch <- subset(arch, class != "PL")
arch$class <- factor(arch$class)
levels(arch$class)
summary(arch$class)


#визуализируем то, что внутри 3 и 4 класса по семантике
arch3 <- subset(arch, class == "3")
x <- table(arch3$sem_modif)
arch4 <- subset(arch, class == "4")
y <- table(arch4$sem_modif)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Archi semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(arch3$sem_modif))

#визуализируем тип согласного по классу
arch3 <- subset(arch, class == "3")
x <- table(arch3$cons_type)
arch4 <- subset(arch, class == "4")
y <- table(arch4$cons_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Archi initial consonants", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(arch3$cons_type), cex.names = 0.8,
        args.legend = list("center", bty = "n", inset=c(-0.15, 0)))

arch$cons_type <- as.character(arch$cons_type)
arch$cons_type[arch$cons_type == "NA"] <- "vowel"
arch$cons_type <- as.factor(arch$cons_type)
#не выкидываем животных!
summary(m.ar.1 <- bayesglm(class ~ sound_type + semantics,
                           family = binomial, data = arch))
drop1(m.ar.1, test="Chisq") #nothing to drop


#как это понимать?
plot(allEffects(m.ar.1), 
     axes = list(x=list(rotate=90)))
#------------------------Rutul-----------
rutl <- subset(full, lang == "rutl" & class != "NA")
rutl <- subset(rutl, class != "PL")
levels(rutl$class)
rutl$class <- factor(rutl$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
rutl3 <- subset(rutl, class == "3")
x <- table(rutl3$semantics)
rutl4 <- subset(rutl, class == "4")
y <- table(rutl4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Rutul semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(rutl3$semantics))

#визуализируем согласные (c животными)
rutl3 <- subset(rutl, class == "3")
x <- table(rutl3$sound_type)
rutl4 <- subset(rutl, class == "4")
y <- table(rutl4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Rutul initial sound", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(rutl3$sound_type), cex.names = 0.8)

#начальный звук без животных
rutl.n <- subset(rutl, semantics != "dom.anim")
rutl.n <- subset(rutl.n, semantics != "wild.anim")

rutl3 <- subset(rutl.n, class == "3")
x <- table(rutl3$sound_type)
rutl4 <- subset(rutl.n, class == "4")
y <- table(rutl4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Rutul initial sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(rutl3$sound_type), cex.names = 0.8)


#первая модель - максимальная - c животными
summary(model.rutl1 <- bayesglm(class ~ semantics + sound_type,
                                family = binomial, data = rutl))
drop1(model.rutl1, test="Chisq") #drop sound type

summary(model.rutl2 <- bayesglm(class ~ semantics,
                                family = binomial, data = rutl))

AIC(model.rutl1, model.rutl2) #model.rutl2 is not worse, stick with rutl2


#визуализировать effect plot
plot(allEffects(model.rutl2), 
     axes = list(x=list(rotate=90)), main = "Rutul effect plot")

#-------------------------Цахурские--------------------------
ts <- subset(full, lang == "tsah" | lang == "tsahg" | lang == "tsahc")
summary(ts$class)
ts <- subset(ts, class !="NA")
ts <- subset(ts, class !="PL")
ts$class <- factor(ts$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
ts3 <- subset(ts, class == "3")
ts4 <- subset(ts, class == "4")
x <- table(ts3$semantics)
y <- table(ts4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Tsakhur semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(ts$semantics), cex.names=0.8)

#визуализируем тип согласного по классам
ts3 <- subset(ts, class == "3")
ts4 <- subset(ts, class == "4")
x <- table(ts3$sound_type)
y <- table(ts4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Tsakhur initial sound", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(ts3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)


#модели
summary(m.ts <- glmer(class ~ sound_type + semantics + (1|lang), 
                      family = binomial, data = ts, 
                      control = glmerControl(optimizer ='bobyqa')))
drop1(m.ts, test="Chisq") #drop sound type

summary(m.ts1 <- glmer(class ~ semantics + (1|lang), 
                       family = binomial, data = ts, 
                       control = glmerControl(optimizer ='bobyqa')))

AIC(m.ts, m.ts1) #worse as a model, final model - m.ts

plot(allEffects(m.ts), 
     axes = list(x=list(rotate=90)))

#-----------------------------КРЫЗСКИЙ---------------
kryz <- subset(full, lang == "kryz" & class != "NA")
kryz <- subset(kryz, class != "PL")
kryz$class <- factor(kryz$class)
levels(kryz$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
kryz3 <- subset(kryz, class == "3")
x <- table(kryz3$semantics)
kryz4 <- subset(kryz, class == "4")
y <- table(kryz4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Kryts semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(kryz3$semantics), cex.names=0.8)


#визуализируем тип начального согласного по классам 
kryz3 <- subset(kryz, class == "3")
kryz4 <- subset(kryz, class == "4")
x <- table(kryz3$sound_type)
y <- table(kryz4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Kryts initial sound", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(kryz3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)
#ничто не является потенциальным предиктором - ожидаемо, тк в крызском - инфиксы и суффиксы

#визуализируем тип начального согласного по классам БЕЗ ЖИВОТНЫХ
kryz.n <- subset(kryz, semantics != "dom.anim")
kryz.n <- subset(kryz.n, semantics != "wild.anim")

kryz3 <- subset(kryz.n, class == "3")
kryz4 <- subset(kryz.n, class == "4")
x <- table(kryz3$sound_type)
y <- table(kryz4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Kryts initial sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(kryz3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)
#ничто не является потенциальным предиктором - ожидаемо, тк в крызском - инфиксы и суффиксы


#тип КОНЕЧНОГО согласного
kryz$fin_cons_type <- factor(kryz$fin_cons_type)
kryz$fin_cons_type <- as.character(kryz$fin_cons_type)
kryz$fin_cons_type[kryz$fin_cons_type == "NA"] <- "vowel"
kryz$fin_cons_type <- as.factor(kryz$fin_cons_type)

kryz3 <- subset(kryz.n, class == "3")
kryz4 <- subset(kryz.n, class == "4")
x <- table(kryz3$fin_cons_type)
y <- table(kryz4$fin_cons_type)
fin_cons <- rbind(x, y) 
barplot(fin_cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Kryts final sound (wihtout animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(kryz3$fin_cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#крызский - язык с суффиксальными КП
#первоначальная модель  class ~ fin_cons_type + POST_SG + semantics_KK
summary(m.kr1 <- bayesglm(class ~ fin_cons_type + semantics + sound_type,
                          family = binomial, data = kryz))

drop1(m.kr1, test="Chisq") #drop sound_type

summary(m.kr2 <- bayesglm(class ~ fin_cons_type + semantics,
                          family = binomial, data = kryz)) 
AIC(m.kr1, m.kr2) #stick with kr1

plot(allEffects(m.kr2), 
     axes = list(x=list(rotate=90)))

#----------------------БУДУХСКИЙ-----------------------
bud <- subset(full, lang == "bud")
bud <- subset(bud, class != "NA")
bud <- subset(bud, class != "PL")
bud$class <- factor(bud$class)
levels(bud$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
bud3 <- subset(bud, class == "3")
x <- table(bud3$semantics)
bud4 <- subset(bud, class == "4")
y <- table(bud4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Budukh semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(kryz3$semantics), cex.names=0.8)

#визуализируем тип согласного по классам

bud3 <- subset(bud, class == "3")
bud4 <- subset(bud, class == "4")
x <- table(bud3$sound_type)
y <- table(bud4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Budukh initial sound", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(bud3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#начальный согласных без животных
bud.n <- subset(bud, semantics != "dom.anim")
bud.n <- subset(bud.n, semantics != "wild.anim")
bud3 <- subset(bud.n, class == "3")
bud4 <- subset(bud.n, class == "4")
x <- table(bud3$sound_type)
y <- table(bud4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Budukh initial sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(bud3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

# визуализируем КОНЕЧНЫЙ согласный
bud$fin_cons_type <- factor(bud$fin_cons_type)
bud.n$fin_cons_type <- factor(bud.n$fin_cons_type)

bud3 <- subset(bud.n, class == "3")
bud4 <- subset(bud.n, class == "4")
x <- table(bud3$fin_cons_type)
y <- table(bud4$fin_cons_type)
fin_cons <- rbind(x, y) 
barplot(fin_cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Budukh final sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(bud3$fin_cons_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)

#по семантике - на графике выделяются животные, время (4 класс), субстанции - тенденция к 4 кл
#по типу согласного - все примерно равны (начальный или конечный - все равно)

summary(m.bud1 <- bayesglm(class ~ fin_cons_type + semantics + sound_type,
                           family = binomial, data = bud))
drop1(m.bud1, test="Chisq") #drop fin_cons_type

summary(m.bud2 <- bayesglm(class ~ semantics + sound_type,
                           family = binomial, data = bud))

AIC(m.bud1, m.bud2) #stick with bud2
drop1(m.bud2, test="Chisq") #drop sound_type

summary(m.bud3 <- bayesglm(class ~ semantics,
                           family = binomial, data = bud))
AIC(m.bud2, m.bud3) #stick with bud3

plot(allEffects(m.bud3), 
     axes = list(x=list(rotate=90)),
     main = "Budukh effect plot")

#---------------------------LAK--------------
lak <- subset(full, lang == "lak")
lak <- subset(lak, class != "PL")
lak <- subset(lak, class != "NA")
lak$class <- factor(lak$class)
levels(lak$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
lak3 <- subset(lak, class == "3")
x <- table(lak3$semantics)
lak4 <- subset(lak, class == "4")
y <- table(lak4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", 
        main = "Lak semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(lak3$semantics))

#визуализируем тип согласного по классу БЕЗ ЖИВ
lak.n <- subset(lak, semantics != "dom.anim")
lak.n <- subset(lak.n, semantics != "wild.anim")
lak.n$semantics <- factor(lak.n$semantics)

lak3 <- subset(lak.n, class == "3")
x <- table(lak3$sound_type)
lak4 <- subset(lak.n, class == "4")
y <- table(lak4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Lak initial sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(lak3$sound_type), cex.names = 0.8)


summary(m.lak.1 <- bayesglm(class ~ sound_type + semantics,
                            family = binomial, data = lak))
drop1(m.lak.1, test="Chisq") #nothing to drop

plot(allEffects(m.lak.1), 
     axes = list(x=list(rotate=90)))

#-------------------Khinalug----------------
hin <- subset(full, lang == "hin")
hin <- subset(hin, class != "PL")
hin <- subset(hin, class != "NA")
hin$class <- factor(hin$class)
levels(hin$class)

#визуализируем то, что внутри 3 и 4 класса по семантике
hin3 <- subset(hin, class == "3")
x <- table(hin3$semantics)
hin4 <- subset(hin, class == "4")
y <- table(hin4$semantics)
sem <- rbind(x, y) 
barplot(sem, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Khinalug semantics", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(hin3$semantics), cex.names=0.8)

#визуализируем тип согласного по классам - БЕЗ ЖИВОТНЫХ
hin.n <- subset(hin, semantics != "dom.anim")
hin.n <- subset(hin.n, semantics != "wild.anim")
hin.n$semantics <- factor(hin.n$semantics)

hin3 <- subset(hin.n, class == "3")
x <- table(hin3$sound_type)
hin4 <- subset(hin.n, class == "4")
y <- table(hin4$sound_type)
cons <- rbind(x, y) 
barplot(cons, beside = TRUE, ylab = "Frequency", legend = c("3", "4"),
        main = "Khinalug initial sound (without animates)", las=2, col = c("#FFFF00", "#FF0000"),
        names.arg=rownames(hin3$sound_type), 
        args.legend = list("centre", bty = "n", inset=c(-0.15, 0)), cex.names=0.8)


summary(m.hin1 <- bayesglm(class ~ sound_type + semantics, data=hin, family=binomial))
drop1(m.hin1, test="Chisq") #nothing to drop


#визуализация эффектов
plot(allEffects(m.hin1), axes = list(x=list(rotate=90)))
