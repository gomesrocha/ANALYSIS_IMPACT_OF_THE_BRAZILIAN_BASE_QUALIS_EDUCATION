library(ggplot2)
library(ggthemes)
library(scales)
library(readxl)
library(RColorBrewer)
library(magrittr)
library(plotly)
library(dplyr)
library(reshape2)
library(Hmisc)

####
# Analysis of data from Qualis Education in comparison with SJR and JCR 
# carried out by Prof. Dr. Alejandro C. Frery (UFAL) and Prof. Me. Fabio
# Gomes Rocha (Unit/ITP) in the scope of research ANALYSIS OF THE 
# INTERNATIONAL IMPACT OF THE BRAZILIAN BASE QUALIS -- EDUCATION
####

#Import Excel Files
revistas_total <- read_excel("../../Data/Excel/Planilha_Qualis_NA.xlsx", col_types = c("text", "text", "text", 
                                                               "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric"))

Qtotal <- read_excel("../../Data/Excel/Qualis_Lista.xlsx", col_types = c("text", "text", "text"))
dataq <-data.frame((Qtotal))
dataq
dataq <- dataq %>% 
  group_by(Estrato) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>%
  arrange(desc(Estrato))
summary(dataq)
dataq$label <- scales::percent(dataq$per)

# Graphics 1: Percentage of journals in  Qualis Education per stratum
EstratoEdu$summary.Education.Estrato.
dataq$Strata = dataq$Estrato
dados = c(Strata, Percent)
dataq$porcent = round((dataq$per*100), digits = 2)
dataq$porcent
ymax = cumsum(dataq$porcent)
ymin = c(0, head(ymax, n=-1))
dataq$rotulo=paste(dataq$porcent,"%",sep="")
dataq$Strata = paste(dataq$Strata, " - ", dataq$porcent,"%",sep="")
paleta_cor <- brewer.pal(8, "Set2")
labelPosition <- (ymax + ymin) / 2
ggplot(dataq, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Strata)) +
  geom_rect() +
  geom_text( x=3.5, aes(y=labelPosition, label=label), size=3) +
  #scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")
#ggsave("../../Plots/PizzaPlotEstratoQualis.pdf")

#Graphics 2: Distribution of journals in the Qualis-Education base, and the JCR and SJR indexes
distribuicao <- read_excel("../../Data/Excel/QualisSjrJcr2.xlsx")
Qualis <- c(rep("A1", 3), rep("A2", 3),rep("B1", 3),rep("B2", 3),rep("B3", 3),rep("B4", 3),rep("B5", 3))
typedata <- rep(c("Qualis Education", "Qualis Education with JCR", "Qualis Education with SJR"))
Quantity <- distribuicao$Quantity
data <- data.frame(Qualis, typedata, Quantity)
ggplot(data, aes(fill=typedata, y=Quantity, x=Qualis)) + 
  geom_bar(position="dodge", stat="identity") +
  theme_light() +
  labs(fill = "")
#ggsave("../../Plots/BarPlotDistribuicaoQUalisSjrJCR.pdf")

#Graphics 3: Notched boxplots of JIF values of QE journals per stratum

Strata = revistas_total$Estrato
ggplot(revistas_total, aes(group=revistas_total$Estrato,y=JIF,x=reorder(Estrato, desc(Estrato)), fill = Strata)) + 
  geom_boxplot(notch=TRUE) + 
  #geom_jitter() +
  scale_x_discrete(limits=rev(levels(revistas_total$Estrato))) +
  scale_y_log10() +
  labs(x="Qualis Stratum", y = "JIF") +
  theme_light()
#ggsave("../../Plots/BoxplotQualisJif.pdf")

# Graphics 4 - Boxplot Qualis education with SJR 

ggplot(revistas_total, aes(group=Estrato,y=`Hindex SJR`,x=reorder(Estrato, desc(Estrato)), fill = Strata)) + 
  geom_boxplot(notch=TRUE) + 
  #  geom_jitter() +
  scale_x_discrete(limits=rev(levels(revistas_total$Estrato))) +
  scale_y_log10() +
  labs(x="Qualis Stratum", y = "SJR") +
  theme_light()
#ggsave("../../Plots/BoxplotQualisSjr.pdf")

# Graphics 5 - QE journals indexed by JCR 
jcrQualis <- read_excel("../../Data/Excel/distribuicao_JCR_Qualis.xlsx", col_types = c("text", "numeric"))
jcrQualis$Percentual
jcrQualis <- data.frame(
  Estrato = c("A1", "A2", "B1", "B3","B4", "NA"),
  value = c(6, 9, 1, 0.5, 0.5, 83)
)

nr=paste(jcrQualis$Estrato, " - ", jcrQualis$value,"%",sep="")
ymax = cumsum(jcrQualis$value)
ymin = c(0, head(ymax, n=-1))
rotulo=paste(jcrQualis$value,"%",sep="")
jcrQualis$Strata=paste(jcrQualis$Estrato, " - ", jcrQualis$value,"%",sep="")
paleta_cor <- brewer.pal(8, "Set2")
labelPosition <- (ymax + ymin) / 2
ggplot(jcrQualis, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Strata)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")
#ggsave("../../Plots/PizzaplotQualisJCR.pdf")
#####


# Graphics 6: QE journals indexed by SJ/R
jcrQualisEst <- read_excel("../../Data/Excel/distribuicao_JCR_Qualis_estrato.xlsx", col_types = c("text", "numeric"))
nr=paste(jcrQualisEst$Estrato, " - ", round(jcrQualisEst$Valores*100/277, 2),"%",sep="")
nr

jcrQualisEst$Percent = c(round(jcrQualisEst$Valores*100/277, 2))
ymax = cumsum(jcrQualisEst$Percent)
ymin = c(0, head(ymax, n=-1))
rotulo=paste(jcrQualisEst$Percent,"%",sep="")
jcrQualisEst$Percent
jcrQualisEst$Strata=c(nr)
jcrQualisEst$Strata
paleta_cor <- brewer.pal(8, "Set2")
labelPosition <- (ymax + ymin) / 2
ggplot(jcrQualisEst, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Strata)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")
#ggsave("../../Plots/PizzaplotQualisSJR.pdf")

#
# Graphics 7: Article Influence X JIF in relation to Qualis-Education
revistas_totalSJ <- read_excel("../../Data/Excel/Planilha_Qualis_NA_Sem_Jama2.xlsx", col_types = c("text", "text", "text", 
                                                                      "numeric", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric"))
revistas_totalSJ
Strata <- revistas_totalSJ$Estrato

ggplot(revistas_totalSJ, aes(x=JIF, y = `Article influence`, 
                      col=Strata)) +
  geom_point(size=4, alpha=.7) +
  theme_light() +
  theme(axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.position = "top") #+
#ggsave("../../Plots/AIJIFRelacaoQualis.pdf")

# Graphics 8: Number of journals Qualis-Education with JIF
distribuicao <- read_excel("../../Data/Excel/Graphic10.xlsx")
distribuicao
ggplot(distribuicao, aes(x = Strata, y = Value))+
  geom_col(aes(fill = Type), width = 0.7) +
  theme_light() +
  labs(fill = "")
ggsave("../../Plots/BarraQualisJIF.pdf")

 
### Test that the proportion of journals in each stratum in Q is the same as that in JCR and SJR
### The data come from Table 1

require(reshape2)
require(ggplot2)
require(ggthemes)

### Counts
Stratum <- c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C")
cQ <- c(10692, 13158, 22761, 18405, 14128, 16261, 18283, 17586)
cQE <- c(121, 380, 542, 425, 357, 307, 782, 1289)
cQE.JCR <- c(121, 159, 58, 6, 7, 4, 6, NA)
cQE.SJR <- c(3, 234, 187, 22, 18, 12, 22, NA)

CountsTable <- data.frame(Stratum=as.factor(Stratum),
                          cQ, cQE, cQE.JCR, cQE.SJR)

NumberJournals <- data.frame(Stratum, 
                             Q=cQ/sum(cQ, na.rm=TRUE), 
                             QE=cQE/sum(cQE, na.rm=TRUE), 
                             QE.JCR=cQE.JCR/sum(cQE.JCR, na.rm=TRUE), 
                             QE.SJR=cQE.SJR/sum(cQE.SJR, na.rm=TRUE))
NumberJournals.melt <- melt(NumberJournals, 
                            variable.name = "Base", 
                            value.name = "Proportion", 
                            id.vars = "Stratum"
)
# Graphics 9: Proportion of venues per stratum in each base
ggplot(NumberJournals.melt, aes(x=forcats::fct_rev(Stratum), y=Proportion, group=Base)) +
  geom_bar(aes(fill=Base), stat="identity", position=position_dodge(), col="black") +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Stratum", y="Proportion") +
  theme_minimal() +
  theme(text=element_text(size=20))
ggsave(file="../../Plots/ProportionsPerBase.pdf")

# Graphics 10: Compare Q and QE.C only
ggplot(subset(NumberJournals.melt, Base=="Q" | Base=="QE"), 
       aes(x=forcats::fct_rev(Stratum), y=Proportion, group=Base)) +
  geom_bar(aes(fill=Base), stat="identity", position=position_dodge(), col="black") +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Stratum", y="Proportion") +
  theme_minimal() +
  theme(text=element_text(size=20))
ggsave(file="../../Plots/ProportionsQ-QE.pdf")