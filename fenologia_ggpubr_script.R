head(fenologia)
str(fenologia)

ggplot(fenologia$VB, aes(as.factor(YEAR), value)) + 
  geom_violin(aes(colour=variable)) + geom_jitter(aes(colour=variable), alpha=0.3,width = 0.25) + ggtitle("VB x Year") + 
  stat_summary(aes(group=variable), fun.y = "mean", geom = "point", shape= 23, size= 2, fill= "white") + 
  geom_smooth(alpha=2/10,aes(group=variable),method = "lm", se=FALSE, color="dark grey", size=.5,linetype="dashed") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + 
  scale_color_manual(values="#56B4E9")

ggplot(fenologia, aes(fenologia$YEAR, fenologia$VB)) +
  geom_boxplot() + 
  theme_bw()


p <- ggboxplot(fenologia, x = "YEAR", y = "VB",
          add = "jitter", color="YEAR",
          shape = "YEAR")

my_comparisons <- list( c("2006", "2007"), c("2006", "2008"), c("2007", "2008") )
str(my_comparisons)

# Add pairwise comparisons p-value
p + stat_compare_means(comparisons = my_comparisons)+ 
  stat_compare_means(label.y = 300) 

#### funziona !! #####
fenologia <- fenologia[!fenologia$YEAR == "2001",]
fenologia <- fenologia[!fenologia$YEAR == "2002",]
fenologia <- fenologia[!fenologia$YEAR == "2003",]
years <- unique(fenologia$YEAR)
years <- as.character(years)
str(years)
comparisons_years <- combn(years,2, simplify = F)
str(comparisons_years)

p <- ggboxplot(fenologia, x = "YEAR", y = "FB",
               color="YEAR")

p + stat_compare_means(comparisons = comparisons_years) + 
  stat_compare_means(label.y = 300)

length(fenologia[fenologia$YEAR=="2004",5])
############################## 
install.packages("sommer")
library(sommer)

head(fenologia)
str(fenologia)
fenologia$CODE <- as.factor(fenologia$CODE)
fenologia$YEAR <- as.factor(fenologia$YEAR)
sub_feno <- fenologia[1:20,]
ans1 <- mmer2(FB~1, random = ~CODE + YEAR , rcov = ~units, data=sub_feno, silent = TRUE)
vc <- ans1$var.comp


library(lme4)
ans1 <- lmer(FB~1, random = ~CODE + YEAR , data=sub_feno)











