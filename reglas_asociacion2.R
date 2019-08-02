library(discretization)
load("bupa.rda")
bupa <- read.csv(file.choose())
disc.bupa=chiM(bupa)
dbupa=disc.bupa$Disc.data

str(dbupa)

for (i in 1:7){dbupa[,i]=as.factor(dbupa[,i])}


dbupa<-as.data.frame(dbupa)
dbupa.ar<-as(dbupa, "transactions")

rules <- apriori(dbupa.ar,parameter =
                   list(supp = 0.20, conf = 0.9,target = "rules"))
summary(rules)
inspect(rules)
library(arulesViz)
plot(rules, measure=c("support", "lift"),
     shading="confidence", interactive=TRUE)

plot(rules,method="graph",control=list(alpha=1))

plot(rules, method="graph",
     control=list(alpha=1,type="items"))