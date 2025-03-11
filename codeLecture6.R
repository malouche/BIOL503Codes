library(agricolae)

fertilizers<-c("F1","F2","F3","F4")

latin_square<-design.lsd(seed=123,fertilizers)

latin_book<-latin_square$book

colnames(latin_book)[c(2,3,4)]<-c('Soil','Temperature','Fertilizer')

latin_book

replicates_per_cell<-10

1:nrow(latin_book)

rep(1:nrow(latin_book),each=replicates_per_cell)

latin_with_rep<-latin_book[rep(1:nrow(latin_book),each=replicates_per_cell),]

## Scenario 1: No interaction between factors

noise<-rnorm(nrow(latin_with_rep),0,1) 

latin_with_rep$Scenario1<- 133+noise

fertilizers_effect<-c(-10,-5,3,22)

latin_with_rep$Scenario2<- 133 +fertilizers_effect[latin_with_rep$Fertilizer] + noise


library(dplyr)

latin_with_rep%>%
  group_by(Fertilizer)%>%
  summarise(mean1=mean(Scenario1),
            mean2=mean(Scenario2))

temperature_effect<-c(-5,5,10,25)
latin_with_rep$Scenario3<- 133 +fertilizers_effect[latin_with_rep$Fertilizer] +
  temperature_effect[latin_with_rep$Temperature] + noise

library(ggpubr)

p11<-ggboxplot(latin_with_rep,x='Fertilizer',y='Scenario1',
              color='Fertilizer',add='jitter')

p12<-ggboxplot(latin_with_rep,x='Temperature',y='Scenario1',
              color='Temperature',add='jitter')

p21<-ggboxplot(latin_with_rep,x='Fertilizer',y='Scenario2',
              color='Fertilizer',add='jitter')

p22<-ggboxplot(latin_with_rep,x='Temperature',y='Scenario2',
               color='Temperature',add='jitter')

p23<-ggboxplot(latin_with_rep,x='Soil',y='Scenario3',
               color='Soil',add='jitter')

p31<-ggboxplot(latin_with_rep,x='Fertilizer',y='Scenario3',legend='none',
               color='Fertilizer',add='jitter')

p32<-ggboxplot(latin_with_rep,x='Temperature',y='Scenario3',legend='none',
               color='Temperature',add='jitter')

p33<-ggboxplot(latin_with_rep,x='Soil',y='Scenario3',legend='none',
               color='Soil',add='jitter')


library(patchwork)

p31+p32/p33
