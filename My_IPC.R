library(tidyverse)
library(readxl)
library(fitdistrplus)
library(hrbrthemes)
library(viridis)
library(forcats)
library(plotly)
library(goftest)
library(FAdist)
library(EnvStats)

#====================

GW <- as_tibble(read_excel("~/R/My_IPC/GW.xlsx"))
RohToujeo <- as_tibble(read_excel("~/R/My_IPC/RawData.xlsx", 
                        sheet = "Tabelle5"))
RohToujeo$`Stückzahl bei Probenzug`


MyRawT<-RohToujeo
MWPZ<-            MyRawT %>%
  group_by(`Stückzahl bei Probenzug`,Charge,.add = TRUE) %>%
  summarize(
    MEAN = mean(c(`Max Gleit   01`,`Max Gleit   02`,`Max Gleit   03`,`Max Gleit   04`,`Max Gleit   05`,
                  `Max Gleit   06`,`Max Gleit   07`,`Max Gleit   08`,`Max Gleit   09`,`Max Gleit   10`,
                  `Max Gleit   11`,`Max Gleit   12`)),
    SD=      sd(c(`Max Gleit   01`,`Max Gleit   02`,`Max Gleit   03`,`Max Gleit   04`,`Max Gleit   05`,
                  `Max Gleit   06`,`Max Gleit   07`,`Max Gleit   08`,`Max Gleit   09`,`Max Gleit   10`,
                  `Max Gleit   11`,`Max Gleit   12`))
  )


pz1 <- MWPZ %>%
  mutate(Charge = fct_reorder(Charge, MEAN)) %>%
  ggplot( aes(x=`Stückzahl bei Probenzug`,y=MEAN, color=Charge, fill=Charge)) +
  geom_point(alpha=0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Probenzug") +
  ylab("Gleitwerte N") +
  xlim(c(0,2.5e6))+
  facet_wrap(~Charge)
pz1
ggplotly(pz1)
max(MWPZ$`Stückzahl bei Probenzug`)


RT<- RohToujeo%>% group_by(Charge)
#RT<- RohToujeo%>% group_by(Mat.Nr.)


RTD <-RT %>%summarise(
  MEAN = mean(c(`Max Gleit   01`,`Max Gleit   02`,`Max Gleit   03`,`Max Gleit   04`,`Max Gleit   05`,
                `Max Gleit   06`,`Max Gleit   07`,`Max Gleit   08`,`Max Gleit   09`,`Max Gleit   10`,
                `Max Gleit   11`,`Max Gleit   12`)),
  SD=      sd(c(`Max Gleit   01`,`Max Gleit   02`,`Max Gleit   03`,`Max Gleit   04`,`Max Gleit   05`,
                `Max Gleit   06`,`Max Gleit   07`,`Max Gleit   08`,`Max Gleit   09`,`Max Gleit   10`,
                `Max Gleit   11`,`Max Gleit   12`))
  
) 

RTD


Gumbel_PARA<-GW %>% group_by(Batch)%>%
  summarise(
    Location = eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[1],
    Scale = eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[2],
    Wert1 = 4,
    Wert2 = 5
  )


GWG<-GW %>% group_by(Batch)
RES<-GWG %>% summarise(
  MEAN = mean(Response),
  SD=  sd(Response),
  P90 = quantile(Response,probs = c(90)/100),
  PG90 =qevd(0.90, eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[1], eevd(GW$Response, ci = TRUE, conf.level = 0.95)$parameters[2]), 
  P95 = quantile(Response,probs = c(95)/100),
  PG95 =qevd(0.95, eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[1], eevd(GW$Response, ci = TRUE, conf.level = 0.95)$parameters[2]),  
  P975 = quantile(Response,probs = c(97.5)/100),
  PG975 =qevd(0.975, eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[1], eevd(GW$Response, ci = TRUE, conf.level = 0.95)$parameters[2]), 
  P99 = quantile(Response,probs = c(99)/100),
  PG99 =qevd(0.99, eevd(Response, ci = TRUE, conf.level = 0.95)$parameters[1], eevd(GW$Response, ci = TRUE, conf.level = 0.95)$parameters[2]) 
)






#$parameters
#location     scale 
#2.4870261 0.5595964 
pevd(4, 2.4870261, 0.5595964) 
#[1] 0.2769203

qevd(0.99, 2.4870261, 0.5595964) 
#[1] -2.163317
base::set.seed(1234)
hist(revd(29604, 2.4870261, 0.5595964) )
hist(GW$Response)


# Build dataset with different distributions
mydata <- data.frame(
  type = c( rep("Batchvalue", 29604), rep("Gumbel", 29604) ),
  value = c( GW$Response, revd(29604, 2.4870261, 0.5595964) )
)

# Represent it
p <- mydata %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram(  binwidth = 0.1,color="#e9ecef", alpha=0.4, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  # labs(fill="")
  theme_ipsum() +
  theme(
    legend.position="right",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Gleitwerte N") +
  ylab("") +
  xlim(c(0,16))

p
ggplotly(p)


G<-GW%>%
  group_by(Batch)%>%
  summarise(
    n=n()
  )



G4<-GW%>%
  group_by(Batch)%>%
  filter(Response>4) %>%
  summarise(
    n=n()
  )

G5<-GW%>%
  group_by(Batch)%>%
  filter(Response>5) %>%
  summarise(
    n=n()
  )



p1 <- GW %>%
  mutate(Batch = fct_reorder(Batch, Response)) %>%
  ggplot( aes(x=Response, color=Batch, fill=Batch)) +
  geom_histogram(alpha=0.6, binwidth = 0.1) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Gleitwerte N") +
  ylab("") +
  xlim(c(0,16))+
  facet_wrap(~Batch)
p1
ggplotly(p1)
# ggplotly(p4)
# ggplotly(p45)
# ggplotly(p5)

GG<-G %>%
  mutate(Ng4=G4$n)%>%
  mutate(Ng5=G5$n)%>%
  mutate(P4=Ng4/n)%>%
  mutate(P4Rest=1-Ng4/n)%>%
  mutate(PG4=1-pevd(Gumbel_PARA$Wert1,Gumbel_PARA$Location,Gumbel_PARA$Scale))%>%
  mutate(PG4Rest=pevd(Gumbel_PARA$Wert1,Gumbel_PARA$Location,Gumbel_PARA$Scale))%>%
  mutate(P5=Ng5/n)%>%
  mutate(P5Rest=1-Ng5/n)%>%
  mutate(PG5=1-pevd(Gumbel_PARA$Wert2,Gumbel_PARA$Location,Gumbel_PARA$Scale))%>%
  mutate(PG5Rest=pevd(Gumbel_PARA$Wert2,Gumbel_PARA$Location,Gumbel_PARA$Scale))



RES

GG

summary(GG%>%
          select(P4,P4Rest,PG4,PG4Rest,P5,P5Rest,PG5,PG5Rest))
