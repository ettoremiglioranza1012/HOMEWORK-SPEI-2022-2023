# HOMEWORK-SPEI-2022-2023
Dipartimento di Economia e Management Corso di Laurea in Economia e Management Esame di Statistica, probabilità e inferenza – a.a. 2022/2023 Prof. Giuseppe Espa, Prof. Flavio Santi. 

These are the solutions to the problems found in the homework assignment for the Statistics 2 exam given by Prof. Giuseppe Espa at the Trento University of Economics and Management. The assignment consisted of five exercises to be completed with the tools provided by the R programming language. These exercises summarize all the statistical skills (applied through the use of R) learned during the first and second statistics exams, which include: fundamentals of descriptive statistics and statistical inference (brief summary of the first exam), comparison between two groups, association analysis between qualitative variables, linear relationships, multivariate linear relationships, and logistic regression. 
This archive is designed for students who, like me, need to learn R and solve exercises like these. Wish u good luck!

# HOMEWORK SPEI 2023 
# ANALISI DEI DATI E STATISTICA
# PROF. Giuseppe Espa 

library(sasld)

#------------------------------------------------------------------------------|
# ESERCIZIO 1 

x <- c(150:189)  # i) Creo vettore x

alpha <- c(-90)  # ii) Definisco alpha e beta
beta <- c(0.8)
ym <- alpha + (beta*x)  # iii) vettore ym 
set.seed(230009)   # iv) Innesco
e <- rnorm(40, mean = 40, sd = 5)  # v) Estraggo con rnorm 
y <- ym + e  # vi) Creo vettore y 

cor(x,y)  # vii) Correlazione 
plot(x,y)  # viii) Plotto X con Y 
fit <- lm(y ~ x)  # ix) Interpolo un modello di regressione lineare
summary(fit)  # visione riassuntiva utile 

print(fit$coef)  # mi faccio stampare i coefficienti 
abline(a = fit$coef[1], b = fit$coef[2])  # aggiungo la retta di regressione 

# xii)
SSE <- sum((fit$residuals)^2)  # devianza residua
SSR <- sum((fit$fitted.values - mean(y))^2)  # devianza spiegata 
TSS <- sum((y - mean(y))^2)  # devianza totale
MSE <- SSE/fit$df.residual  # varianza residua 
s <- sqrt(MSE)  # residual standard error s 
R2 <- (TSS-SSE)/TSS  # coefficiente di determinazione 
cbind(SSE,SSR,TSS,MSE,s,R2) # tabella riassuntiva 
# Verifica che i calcoli siano giusti con la tavola delle varianze ANOVA 
anova(fit)

# xi)
se_B <- s/sqrt(sum(x-mean(x))^2)  # Errore standard del coeff. 
confint(fit)  # IC con level = 0.95 (default) 
# Valore 0.8 compreso

# xii) 
xcen <- x - mean(x)  # centro variabile x
ycen <- y - mean(y)  # centro variabile y 
fit.cen <- lm(ycen ~ xcen)  # xiii) modello regressione lineare con variabili 
# centrate  
summary(fit.cen)  # prospetto riassuntivo 
# notiamo come stima intercetta molto vicina a 0, pendenza coefficiente 
# di x coincide con quella al punto ix)

#------------------------------------------------------------------------------|
# ESERCIZIO 2 

library(sasld)
options(PACKAGE_MAINFOLDER="C:/Users/...")
data <- read.table(file = "C:/Users/ettor/OneDrive/Documenti/UNITN/CORSI/2 anno, 2 SEM/Statistica 2/Homework Statistica 2/Homework assignment/2004 statewide crime.txt", header = TRUE, sep=",")
head(data) 			# Verifico corretto inserimento

# i)
par(mfrow = c(1,2)) 		# Funzione per poter rappresentare più di un grafico contemporaneamente 

fit.Murder <- lm(data$Murder ~ data$Poverty) 		# Interpolo Murder controllando per Poverty 
fit.Highsch <- lm(data$HighSch ~ data$Poverty)  	# Interpolo HighSch controllando per Poverty
plot(fit.Highsch$res,fit.Murder$res) 		 # Disegno i partial regression plot con i residui 
Poverty_controllo <- lm(fit.Murder$res ~ fit.Highsch$res)  	# Interpolo modello di previsione parziale
Poverty_controllo$coef[2] 		 # Sorprendente relazione positiva tra percentuale di 
# diplomati e tasso di omicidi, forse dovuta ad Outlier.
abline(Poverty_controllo)  		# Disegno retta di regressione parziale

# Ripeto lo stesso procedimento controllando HighSch
fit.Murder <- lm(data$Murder ~ data$HighSch)
fit.Poverty <- lm(data$Poverty ~ data$HighSch)
plot(fit.Poverty$res, fit.Murder$res)
Highsch_controllo <- lm(fit.Murder$res ~ fit.Poverty$res)
Highsch_controllo$coef[2]  		# Relazione positiva plausibile tra Povertà e Omicidi 
abline(Highsch_controllo)
# ii) 
fit <- lm(data$Murder ~ data$Poverty + data$HighSch)  		# Interpolo modello di regressione multipla
summary(fit)  		# Prospetto riassuntivo
print(fit$coefficients)  		# Mi stampo i coefficienti
eq_previsione <- c("y = -60.4982411 + Poverty*1.6048755 + HighSch*0.5876648")
print(eq_previsione)  		# Rappresentazione visiva equazione di regressione multipla
# Notiamo come i coefficienti dei due regressori sono entrambi positivi, 
# ciò significa che all'aumentare del tasso di povertà e della percentuale 
# di studenti diplomati aumenta anche la variabile risposta tasso di omicidi. 

# iii)
# Per rimuovere il distretto DC in posizione [51] nel data-frame ricreo i vettori
# escludendo i valori in quella posizione. 
Mu <- data$Murder[1:50]
Po <- data$Poverty[1:50]
Hi <- data$HighSch[1:50]
# Ora ripeto le osservazioni fatte al punto precedente. 
fit_senzaDC <- lm(Mu ~ Po + Hi)  		# Interpolo modello di regressione multipla
summary(fit_senzaDC) 		 # Prospetto riassuntivo
print(fit_senzaDC$coefficients) 
# Tale osservazione è molto rilevante ai fini della stima, notiamo come ogni 
# coefficiente varia notevolmente, il coefficiente di HighSch cambia
# addirittura segno. L'osservazione DC era un Outlier, che probabilmente aveva 
# influenzato il nostro modello di previsione parziale con controllo della 
# variabile Poverty, portando ad una relazione positiva tra percentuale di 
# diplomati e tasso  di omicidi(inverosimile). Di fatto, escludendo l'Outlier 
# arriviamo ad una conclusione molto più plausibile (relazione inversa tra 
# omicidi e livello di istruzione).

#------------------------------------------------------------------------------|
# ESERCIZIO 3 

lavoro <- read.table(file = "C:/Users/ettor/OneDrive/Documenti/UNITN/CORSI/2 anno, 2 SEM/Statistica 2/Homework Statistica 2/Homework assignement/lavoro.txt", header = TRUE)

n <- dim(lavoro)

names(lavoro)
attach(lavoro)

Y <- Average_Score 
X <- Years_Service 
C1 <- Sex 
C2 <- Race 

cbind(Y,X,C1,C2)

# i) 4 vettori, uno per ogni livello ottenuto
MB <- which(C1=="Male" & C2=="White")		 
MN <- which(C1=="Male" & C2=="Nonwhite")
FB <- which(C1=="Female" & C2=="White")
FN <- which(C1=="Female" & C2=="Nonwhite")
col<-c("blue","purple","green","red")
col[FB]<- "purple"
col[FN]<-"red"
col[MN]<-"green"
col[MB]<-'blue'
# Disegniamo lo scatter di Y contro X stratificando per C1 e C2
plot(X,Y, main="Scatter Plot di X vs Y", lwd=2, xlab="Anni di Servizio", ylab="Punteggio medio", col=col)
# Interpoliamo le rette di previsione per ognuno dei 4 livelli ottenuti sulla base di Y = punteggio medio su X = # anni di servizio
fit <- lm(Y[FN] ~ X[FN])
y_prev <- fit$fitted
lines(X[FN],y_prev,lwd=2,col="red")
fit <- lm(Y[MB] ~ X[MB])
y_prev <- fit$fitted
lines(X[MB],y_prev,lwd=2,col="blue")
fit <- lm(Y[MN] ~ X[MN])
y_prev <- fit$fitted
lines(X[MN],y_prev,lwd=2,col="green")
fit <- lm(Y[FB] ~ X[FB])
y_prev <- fit$fitted
lines(X[FB],y_prev,lwd=2,col="purple")
legend(20,5,legend=c("FN","MB","MN","FB"),col=c("red","blue","green","purple"), lwd=2,cex=0.85)
# Notiamo come tutte le rette hanno diversa intercetta, ma coefficiente angolare molto simile. 

n <- ncol(lavoro)
femmine <- which(C1=="Female")
maschi <- which(C1=="Male")
bianchi <- which(C2=="White")
nonbianchi <- which(C2=="Nonwhite")
sesso <- rep(0,n)
sesso[femmine] <- 0
sesso[maschi] <- 1
razza <- rep(0,n)
razza[bianchi] <- 1
razza[nonbianchi] <- 0
fit.completo <- lm(Y~X+sesso+razza+X*sesso+X*razza)
summary(fit.completo)
fit.ristretto<-lm(Y~X+sesso+razza)
summary(fit.ristretto)


residui<-fit.ristretto$res
residui
Yprevisti<-fit.ristretto$fitted
Yprevisti
plot(Yprevisti,residui,main="Residui vs valori previsti",xlab="Y previsti",ylab="Residui")
abline(h=0)
qqnorm(residui)


#------------------------------------------------------------------------------|
# ESERCIZIO 4 

dati <- read.csv(file = "C:/Users/ettor/OneDrive/Documenti/UNITN/CORSI/2 anno, 2 SEM/Statistica 2/Homework Statistica 2/Homework assignement/dati_controlli.csv", header = TRUE, sep=",")

# i)
attach(dati)
names(dati)
Y <- Irr
X1 <- prima
X2 <- ExtraUE
X3 <- dichiarato
fit <- glm(Y ~ X1 + X2 + X3, family = binomial)
summary(fit)

# ii)
a  <- fit$coef[1]
x1 <- fit$coef[2]
x2 <- fit$coef[3]
x3 <- fit$coef[4]
NumP <- exp(a+x1*1+x2*0+x3*200)
DenP <- 1+exp(a+x1*1+x2*0+x3*200)
P <- NumP/DenP
print(P)

# iii)

fit <- glm(Y ~ X1 + X2 + X3 + X1*X2, family = binomial)
summary(fit)


#------------------------------------------------------------------------------|
# ESERCIZIO 5 

# i)
tmp <- prop.test(c(81290,63029),c(5025735,5324975), correct = FALSE)
tmp
tmp$conf.int

# ii)
MortiMas <- read.table(file = "C:/Users/ettor/OneDrive/Desktop/Morti maschi.txt", header = TRUE, sep =",")
MorteFem <- read.table(file = "C:/Users/ettor/OneDrive/Desktop/Morte femmine.txt", header = TRUE, sep =",")
MortiMas
MorteFem
names(MortiMas)
Classemedia <- c(seq(5,95,10))

X2M <- MortiMas$N.casi
X2M_medio <- sum(X2M*Classemedia)/sum(X2M)
X2M_medio

X2F <- MorteFem$N.casi
X2F_medio <- sum(X2F*Classemedia)/sum(X2F)
X2F_medio

VARM <- sum((Classemedia-X2M_medio)^2*X2M)/sum(X2M)
VARM
dsM <- sqrt(VARM)

VARF <- sum((Classemedia-X2F_medio)^2*X2F)/sum(X2F)
VARF
dsF <- sqrt(VARF)

NMas <- sum(X2M)
NFem <- sum(X2F)

test.t2ci(X2M_medio, dsM, NMas, X2F_medio, dsF, NFem)

# iii)

X2M <- MortiMas$N.deceduti
X2M_medio <- sum(X2M*Classemedia)/sum(X2M)
X2M_medio

X2F <- MorteFem$N.deceduti
X2F_medio <- sum(X2F*Classemedia)/sum(X2F)
X2F_medio

VARM <- sum((Classemedia-X2M_medio)^2*X2M)/sum(X2M)
VARM
dsM <- sqrt(VARM)
dsM

VARF <- sum((Classemedia-X2F_medio)^2*X2F)/sum(X2F)
VARF
dsF <- sqrt(VARF)
dsF

NMas <- sum(X2M)
NFem <- sum(X2F)

test.t2ci(X2M_medio, dsM, NMas, X2F_medio, dsF, NFem)

# iv) 

casi <- cbind(MortiMas$N.casi,MorteFem$N.casi)
casi
sum(casi)
tabcont <- xtabs(casi ~ MortiMas$Classe_di_etÃ.)
tabcont
prop.table(tabcont)
f.oss <- chisq.test(tabcont)$observed
f.oss
f.att <- chisq.test(tabcont)$expected
f.att
prop.riga.H0 <- prop.table(f.att,1)
prop.riga.H0
prop.col.H0 <- prop.table(f.att,2)
prop.col.H0
H <-(f.oss-f.att)/sqrt(f.att*(1-prop.riga.H0)*(1-prop.col.H0))
H
X2 <- chisq.test(tabcont)
V <- sqrt((X2$statistic / sum(casi)) / (min(dim(casi)) - 1))
V

# v) 

deceduti<-cbind(MortiMas$N.deceduti, MorteFem$N.deceduti)
deceduti
tabcon<-xtabs(deceduti ~ MortiMas$Classe_di_etÃ.)
prop.table(tabcon)
f.oss<-chisq.test(tabcon)$observed
f.oss
f.att<-chisq.test(tabcon)$expected
f.att
prop.riga.H0<-prop.table(f.att,1)
prop.riga.H0
prop.col.H0<-prop.table(f.att,2)
prop.col.H0
L <-(f.oss-f.att)/sqrt(f.att*(1-prop.riga.H0)*(1-prop.col.H0))
L
X2 <- chisq.test(tabcon)
V <- sqrt((X2$statistic / sum(deceduti)) / (min(dim(deceduti)) - 1))

