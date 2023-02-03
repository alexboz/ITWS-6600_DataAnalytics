library("readxl")

# Load and attach dataset
EPIData <- read_excel("C:/Users/alexboz/Desktop/2010EPI_data.xls", sheet="EPI2010_all countries")
attach(EPIData)

# Verify data loaded correctly
fix(EPIData)
EPI

# Convert strings to numeric
EPI <- as.numeric(EPI)
EPI

# Remove NA values
tf <- is.na(EPI)
tf
EPI <- EPI[!tf]
EPI

# Exercise 1: Exploration
summary(EPI)
fivenum(EPI)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI, bw=1.))
lines(density(EPI, bw="SJ"))
rug(EPI)
?stem

# Exercise 1: Fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
?par
qqnorm(EPI)
qqline(EPI)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Exercise 1: Repeat for ENVHEALTH
ENVHEALTH
ENVHEALTH <- as.numeric(ENVHEALTH)
ENVHEALTH
ENVHEALTH <- ENVHEALTH[!is.na(ENVHEALTH)]
ENVHEALTH

summary(ENVHEALTH)
fivenum(ENVHEALTH)
stem(ENVHEALTH)
hist(ENVHEALTH)
lines(density(ENVHEALTH, bw=1.))
lines(density(ENVHEALTH, bw="SJ"))
rug(ENVHEALTH)

plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ENVHEALTH)
qqline(ENVHEALTH)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Exercise 1: Repeat for ECOSYSTEM
ECOSYSTEM
ECOSYSTEM <- as.numeric(ECOSYSTEM)
ECOSYSTEM
ECOSYSTEM <- ECOSYSTEM[!is.na(ECOSYSTEM)]
ECOSYSTEM

summary(ECOSYSTEM)
fivenum(ECOSYSTEM)
stem(ECOSYSTEM)
hist(ECOSYSTEM)
lines(density(ECOSYSTEM, bw=1.))
lines(density(ECOSYSTEM, bw="SJ"))
rug(ECOSYSTEM)

plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ECOSYSTEM)
qqline(ECOSYSTEM)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Exercise 1: Repeat for AGRICULTURE
AGRICULTURE
AGRICULTURE <- as.numeric(AGRICULTURE)
AGRICULTURE
AGRICULTURE <- AGRICULTURE[!is.na(AGRICULTURE)]
AGRICULTURE

summary(AGRICULTURE)
fivenum(AGRICULTURE)
stem(AGRICULTURE)
hist(AGRICULTURE)
lines(density(AGRICULTURE, bw=1.))
lines(density(AGRICULTURE, bw="SJ"))
rug(AGRICULTURE)

plot(ecdf(AGRICULTURE), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(AGRICULTURE)
qqline(AGRICULTURE)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Exercise 1: Repeat for CLIMATE
CLIMATE
CLIMATE <- as.numeric(CLIMATE)
CLIMATE
CLIMATE <- CLIMATE[!is.na(CLIMATE)]
CLIMATE

summary(CLIMATE)
fivenum(CLIMATE)
stem(CLIMATE)
hist(CLIMATE)
lines(density(CLIMATE, bw=1.))
lines(density(CLIMATE, bw="SJ"))
rug(CLIMATE)

plot(ecdf(CLIMATE), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(CLIMATE)
qqline(CLIMATE)
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

# Comparing distributions
boxplot(AGRICULTURE, CLIMATE)
qqplot(AGRICULTURE, CLIMATE)

# Comparing distributions
DALY
DALY <- as.numeric(DALY)
DALY
boxplot(EPI, DALY)
qqplot(EPI, DALY)

# Exercise 2: Filtering (populations)
EPILand <- EPI[!Landlock]
EPILand <- EPILand[!is.na(EPILand)]
hist(EPILand)
hist(EPILand, seq(30., 95., 1.0), prob=TRUE)

plot(ecdf(EPILand), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPILand)
qqline(EPILand)

EPI_South_Asia <- EPI[EPI_regions=="South Asia"]
EPI_South_Asia
