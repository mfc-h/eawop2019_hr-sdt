# The Positive Effects of HR on Organizational Commitment using Basic Need Satisfaction as Mediator

# Poster Presentation at the EAWOP Conference, 29th May to 1st June 2019, Turin, Italy
# Matthias F.C. Hudecek, Mona Lehleiter & Birgit M. Stephan

# R Code

# Load packages
library(psych)
library(car)
library(mosaic)

# Import dataset
sdt <- read.csv2("data_sdt.csv")



# Preparation dataset and variables ---------------------------------------

sdt$Autonomie_1 <- as.numeric(sdt$Autonomie_1)

# Calculate dimensions of SDT
sdt$Autonomy <- with(sdt, (Autonomie_1 + Autonomie_2 + Autonomie_3 + Autonomie_4 + Autonomie_5 + Autonomie_6)/6) # Autonomie = Autonomy
sdt$Competence <- with(sdt, (Kompetenz_1 + Kompetenz_2 + Kompetenz_3 + Kompetenz_4 + Kompetenz_5 + Kompetenz_6)/6) # Kompetenz = Competence
sdt$Relatedness <- with(sdt, (Zugehoerigkeit_1 + Zugehoerigkeit_2 + Zugehoerigkeit_3 + Zugehoerigkeit_4 + Zugehoerigkeit_5 + Zugehoerigkeit_6)/6) # Zugehoerigkeit = Relatedness


# Calculate dimensions of HR methods
sdt$APReview <- with(sdt, (MAG1 + MAG2 + MAG3 + MAG4 + MAG5 + MAG6)/6)
sdt$JobRotation <- with(sdt, (JR1 + JR2 + JR3 + JR4 + JR5 + JR6)/6)
sdt$CareerPlanning <- with(sdt, (Laufbahn1 + Laufbahn2 + Laufbahn3 + Laufbahn4 + Laufbahn5 + Laufbahn6)/6)
sdt$Mentoring <- with(sdt, (Mentoring1 + Mentoring2 + Mentoring3 + Mentoring4 + Mentoring5 + Mentoring6)/6)

# Recode OC_2
sdt$OC_2I <- Recode(sdt$OC_2, "1=5; 2=4; 3=3; 4=2; 5=1") 
# Calculate dimension OCA
sdt$OCA <- with(sdt, (OC_1 + OC_2I + OC_3 + OC_4 + OC_5)/5)



# Descriptive Statistics --------------------------------------------------

# Descriptives
favstats(~Autonomy, data=sdt)
favstats(~Competence, data=sdt)
favstats(~Relatedness, data=sdt)
favstats(~APReview, data=sdt)
favstats(~JobRotation, data=sdt)
favstats(~CareerPlanning, data=sdt)
favstats(~Mentoring, data=sdt)


# Select variables for correlation matrix
cortable <- select(sdt, 75:80, 60, 82)

# Correlation
corr.test(cortable)
write.csv(correlation$r, file = "correlationR.csv") 
write.csv(correlation$p, file = "correlationP.csv")


# Mediation analysis ------------------------------------------------------

# Annual Performance Review
acr1 <- mediate(OCA ~ APReview + (Autonomy) + (Competence) + (Relatedness), data=sdt, n.iter = 10000)
print(acr1, short=F)

# Job Rotation
acr2 <- mediate(OCA ~ JobRotation + (Autonomy) + (Competence) + (Relatedness), data=sdt, n.iter = 10000)
print(acr2, short=F)

# Career Planning
acr3 <- mediate(OCA ~ CareerPlanning + (Autonomy) + (Competence) + (Relatedness), data=sdt, n.iter = 10000)
print(acr3, short=F)

# Mentoring
acr4 <- mediate(OCA ~ Mentoring + (Autonomy) + (Competence) + (Relatedness), data=sdt, n.iter = 10000)
print(acr4, short=F)
