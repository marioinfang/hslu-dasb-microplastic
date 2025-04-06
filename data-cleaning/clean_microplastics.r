library(dplyr)

df_left <- read.csv("data-cleaning/Marine Microplastic Concentrations-left.csv", stringsAsFactors = FALSE)
df_right <- read.csv("data-cleaning/Marine Microplastic Concentrations-right.csv", stringsAsFactors = FALSE)

df_combined <- rbind(df_left, df_right)

df_combined <- distinct(df_combined)

colnames(df_combined)

df_combined <- subset(df_combined, select = -c(Short.Reference, Long.Reference, DOI, Keywords, Organization, NCEI.Accession.Number, NCEI.Accession.Link) )
names(df_combined)[names(df_combined) == "Microplastics.Measurement..density."] <- "Microplastics.Measurement.Density"

colnames(df_combined)

write.csv(df_combined, "plastic-drift-app/datasources/cleaned_microplastics.csv", row.names = FALSE)
