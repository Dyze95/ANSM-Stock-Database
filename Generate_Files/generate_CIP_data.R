CIS <- read.delim(file = "CIS_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="latin1")
CIP <-  read.delim(file = "CIS_CIP_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="latin1")
COMPO <- read.delim(file = "CIS_COMPO_bdpm.txt", quote = "", fill = F, stringsAsFactors = F, header = F, fileEncoding="latin1")
CODEX <- read.csv2("Codex_China.csv", stringsAsFactors = F, fileEncoding="latin1")

write.table(CIS[which(!CIS$V1 %in% CIP$V1),], "in_CIS_bdpm_but_not_in_CIS_CIP_bdpm.csv", sep=";")
write.table(CIP[which(!CIP$V1 %in% CIS$V1),], "in_CIS_CIP_bdpm_but_not_in_CIS_bdpm.csv", sep=";")
write.table(CIS[which(!CIS$V1 %in% COMPO$V1),], "in_CIS_bdpm_but_not_in_CIS_COMPO_bdpm.csv", sep=";")

getDCI <- function(codeCIS) {
  if (length(codeCIS) <= 1) {
    R = COMPO[COMPO$V1==codeCIS & COMPO$V7 == "SA",]
    R = R[order(R$V4),]
    R$V4
  } else {
    sapply(codeCIS, getDCI)
  }
}

getDosage <- function(codeCIS) {
  if (length(codeCIS) <= 1) {
    sub <- COMPO[COMPO$V1==codeCIS,]
    sub = sub[order(sub$V4),]
    sapply(sort(unique(sub$V8)), function(i) {
      if (length(sub$V7[sub$V7=="FT" & sub$V8 == i])>0 && sub$V7[sub$V7=="FT" & sub$V8 == i] != "") {
        sub$V5[sub$V7=="FT" & sub$V8 == i]
      } else {
        sub$V5[sub$V7=="SA" & sub$V8 == i]
      }
    })
  } else {
    sapply(codeCIS, getDCI)
  }
}

CIS$DCI <- sapply(CIS$V1, function(i) {stri_trans_general(paste0(getDCI(i), collapse = ";"), "Latin-ASCII")})
CIS$Dosage <- sapply(CIS$V1, function(i) {paste0(getDosage(i), collapse = ";")})
CIS$Dosage <- sapply(CIS$Dosage, function(dosage) { gsub("(?:(,[0-9]*?[1-9]+)|,)0*", "\\1",
                                                         gsub(" ", "", dosage))})
                  
CIP_data <- data.frame(CIP7 = CIP$V2[CIP$V5 == "Déclaration de commercialisation" & CIP$V1 %in% CIS$V1],
                       stringsAsFactors = F)
CIP_data$CIP13 <- sapply(CIP_data$CIP7, function(i) {CIP$V7[CIP$V2==i]})
CIP_data$CIS   <- sapply(CIP_data$CIP7, function(i) {CIP$V1[CIP$V2==i]})
CIP_data$DCI   <- sapply(CIP_data$CIS , function(i) {CIS$DCI[CIS$V1==i]})
CIP_data$Laboratoire <- sapply(CIP_data$CIS , function(i) {
  if(CIS$V6[CIS$V1 == i] == "Autorisation d'importation parallèle") {
    return(CIS$V11[CIS$V1 == i])
  } else {
    exploitant <- unique(CODEX$Exploitant[CODEX$Code.CIS == i])
    if(length(exploitant) == 1)
      return(gsub("^([A-Z/ a-z\\(\\)&0-9'-\\.]*?)( -|,).*", "\\1", exploitant))
    else
      return(NA)
  }
})
CIP_data$Specialite <- sapply(CIP_data$CIS, function(i) {CIS$V2[CIS$V1==i]})
CIP_data$Presentation <- sapply(CIP_data$CIP7, function(i) {CIP$V3[CIP$V2==i]})
CIP_data$Forme <- sapply(CIP_data$CIS, function(i) {CIS$V3[CIS$V1==i]})
CIP_data$Dosage <- sapply(CIP_data$CIS, function(i) {CIS$Dosage[CIS$V1==i]})
CIP_data$Unites <- sapply(CIP_data$CIP7, function(i) {stri_extract_first_regex(CIP$V3[CIP$V2==i], "[0-9]+")})
CIP_data$Unites[is.na(CIP_data$Unites)] <- 1

CIP_data$Dosage <- gsub("[^,0-9]([1-9])g", "\\1000mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("^([1-9])g", "\\1000mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("^([1-9]),([1-9])g", "\\1\\200mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("0,([1-9])g", "\\100mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("0,([1-9][0-9])g", "\\10mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("^([1-9][0-9][0-9])microgrammes", "0,\\1mg", CIP_data$Dosage)
CIP_data$Dosage <- gsub("^([1-9][0-9])microgrammes", "0,0\\1mg", CIP_data$Dosage)

# Corrections Florent & Victor
correction <- read.csv2(file = "../correctionsDosage.csv", stringsAsFactors = F)
for(i in correction$CIP7) {
  if (!is.null(CIP_data$Dosage[CIP_data$CIP7==i])) {
    CIP_data$Dosage[CIP_data$CIP7==i] <- correction$Dosage[correction$CIP7==i]
    CIP_data$Unites[CIP_data$CIP7==i] <- correction$Unites[correction$CIP7==i]
  }
}

# Corrections Laboratoires à double exploitant selon CODEX
corrections_exploitant <- data.frame(CIS = character(), Laboratoire = character())
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=60427957, Laboratoire="LIPOMED"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=60961427, Laboratoire="ASTRAZENCA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=61659061, Laboratoire="SANOFI AVENTIS FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=62433250, Laboratoire="CELGENE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=62708127, Laboratoire="SANOFI AVENTIS FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=63989385, Laboratoire="MENARINI FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=64050311, Laboratoire="MENARINI FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=64828545, Laboratoire="BIOGARAN"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=65020253, Laboratoire="SANOFI AVENTIS FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=65036357, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=65546488, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=65773258, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=65916489, Laboratoire="DAIICHI SANKYO FRANCE SAS"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=66332028, Laboratoire="KYOWA KIRIN PHARMA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=66664532, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=66669173, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=66865211, Laboratoire="IPSEN PHARMA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=67037162, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=67180242, Laboratoire="MENARINI FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=67668455, Laboratoire="BIOPROJET PHARMA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=67692513, Laboratoire="MYLAN"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=67767535, Laboratoire="SANOFI AVENTIS FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=68441909, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=68680154, Laboratoire="MENARINI FRANCE"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=68725005, Laboratoire="ASTRAZENECA"))
corrections_exploitant <- rbind(corrections_exploitant,
                                data.frame(CIS=68818192, Laboratoire="SANOFI AVENTIS FRANCE"))
for(i in corrections_exploitant$CIS) {
  CIP_data$Laboratoire[CIP_data$CIS == i] <- corrections_exploitant$Laboratoire[corrections_exploitant$CIS == i]
}


write.table(CIP_data, "CIP_data.csv", sep=";", row.names=FALSE)

#several_exploitant_CIS<-c()
#for(i in unique(CIP_data$CIS)) {
#  if(length(unique(CODEX$Exploitant[CODEX$Code.CIS == i])) > 1) several_exploitant_CIS<-c(several_exploitant_CIS,i)
#}

#has_several_exploitant <- function(code_CIS) {
#  if(length(unique(CODEX$Exploitant[CODEX$Code.CIS == code_CIS])) > 1) T else F
#}
#has_no_exploitant <- function(code_CIS) {
#  if(length(unique(CODEX$Exploitant[CODEX$Code.CIS == code_CIS])) < 1) T else F
#}
#sum(sapply(unique(CIP_data$CIS), has_several_exploitant))
#sum(sapply(unique(CIP_data$CIS), has_no_exploitant))


