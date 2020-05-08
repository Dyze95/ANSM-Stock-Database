get_mg_from_dosage <- function(dosage) {
  if(grepl("^([0-9,]+)mg$",dosage)){
    return(as.numeric(gsub(",",".",gsub("^([0-9,]+)mg$", "\\1", dosage))))
  } else if(grepl("^([0-9,]+)mg/mL;([0-9,]+)mL$", dosage)) {
    c <- as.numeric(gsub(",",".",gsub("^([0-9,]+)mg/mL;([0-9,]+)mL$", "\\1", dosage)))
    v <- as.numeric(gsub(",",".",gsub("^([0-9,]+)mg/mL;([0-9,]+)mL$", "\\2", dosage)))
    return(c*v)
  } else {
    return(NA)
  }
}
