createConclass <- function(statusVec, decayVec){
  ifelse(statusVec == 2 & decayVec %in%  1:2, 3,
  ifelse(statusVec == 2 & decayVec %in%  3:4, 4,
  ifelse(statusVec == 2 & decayVec %in%  5:6, 5,
  ifelse(statusVec == 3, 6,
  ifelse(statusVec == 4 & decayVec %in%  1:2, 3,
  ifelse(statusVec == 4 & decayVec %in%  3:4, 4,
  ifelse(statusVec == 4 & decayVec %in%  5:6, 5,
  ifelse(statusVec == 4 & decayVec == 7, 8,
  ifelse(statusVec == 6, 7,
  ifelse(statusVec == 5, 7, NA))))))))))
}
