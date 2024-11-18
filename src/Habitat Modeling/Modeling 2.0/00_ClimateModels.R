modelsclimate <- list(
  . ~ . + FFP,
  . ~ . + MAP,
  . ~ . + TD + CMD,
  . ~ . + bio15 + bio9,
  . ~ . + MAP + FFP + bio15 + bio9,
  . ~ . + MAP + TD + CMD + bio15 + bio9,
  . ~ . + MAP + FFP + TD + CMD + bio15 + bio9)
