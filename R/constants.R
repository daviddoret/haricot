# Pseudo constants, by alphabetic order.

if(any(objects() == "ALGO_PREFIX")){
  rm(ALGO_PREFIX);
};
ALGO_PREFIX <- "a";
#lockBinding("ALGO_PREFIX", globalenv());

if(any(objects() == "INPUT_PREFIX")){
  rm(INPUT_PREFIX);
};
INPUT_PREFIX <- "i";
#lockBinding("INPUT_PREFIX", globalenv());

if(any(objects() == "NAMESPACE_SEPARATOR")){
  rm(NAMESPACE_SEPARATOR);
};
NAMESPACE_SEPARATOR <- ".";
#lockBinding("NAMESPACE_SEPARATOR", globalenv());

if(any(objects() == "NOBIT_PREFIX")){
  rm(NOBIT_PREFIX);
};
NOBIT_PREFIX <- "x";
#lockBinding("NOBIT_PREFIX", globalenv());

if(any(objects() == "OUTPUT_PREFIX")){
  rm(OUTPUT_PREFIX);
};
OUTPUT_PREFIX <- "o";
#lockBinding("OUTPUT_PREFIX", globalenv());

