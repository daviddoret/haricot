# Pseudo constants, by alphabetic order.

if(any(objects() == "ALGO_PREFIX")){
  rm(ALGO_PREFIX);
};
ALGO_PREFIX <- "a";
#lockBinding("ALGO_PREFIX", globalenv());

##### LEGACY. MIGRATE EVERYTHING TO FUTILE.LOGGER #####

if(any(objects() == "LOG_ERROR")){
  rm(LOG_ERROR);
};
LOG_ERROR <- futile.logger::ERROR;

if(any(objects() == "LOG_FATAL")){
  rm(LOG_FATAL);
};
LOG_FATAL <- futile.logger::FATAL;

if(any(objects() == "LOG_INFO")){
  rm(LOG_INFO);
};
LOG_INFO <- futile.logger::INFO;

if(any(objects() == "LOG_TRACE")){
  rm(LOG_TRACE);
};
LOG_TRACE <- futile.logger::TRACE;

if(any(objects() == "LOG_WARN")){
  rm(LOG_WARN);
};
LOG_WARN <- futile.logger::WARN;

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


if(any(objects() == "VECTOR_DIRECTION_HORIZONTAL")){
  rm(VECTOR_DIRECTION_HORIZONTAL);
};
VECTOR_DIRECTION_HORIZONTAL <- 1;

if(any(objects() == "VECTOR_DIRECTION_VERTICAL")){
  rm(VECTOR_DIRECTION_VERTICAL);
};
VECTOR_DIRECTION_VERTICAL <- 2;
