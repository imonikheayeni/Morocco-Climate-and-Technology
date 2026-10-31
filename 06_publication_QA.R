# Publication QA: fail on classification or index errors.
# Run after 01-05. Not a substitute for independent validity checks.
stopifnot(exists("path_ml"),exists("path_tables"))
index <- readr::read_csv(file.path(path_ml,"morocco_ctvi_adm2_index_typologies.csv"),show_col_types=FALSE)
stopifnot(nrow(index)==75L,dplyr::n_distinct(index$adm2_id)==75L)
stopifnot(all(is.finite(index$ctvi_score)))
stopifnot(all(index$ctvi_score>=0 & index$ctvi_score<=1))
stopifnot(max(abs(index$ctvi_score-(index$climate_exposure_score+index$technology_gap_score)/2))<1e-10)
expected <- c("Smart city / urban core"=11L,"Urban edge / peri-urban"=34L,"Rural / lower-density"=30L)
actual <- table(as.character(index$environment_type))
stopifnot(identical(as.integer(actual[names(expected)]),as.integer(expected)))
if (any(!is.finite(index$population)) || any(index$population < 1000,na.rm=TRUE)) {
 warning("Unverified population values detected. Do not release a population-weighted priority map.")
}
q <- readr::read_csv(file.path(path_tables,"environment_kruskal_wallis_tests.csv"),show_col_types=FALSE)
stopifnot(nrow(q)==3L,all(is.finite(q$statistic)),all(is.finite(q$p_value)))
message("Numerical integrity checks passed; empirical indicator validity still requires independent review.")
