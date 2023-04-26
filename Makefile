readme:
		R -e "knitr::knit('README.Rmd', 'README.md')"

load_data:
		R -e "path <- here::here(); source(file.path(path, 'data-raw/social_classes/legacy_social_classes.R')); source(file.path(path, 'data-raw/hierarchy_isco.R')); source(file.path(path, 'data-raw/social_classes.R')); devtools::load_all()"
