
For final version: 
------------------
Authors@R: c(
    person(given = "Fabian", family = "Falck", email = "fabian.falck@stats.ox.ac.uk", role = c("cre", "aut")),
    person(given = "Matthias", family = "Kormaksson", email = "matthias.kormaksson@novartis.com", role = "aut"), 
    person(given = "George", family = "Nicholson", email = "george.nicholson@stats.ox.ac.uk", role = "aut"))


Remembering
-----------
- check documentation: devtools::check_man
- check coverage: 
  - library(covr)
  - report()
<<<<<<< HEAD
- create reference manual: 
  - system("R CMD Rd2pdf .")
  - locally: system("R CMD Rd2pdf /Users/FabianFalck/Dropbox/Novartis/mlfa_package/")
  - on server: system("R CMD Rd2pdf /home/bdivdi.local/4tvcsw/multLinFacAnalys/mlfa")
=======
- create reference manual (Version 1: from source)
  - system("R CMD Rd2pdf /Users/FabianFalck/Dropbox/Novartis/mlfa")
- create reference manual (Version 2: from build):  TODO MUST INCLUDE Authors@R (see above)
  - system("R CMD build /Users/FabianFalck/Dropbox/Novartis/mlfa/")
  - system("tar zxvf /Users/FabianFalck/Dropbox/Novartis/mlfa/mlfa_0.1.tar.gz")
  - system("R CMD Rd2pdf /Users/FabianFalck/Dropbox/Novartis/mlfa/mlfa")
>>>>>>> a2ec3c414c40b48e356125842ce0628f41420654
- render README in html: 
  - rmarkdown::render("README.md")
  - rmarkdown::render("vignettes/mlfa_vignette.Rmd")




Good pointers: 
--------------
R package development: http://r-pkgs.had.co.nz/description.html -> see table of contents
tidyverse book: https://r4ds.had.co.nz/
Hadley Wickham style guide: https://style.tidyverse.org/
(Google style guide: https://google.github.io/styleguide/Rguide.html)
Luke: how to build a package: http://r-pkgs.had.co.nz
object orientation in R: S3, S4, RC (not copy on mute): http://adv-r.had.co.nz/OO-essentials.html ; http://adv-r.had.co.nz/R5.html


Assumptions: 

- working directory is set to folder mlfa


TODO once more time: 
- write more warnings
- improve documentation
- make more edgetest cases
- further checks in loadings that results are somewhat valid looking
- NA -> this case means we expext no error for this call -> TODO implement such a test case for every single test file 
- differentiate list(...) and c(...) more precisely everywhere
- everything copied over from old script estimate_...
- is sampling at all always necessary? 
- unify per_meas... and vars name
- more "positive" tests -> aim at ~250 tests
- # TODO: but: estimate A and Z independently for every dataset, merge Z, but do not merge A !!! (Z does not waste memory, but large A matrix would)
