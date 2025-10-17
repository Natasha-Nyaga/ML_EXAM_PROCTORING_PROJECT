############################################################################
# Script to reproduce the dissertation results
# (Kevin Killourhy)
############################################################################

R -e "source('r/run-ch-litrev.R'); run()"
R -e "source('r/run-ch-lmm.R'); run()"
R -e "source('r/run-ch-bench.R'); run()"
R -e "source('r/run-ch-trait.R'); run()"
R -e "source('r/run-ch-factor.R'); run()"
