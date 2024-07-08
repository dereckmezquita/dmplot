#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _dmplot_bb(SEXP, SEXP, SEXP);
extern SEXP _dmplot_ema(SEXP, SEXP, SEXP);
extern SEXP _dmplot_macd(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dmplot_mom(SEXP, SEXP);
extern SEXP _dmplot_monte_carlo(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_dmplot_bb", (DL_FUNC) &_dmplot_bb, 3},
    {"_dmplot_ema", (DL_FUNC) &_dmplot_ema, 3},
    {"_dmplot_macd", (DL_FUNC) &_dmplot_macd, 5},
    {"_dmplot_mom", (DL_FUNC) &_dmplot_mom, 2},
    {"_dmplot_monte_carlo", (DL_FUNC) &_dmplot_monte_carlo, 4},
    {NULL, NULL, 0}
};

void R_init_dmplot(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}