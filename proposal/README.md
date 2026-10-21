# Book proposal: replication

`proposal_numbers.R` reproduces every figure in the book proposal for *What's Wrong with Mass Incarceration?* (Christopher Lewis and Adaner Usmani).

## Data

- **World History of Punishment Database** (Clegg, Spitz, and Usmani), vintage of 20 October 2026, as harmonized by the replication pipeline of *From Plantation to Prison* (Clegg and Usmani). The database and that pipeline are not yet public. This script will run once they are released.
- **IPUMS USA** (American Community Survey), tabulated by the same pipeline. It supplies the incarceration rates by race and education.
- **OECD Income Distribution Database**. The script downloads it directly.

## Running it

From the root of this repository:

```
Rscript proposal/proposal_numbers.R
```

The script expects the *From Plantation to Prison* repository at `../p2p`. Set the `P2P_DIR` environment variable if it is somewhere else. It needs only the R package `data.table`.

It prints each figure next to a short description and writes them all to `proposal/output/proposal_numbers.csv`.
