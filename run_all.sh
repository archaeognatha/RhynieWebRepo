#!/usr/bin/env bash
# ============================================================
# run_all.sh — regenerate Rhynie SLNs + trophospecies, rerun
# web_metrics on every web, then collate.
# Stop-gap until the Snakefile: each numbered block = one future rule.
#
# Run from the repo root (needed for julia --project=.):
#   N_REPS=10 bash run_all.sh      # quick test of the whole chain
#   bash run_all.sh 2>&1 | tee run_all_$(date +%Y%m%d_%H%M).log
# ============================================================

set -euo pipefail          # stop at the first command that fails

N_REPS=${N_REPS:-1000}     # override on the command line for a test run
BASE_SEED=20260928         # dataset k gets seed BASE_SEED + k (fixed order below)
METAWEB_DIR=data/rhynie    # metaweb_builder_rhynie.R writes <dsid>/guilds.csv, guild_matrix.csv
RUN_NICHE=false            # set true to regenerate niche nulls too

RHYNIE="rhynie_unlumped_complete rhynie_unlumped_terr rhynie_unlumped_aqu
        rhynie_lumped_complete   rhynie_lumped_terr   rhynie_lumped_aqu"
EMPIRICAL="deruiter_soil ecoweb other_modern"   # messel and digel_soil have their own blocks (3a, 3b)

metrics() {                # $1 = folder holding matrix_*.csv / speciesinfo_*.csv
  echo "--- web_metrics: $1"
  julia --project=. scripts/web_metrics.jl --in-dir "$1"
}

k=0

# ---- 1. Rhynie: SLNs -> trophospecies -> metrics ------------
for ds in $RHYNIE; do
  k=$((k + 1))
  echo "=== $ds  (seed $((BASE_SEED + k))) ==="
  julia --project=. scripts/sln_builder_rhynie.jl \
      --in-dir  "$METAWEB_DIR/$ds" \
      --out-dir "SLNs/$ds/raw" \
      --n-reps  "$N_REPS" --seed "$((BASE_SEED + k))"
  julia --project=. scripts/TrophSpLumper.jl --in-dir "SLNs/$ds/raw"   # writes SLNs/$ds/ts
  metrics "SLNs/$ds/raw"
  metrics "SLNs/$ds/ts"
done

# ---- 2. Niche nulls (optional), drawn from new Rhynie ts metrics
if [ "$RUN_NICHE" = true ]; then
  for res in unlumped lumped; do
    k=$((k + 1))
    echo "=== niche_$res  (seed $((BASE_SEED + k))) ==="
    Rscript scripts/niche_model_generator.R \
        --in-dir "SLNs/rhynie_${res}_complete/ts" \
        --n-reps "$N_REPS" --seed "$((BASE_SEED + k))"             # writes SLNs/niche_$res/raw
    julia --project=. scripts/TrophSpLumper.jl --in-dir "SLNs/niche_$res/raw"
    metrics "SLNs/niche_$res/raw"
    metrics "SLNs/niche_$res/ts"
  done
fi

# ---- 3a. Messel: rebuild the 6 webs -> trophospecies -> metrics
echo "=== messel ==="
Rscript scripts/sln_builder_Messel.R \
    --speciesinfo data/messel/speciesinfo_messel.csv \
    --links       data/messel/links_messel.csv \
    --out         SLNs/messel/raw
julia --project=. scripts/TrophSpLumper.jl --in-dir SLNs/messel/raw   # writes SLNs/messel/ts
#metrics SLNs/messel/raw
metrics SLNs/messel/ts

# ---- 3b. Digel: rebuild the 48 plot webs -> trophospecies -> metrics
echo "=== digel_soil ==="
julia --project=. scripts/sln_builder_digel_soil.jl \
    --in-dir  data/digel_soil \
    --out-dir SLNs/digel_soil/raw --create
julia --project=. scripts/TrophSpLumper.jl --in-dir SLNs/digel_soil/raw   # writes SLNs/digel_soil/ts
metrics SLNs/digel_soil/raw
metrics SLNs/digel_soil/ts

# ---- 3c. Other empirical webs: inputs unchanged, metrics only
for ds in $EMPIRICAL; do
  echo "=== $ds ==="
  metrics "SLNs/$ds/raw"
  metrics "SLNs/$ds/ts"
done

# ---- 4. Collate --------------------------------------------
Rscript scripts/collate_metrics.R     # CHECK: add whatever args you normally pass

echo "Done."
