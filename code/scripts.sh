Rnosave step_05_zjus1_fit_ML_rev.R -J ZJUS1 --array=1-153 --nodes=1 --ntasks=1 --cpus-per-task=8 -t 3-0:00 --mem=10G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu  -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave step_05_zjus1s2_fit_ML_rev.R -J ZJUS1S2 --array=1-153 --nodes=1 --ntasks=1 --cpus-per-task=8 -t 3-0:00 --mem=10G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu  -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave step_05_IU_fit_ML_rev.R -J MLIU --array=1-32 --nodes=1 --ntasks=1 --cpus-per-task=8 -t 3-0:00 --mem=15G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu  -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave step_05b_zjus1s2_fit_ML_extended.R -J ZJUS1S2EXT --array=1-153 --nodes=1 --ntasks=1 --cpus-per-task=8 -t 3-0:00 --mem=10G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu  -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err
Rnosave step_05b_zjus1_fit_ML_extended.R -J ZJUS1EXT --array=1-153 --nodes=1 --ntasks=1 --cpus-per-task=8 -t 3-0:00 --mem=10G --mail-type=FAIL,END --mail-user=lkoffma2@jh.edu  -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

