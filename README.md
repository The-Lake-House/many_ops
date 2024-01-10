# many-ops

## Usage

    (cd many-inserts ; ./run) && (cd ../many-deletes ; ./run) && (cd ../many-updates ; ./run) && (cd ../many-inserts-partitioned ; ./run)

    ./collect results/raw results/processed

    ./plot results/processed results/plots pdf
