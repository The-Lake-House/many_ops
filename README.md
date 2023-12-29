# many-ops

## Usage

    # Trino
    cd many-inserts ; ./run_trino && cd ../many-deletes ; ./run_trino && cd ../many-updates ; ./run_trino

    # Spark
    cd many-inserts ; ./run_spark && cd ../many-deletes ; ./run_spark && cd ../many-updates ; ./run_spark && cd ../many-inserts-partitioned ; ./run_spark

    ./collect results/raw results/processed

    ./plot results/processed results/plots pdf
