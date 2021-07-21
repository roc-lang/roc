#!/usr/bin/env bash

# script to return exit code 1 if benchmarks have regressed
# script assumes we are in repo root

ulimit -s unlimited

LOG_FILE="bench_log.txt"

# one dry run, three regular runs
NR_REPEATS=4

for ctr in `seq 1 $NR_REPEATS`;
    do
        if [ $ctr -eq 1 ]; then
            export BENCH_DRY_RUN=1
        else
            export BENCH_DRY_RUN=0
        fi
        #
        # <run benchmarks>
        #
        # delete criterion folder to remove old benchmark data (ignore error if folder does not exist)
        rm -rf bench-folder-trunk/target/criterion
        rm -rf bench-folder-branch/target/criterion
        
        cd bench-folder-trunk
        # benchmark trunk
        ./target/release/deps/time_bench --bench
        cd ..

        # move benchmark results so they can be compared later
        cp -r bench-folder-trunk/target/criterion bench-folder-branch/target/

        cd bench-folder-branch

        # ignore error if file does not exist
        rm -f $LOG_FILE
        touch $LOG_FILE

        FULL_CMD=" ./target/release/deps/time_bench --bench"
        echo $FULL_CMD

        script -efq $LOG_FILE -c "$FULL_CMD"

        EXIT_CODE=$?

        if [ $ctr -eq 1 ]; then

            rm -f bench_sha_sums.txt
            touch bench_sha_sums.txt

            # on new branch
            find examples/benchmarks -executable -type f | while read exec_filename; do
                sha1sum $exec_filename >> bench_sha_sums.txt
            done

            # on trunk
            find ../bench-folder-trunk/examples/benchmarks -executable -type f | while read exec_filename; do
                sha1sum $exec_filename >> bench_sha_sums.txt
            done

            # keep only unique sha sums
            sort bench_sha_sums.txt | uniq -u > bench_sha_sums.txt

            if [ $(cat bench_sha_sums.txt | wc -l) -eq 0 ]; then
                echo ""
                echo ""
                echo "No benchmark executables have changed through changes from this PR."
                echo ""
                echo ""
                exit 0
            else
                cd ..
                # go to next iteration of loop
                continue
            fi

        fi
        #
        # </run benchmarks>
        #

        #
        # <save which tests regressed>
        #
        REGRESSED_TESTS_FILE_NAME="regressed_$ctr.txt"
        
        grep -B3 "regressed" $LOG_FILE | sed 's/\x1B\[[0-9;]\{1,\}[A-Za-z]//g' | grep -o "\".*\"" | rev | cut -d' ' -f1 | rev > $REGRESSED_TESTS_FILE_NAME

        #
        # </save which tests regressed>
        #

        if [ $(cat $REGRESSED_TESTS_FILE_NAME | wc -l) -gt 0 ]; then 
            if [ $ctr -ne $NR_REPEATS ]; then
                echo ""
                echo ""
                echo "------<<<<<<>>>>>>------"
                echo "Benchmark regression detected for:"
                cat $REGRESSED_TESTS_FILE_NAME
                echo "Running benchmarks again to confirm regression is real..."
                echo "------<<<<<<>>>>>>------"
                echo ""
                echo ""
            else
                echo ""
                echo ""
                echo "------<<<<<<!!!!!!>>>>>>------"
                echo "Benchmarks were run $NR_REPEATS times and a regression was detected every time for the following benchmarks:"
                cat regressed_*.txt > regressed.txt
                sort regressed.txt | uniq -d 
                echo "------<<<<<<!!!!!!>>>>>>------"
                echo ""
                echo ""
                exit 1
            fi
        else
            echo ""
            echo "Benchmark execution finished with exit code: $EXIT_CODE."
            echo ""
            exit $EXIT_CODE
        fi

        cd ..
    done  