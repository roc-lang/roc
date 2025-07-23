app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import cli.Arg exposing [Arg]
import cli.File
import cli.Cmd

# This script runs `./zig-out/bin/roc check src/PROFILING/bench_repeated_check.roc --time --no-cache` 5 times,
# extracts timing data from the output, calculates median values, and appends results to out_file

main! : List Arg => Result {} _
main! = |raw_args|
    args = List.map(raw_args, Arg.display)

    # get the second argument, the first is the executable's path
    out_file = when List.get(args, 1) |> Result.map_err(|_| ZeroArgsGiven) is
        Err(ZeroArgsGiven) ->
            Err(Exit(1, "Error ZeroArgsGiven:\n\tI expected one argument, but I got none.\n\tRun the app like this: `roc exec_bench.roc -- output.txt`"))?
        Ok(first_arg) ->
            first_arg
    # Run the command 5 times and collect outputs
    outputs = List.range({ start: At(0), end: Before(5) })
        |> List.map_try!(|_| run_benchmark_command!({}))?

    # Parse all outputs to extract timing data
    all_timing_data = List.map_try(outputs, parse_output)?

    # Validate phases are consistent across all runs
    validate_phases(all_timing_data)?

    # Calculate median values
    median_results = calculate_medians(all_timing_data)

    # calculate bench file hash so we're aware of changes
    bench_file_hash_out = run_cmd_w_output!("sha256sum", ["src/PROFILING/bench_repeated_check.roc"])?
    bench_file_hash =
        bench_file_hash_out
        |> Str.split_on(" ")
        |> List.get(0)?
    
    # Get the current commit hash
    commit_hash_out = run_cmd_w_output!("git", ["rev-parse", "HEAD"])?
    commit_hash = Str.trim(commit_hash_out)

    # Get zig version
    zig_version_out = run_cmd_w_output!("zig", ["version"])?
    zig_version = Str.trim(zig_version_out)

    # Get operating system with version (works on both Linux and macOS)
    operating_system_out = run_cmd_w_output!("uname", ["-sr"])?
    operating_system = Str.trim(operating_system_out)

    # Create the AllBenchmarkData record
    benchmark_data : AllBenchmarkData
    benchmark_data = {
        bench_file_hash,
        commit_hash,
        zig_version,
        operating_system,
        median_results,
    }

    append_results_to_file!(benchmark_data, out_file)?

    Ok({})

run_benchmark_command! : {} => Result Str _
run_benchmark_command! = |{}|
    run_cmd_w_output!("./zig-out/bin/roc", ["check", "src/PROFILING/bench_repeated_check.roc", "--time", "--no-cache"])

parse_output : Str -> Result TimingData _
parse_output = |output|
    lines = Str.split_on(output, "\n")
    
    # Extract total time from "No errors found in XXX ms"
    total_time_ms = extract_total_time(lines)?
    
    # Extract phase timings
    phase_timings = extract_phase_timings(lines)?
    
    Ok({ total_time_ms, phase_timings })

extract_total_time : List Str -> Result Dec _
extract_total_time = |lines|
    when List.find_first(lines, |line| Str.contains(line, "No errors found in")) is
        Ok line ->
            # Parse "No errors found in 772.4 ms for src/PROFILING/bench_repeated_check.roc"
            parts = Str.split_on(line, " ")
            total_time_str = List.get(parts, 4) ? |_| Err(SplitListShouldHaveAtLeastFiveElts(parts))

            when Str.to_dec(total_time_str) is
                Ok time -> Ok(time)
                Err _ -> Err(InvalidTotalTime("Could not parse total time: ${total_time_str}"))

        Err _ -> Err(InvalidTotalTime("Could not find 'No errors found in' line"))

extract_phase_timings : List Str -> Result (List PhaseTime) _
extract_phase_timings = |lines|
    expected_phases = [
        "tokenize + parse:",
        "canonicalize:",
        "can diagnostics:",
        "type checking:",
        "type checking diagnostics:",
    ]
    
    phase_times = List.map_try(expected_phases, |phase_name| extract_phase_time(lines, phase_name))?
    
    Ok(phase_times)

extract_phase_time : List Str, Str -> Result PhaseTime _
extract_phase_time = |lines, phase_name|
    when List.find_first(lines, |line| Str.contains(line, phase_name)) is
        Ok line ->
            # Parse "  tokenize + parse:             37.6 ms  (37602963 ns)"
            parts = Str.split_on(line, "(")
            when List.get(parts, 1) is
                Ok ns_part ->
                    ns_str = Str.replace_first(ns_part, " ns)", "") |> Str.trim
                    when Str.to_u64(ns_str) is
                        Ok ns -> Ok({ phase: phase_name, time_ns: ns })
                        Err _ -> Err(InvalidPhaseTime("Could not parse ns value: ${ns_str}"))
                Err _ -> Err(InvalidPhaseTime("Could not find ns value in line: ${line}"))
        Err _ -> Err(InvalidPhaseTime("Could not find phase '${phase_name}' in:\n\t${Str.join_with(lines, "\n")}"))

validate_phases : List TimingData -> Result {} _
validate_phases = |all_timing_data|
    when List.first(all_timing_data) is
        Ok first_data ->
            if List.len(first_data.phase_timings) != 5 then
                Err(InvalidPhaseCount("Expected 5 phases, found ${Num.to_str(List.len(first_data.phase_timings))}"))
            else
                expected_phases = [
                    "tokenize + parse:",
                    "canonicalize:",
                    "can diagnostics:",
                    "type checking:",
                    "type checking diagnostics:",
                ]
                
                actual_phases = List.map(first_data.phase_timings, |pt| pt.phase)
                
                if actual_phases == expected_phases then
                    Ok({})
                else
                    Err(InvalidPhaseOrder("Phases not in expected order"))
        Err _ -> Err(NoTimingData("No timing data found"))

calculate_medians : List TimingData -> MedianResults
calculate_medians = |all_timing_data|
    # Extract all total times and calculate median
    total_times = List.map(all_timing_data, |data| data.total_time_ms)
    median_total = calculate_median(total_times)
    
    # Extract times for each phase and calculate medians
    phase_medians = List.range({ start: At(0), end: Before(5) })
        |> List.map(
            |phase_index|
                phase_times = List.map(
                    all_timing_data,
                    |data|
                        when List.get(data.phase_timings, phase_index) is
                            Ok phase_time -> phase_time.time_ns
                            Err _ -> 0  # This shouldn't happen due to validation
                )
                calculate_median_u64(phase_times)
        )
    
    {
        total_median_ms: median_total,
        tokenize_parse_median_ns: List.get(phase_medians, 0) |> Result.with_default(0),
        canonicalize_median_ns: List.get(phase_medians, 1) |> Result.with_default(0),
        can_diagnostics_median_ns: List.get(phase_medians, 2) |> Result.with_default(0),
        type_checking_median_ns: List.get(phase_medians, 3) |> Result.with_default(0),
        type_checking_diagnostics_median_ns: List.get(phase_medians, 4) |> Result.with_default(0),
    }

calculate_median : List Dec -> Dec
calculate_median = |values|
    sorted = List.sort_asc(values)
    len = List.len(sorted)
    middle_index = len // 2
    
    when List.get(sorted, middle_index) is
        Ok median -> median
        Err _ -> 0

calculate_median_u64 : List U64 -> U64
calculate_median_u64 = |values|
    sorted = List.sort_asc(values)
    len = List.len(sorted)
    middle_index = len // 2
    
    when List.get(sorted, middle_index) is
        Ok median -> median
        Err _ -> 0

append_results_to_file! : AllBenchmarkData, Str => Result {} _
append_results_to_file! = |benchmark_data, output_file_path_str|
    content = 
        """
        
        commit: ${benchmark_data.commit_hash}
        bench_file_hash: ${benchmark_data.bench_file_hash}
        zig_version: ${benchmark_data.zig_version}
        operating_system: ${benchmark_data.operating_system}

        tokenize + parse:             ${Num.to_str(benchmark_data.median_results.tokenize_parse_median_ns)} ns
        canonicalize:                 ${Num.to_str(benchmark_data.median_results.canonicalize_median_ns)} ns
        can diagnostics:              ${Num.to_str(benchmark_data.median_results.can_diagnostics_median_ns)} ns
        type checking:                ${Num.to_str(benchmark_data.median_results.type_checking_median_ns)} ns
        type checking diagnostics:    ${Num.to_str(benchmark_data.median_results.type_checking_diagnostics_median_ns)} ns

        total:                        ${Num.to_str(benchmark_data.median_results.total_median_ms)} ms
        ------------------------------
        """
    
    # Read existing content if file exists
    existing_content = File.read_utf8!(output_file_path_str) |> Result.with_default("")
    
    # Append new content
    new_content = Str.concat(existing_content, content)
    
    File.write_utf8!(new_content, output_file_path_str)?
    
    Ok({})

# Type definitions
TimingData : {
    total_time_ms : Dec,
    phase_timings : List PhaseTime,
}

PhaseTime : {
    phase : Str,
    time_ns : U64,
}

MedianResults : {
    total_median_ms : Dec,
    tokenize_parse_median_ns : U64,
    canonicalize_median_ns : U64,
    can_diagnostics_median_ns : U64,
    type_checking_median_ns : U64,
    type_checking_diagnostics_median_ns : U64,
}

AllBenchmarkData : {
    bench_file_hash : Str,
    commit_hash : Str,
    zig_version : Str,
    operating_system : Str,
    median_results : MedianResults,
}

run_cmd_w_output! : Str, List Str => Result Str [BadCmdOutput(Str)]_
run_cmd_w_output! = |cmd_str, args|
    cmd_out =
        Cmd.new(cmd_str)
        |> Cmd.args(args)
        |> Cmd.output!()

    stdout_utf8 = Str.from_utf8_lossy(cmd_out.stdout)

    when cmd_out.status is
        Ok(0) ->
            Ok(stdout_utf8)
        _ ->
            stderr_utf8 = Str.from_utf8_lossy(cmd_out.stderr)
            err_data =
                """
                Cmd `${cmd_str} ${Str.join_with(args, " ")}` failed:
                - status: ${Inspect.to_str(cmd_out.status)}
                - stdout: ${stdout_utf8}
                - stderr: ${stderr_utf8}
                """

            Err(BadCmdOutput(err_data))

# Test functions
expect
    test_lines = [
        "  tokenize + parse:             37.6 ms  (37602963 ns)",
        "  canonicalize:                 723.5 ms  (723512838 ns)",
        "  other line",
    ]
    
    result = extract_phase_time(test_lines, "tokenize + parse:")
    expected = Ok({ phase: "tokenize + parse:", time_ns: 37602963 })
    
    result == expected

expect
    test_lines = [
        "  canonicalize:                 723.5 ms  (723512838 ns)",
        "  type checking:                8.2 ms  (8218657 ns)",
    ]
    
    result = extract_phase_time(test_lines, "canonicalize:")
    expected = Ok({ phase: "canonicalize:", time_ns: 723512838 })
    
    result == expected

expect
    test_lines = [
        "  tokenize + parse:             37.6 ms  (37602963 ns)",
        "  other line",
    ]
    
    result = extract_phase_time(test_lines, "missing_phase:")
    
    when result is
        Err _ -> Bool.true
        Ok _ -> Bool.false

expect
    test_lines = [
        "  tokenize + parse:             37.6 ms  (invalid ns)",
    ]
    
    result = extract_phase_time(test_lines, "tokenize + parse:")
    
    when result is
        Err _ -> Bool.true
        Ok _ -> Bool.false

expect
    test_output = 
        """
        No errors found in 772.4 ms for src/PROFILING/bench_repeated_check.roc

        Timing breakdown:
          tokenize + parse:             37.6 ms  (37602963 ns)
          canonicalize:                 723.5 ms  (723512838 ns)
          can diagnostics:              0.0 ms  (1008 ns)
          type checking:                8.2 ms  (8218657 ns)
          type checking diagnostics:    0.0 ms  (300 ns)
        """
    
    result = parse_output(test_output)
    
    expected = Ok({
        total_time_ms: 772.4dec,
        phase_timings: [
            { phase: "tokenize + parse:", time_ns: 37602963 },
            { phase: "canonicalize:", time_ns: 723512838 },
            { phase: "can diagnostics:", time_ns: 1008 },
            { phase: "type checking:", time_ns: 8218657 },
            { phase: "type checking diagnostics:", time_ns: 300 },
        ],
    })
    
    result == expected

expect
    test_lines = [
        "No errors found in 772.4 ms for src/PROFILING/bench_repeated_check.roc",
        "",
        "Timing breakdown:",
    ]
    
    result = extract_total_time(test_lines)
    result == Ok(772.4dec)

expect
    test_values = [1.0, 3.0, 2.0, 5.0, 4.0]
    result = calculate_median(test_values)
    result == 3.0dec

expect
    test_values = [1, 3, 2, 5, 4]
    result = calculate_median_u64(test_values)
    result == 3

expect
    test_timing_data = [
        {
            total_time_ms: 772.4,
            phase_timings: [
                { phase: "tokenize + parse:", time_ns: 37602963 },
                { phase: "canonicalize:", time_ns: 723512838 },
                { phase: "can diagnostics:", time_ns: 1008 },
                { phase: "type checking:", time_ns: 8218657 },
                { phase: "type checking diagnostics:", time_ns: 300 },
            ],
        },
    ]
    
    result = validate_phases(test_timing_data)
    result == Ok({})