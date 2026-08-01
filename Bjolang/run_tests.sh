#!/bin/bash

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Bjolang Test Runner ===${NC}"
echo ""

# Find all files starting with double digits in TestFiles/
TEST_FILES=$(ls TestFiles/[0-9][0-9]_*.bjo 2>/dev/null | sort)

if [ -z "$TEST_FILES" ]; then
    echo -e "${RED}No test files matching TestFiles/[0-9][0-9]_*.bjo found.${NC}"
    exit 1
fi

success_count=0
fail_compile_count=0
fail_run_count=0
skipped_count=0

declare -a compiled_success
declare -a compiled_failed
declare -a run_success
declare -a run_failed
declare -a skipped_list

for bjo_file in $TEST_FILES; do
    basename=$(basename "$bjo_file")
    
    # Check if this is the iterator file (user requested to skip)
    if [[ "$basename" == "08_iterators.bjo" ]]; then
        echo -e "${YELLOW}Skipping $basename (as requested)${NC}"
        echo "--------------------------------------------------"
        skipped_list+=("$basename")
        ((skipped_count++))
        continue
    fi
    
    exe_file="${bjo_file%.bjo}.exe"
    dll_file="${bjo_file%.bjo}.dll"
    
    echo -e "${BLUE}Processing $basename...${NC}"
    
    # 1. Remove previously generated exes and dlls
    if [ -f "$exe_file" ]; then
        echo "  Removing existing exe: $exe_file"
        rm -f "$exe_file"
    fi
    if [ -f "$dll_file" ]; then
        rm -f "$dll_file"
    fi
    
    # 2. Compile
    echo "  Compiling..."
    dotnet run "$bjo_file"
    compile_status=$?
    
    if [ $compile_status -ne 0 ]; then
        echo -e "  ${RED}Compilation FAILED for $basename${NC}"
        compiled_failed+=("$basename")
        ((fail_compile_count++))
        echo "--------------------------------------------------"
        continue
    fi
    
    compiled_success+=("$basename")
    echo -e "  ${GREEN}Compilation succeeded!${NC}"
    
    # Check if exe was actually generated
    if [ ! -f "$exe_file" ]; then
        echo -e "  ${YELLOW}No executable (.exe) generated (might be a library). Skipping execution.${NC}"
        echo "--------------------------------------------------"
        continue
    fi
    
    # 3. Run
    #
    # A test that reads from stdin gets its input from a `.in` file sitting
    # next to the source. Everything else is run against /dev/null: an
    # unattended suite must never be able to block waiting for a terminal.
    input_file="${bjo_file%.bjo}.in"
    if [ -f "$input_file" ]; then
        echo "  Running (stdin from $(basename "$input_file"))..."
        dotnet "$exe_file" < "$input_file"
    else
        echo "  Running..."
        dotnet "$exe_file" < /dev/null
    fi
    run_status=$?
    
    if [ $run_status -ne 0 ]; then
        echo -e "  ${RED}Execution FAILED for $basename (exit code: $run_status)${NC}"
        run_failed+=("$basename")
        ((fail_run_count++))
    else
        echo -e "  ${GREEN}Execution succeeded!${NC}"
        run_success+=("$basename")
        ((success_count++))
    fi
    echo "--------------------------------------------------"
done

# Print Summary Table
echo ""
echo -e "${BLUE}=== Summary ===${NC}"
echo -e "Total files found:  $(echo "$TEST_FILES" | wc -w)"
echo -e "Skipped:            $skipped_count"
echo -e "Compile failures:   $fail_compile_count"
echo -e "Execution failures: $fail_run_count"
echo -e "Successful runs:    $success_count"
echo ""

if [ ${#compiled_failed[@]} -ne 0 ]; then
    echo -e "${RED}Failed Compilation:${NC}"
    for f in "${compiled_failed[@]}"; do
        echo "  - $f"
    done
fi

if [ ${#run_failed[@]} -ne 0 ]; then
    echo -e "${RED}Failed Execution:${NC}"
    for f in "${run_failed[@]}"; do
        echo "  - $f"
    done
fi

if [ $fail_compile_count -eq 0 ] && [ $fail_run_count -eq 0 ]; then
    echo -e "${GREEN}All active tests compiled and ran successfully!${NC}"
else
    echo -e "${RED}Some tests failed.${NC}"
fi
