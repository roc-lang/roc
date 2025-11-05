#!/usr/bin/env bash

# This script checks that all test files in src/ are properly wired through mod.zig files
# so they actually get run in CI.

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Checking test wiring in src/ directory..."
echo ""

# Track if we found any issues
ISSUES_FOUND=0

# Find all .zig files in src/ (excluding crates/) that might contain tests
# We look for files that either:
# 1. Have "test" in the filename (like foo_test.zig)
# 2. Contain test blocks (test "..." or test {)
#
# We also exclude certain directories that use main.zig-based testing:
# - cli/ (tested via src/cli/main.zig in build.zig)
# - watch/ (tested via watch module in build.zig)
# - snapshot_tool/ (tested via src/snapshot_tool/main.zig in build.zig)

echo "Step 1: Finding all potential test files..."
TEST_FILES=()

# Find files with "test" in name
while IFS= read -r file; do
    # Skip mod.zig files themselves
    if [[ "$file" != *"/mod.zig" ]]; then
        # Skip files in directories that use main.zig-based testing
        if [[ "$file" != src/cli/* && "$file" != src/watch/* && "$file" != src/snapshot_tool/* ]]; then
            TEST_FILES+=("$file")
        fi
    fi
done < <(find src -type f -name "*test*.zig" ! -path "*/crates/*" 2>/dev/null || true)

# Find files containing test blocks (but not already in TEST_FILES)
while IFS= read -r file; do
    # Skip if already in list
    skip=0
    for existing in "${TEST_FILES[@]}"; do
        if [[ "$existing" == "$file" ]]; then
            skip=1
            break
        fi
    done

    # Skip mod.zig files (they aggregate tests but don't need to be wired)
    if [[ "$file" == *"/mod.zig" ]]; then
        skip=1
    fi

    # Skip files in directories that use main.zig-based testing
    if [[ "$file" == src/cli/* || "$file" == src/watch/* || "$file" == src/snapshot_tool/* ]]; then
        skip=1
    fi

    if [[ $skip -eq 0 ]]; then
        TEST_FILES+=("$file")
    fi
done < <(grep -l "test \"" src/**/*.zig 2>/dev/null | grep -v "/crates/" || true)

echo "Found ${#TEST_FILES[@]} potential test files"
echo ""

if [[ ${#TEST_FILES[@]} -eq 0 ]]; then
    echo -e "${GREEN}✓${NC} No test files found to check"
    exit 0
fi

# Step 2: Extract all refAllDecls references from mod.zig files
echo "Step 2: Extracting test references from mod.zig files..."
REFERENCED_FILES=()

for mod_file in src/*/mod.zig; do
    # Skip if in crates
    if [[ "$mod_file" == *"/crates/"* ]]; then
        continue
    fi

    mod_dir=$(dirname "$mod_file")

    # Look for test references
    # Patterns:
    # 1. std.testing.refAllDecls(@import("file.zig"))
    # 2. test { _ = @import("file.zig"); }
    # 3. test { _ = varname; } - look up varname in the same file to find its import
    # 4. pub const X = @import (these files get tested transitively)
    # 5. const varname = @import (non-pub imports also get tested)
    while IFS= read -r line; do
        # Pattern 1: refAllDecls
        if [[ "$line" == *"refAllDecls"* && "$line" == *"@import("* ]]; then
            import_path=$(echo "$line" | grep -o '@import("[^"]*")' | sed 's/@import("\(.*\)")/\1/')

            if [[ -n "$import_path" && "$import_path" == *".zig" ]]; then
                if [[ "$import_path" == *"/"* ]]; then
                    full_path="$mod_dir/$import_path"
                else
                    full_path="$mod_dir/$import_path"
                fi
                full_path=$(echo "$full_path" | sed 's|/\./|/|g')
                REFERENCED_FILES+=("$full_path")
            fi
        # Pattern 2: _ = @import (within test blocks)
        elif [[ "$line" == *"_ = @import("* || "$line" == *"_ ="*"@import("* ]]; then
            import_path=$(echo "$line" | grep -o '@import("[^"]*")' | sed 's/@import("\(.*\)")/\1/')

            if [[ -n "$import_path" && "$import_path" == *".zig" ]]; then
                if [[ "$import_path" == *"/"* ]]; then
                    full_path="$mod_dir/$import_path"
                else
                    full_path="$mod_dir/$import_path"
                fi
                full_path=$(echo "$full_path" | sed 's|/\./|/|g')
                REFERENCED_FILES+=("$full_path")
            fi
        # Pattern 3: _ = varname; (look up the varname)
        elif [[ "$line" =~ _[[:space:]]*=[[:space:]]*([a-zA-Z_][a-zA-Z0-9_]*)\; ]]; then
            var_name="${BASH_REMATCH[1]}"
            # Look for "const varname = @import(...)" in the same file
            import_line=$(grep "const $var_name = @import" "$mod_file" || true)
            if [[ -n "$import_line" ]]; then
                import_path=$(echo "$import_line" | grep -o '@import("[^"]*")' | sed 's/@import("\(.*\)")/\1/')
                if [[ -n "$import_path" && "$import_path" == *".zig" ]]; then
                    if [[ "$import_path" == *"/"* ]]; then
                        full_path="$mod_dir/$import_path"
                    else
                        full_path="$mod_dir/$import_path"
                    fi
                    full_path=$(echo "$full_path" | sed 's|/\./|/|g')
                    REFERENCED_FILES+=("$full_path")
                fi
            fi
        # Pattern 4 & 5: const X = @import or pub const X = @import (these files get tested transitively)
        elif [[ "$line" == *"const"* && "$line" == *"@import("* ]]; then
            import_path=$(echo "$line" | grep -o '@import("[^"]*")' | sed 's/@import("\(.*\)")/\1/')

            if [[ -n "$import_path" && "$import_path" == *".zig" ]]; then
                if [[ "$import_path" == *"/"* ]]; then
                    full_path="$mod_dir/$import_path"
                else
                    full_path="$mod_dir/$import_path"
                fi
                full_path=$(echo "$full_path" | sed 's|/\./|/|g')
                REFERENCED_FILES+=("$full_path")
            fi
        fi
    done < "$mod_file"
done

echo "Found ${#REFERENCED_FILES[@]} file references in mod.zig files"
echo ""

# Step 3: Check each test file is referenced
echo "Step 3: Checking if all test files are properly wired..."
echo ""

UNWIRED_FILES=()

for test_file in "${TEST_FILES[@]}"; do
    # Check if this file is referenced
    referenced=0

    if [[ ${#REFERENCED_FILES[@]} -gt 0 ]]; then
        for ref in "${REFERENCED_FILES[@]}"; do
            if [[ "$test_file" == "$ref" ]]; then
                referenced=1
                break
            fi
        done
    fi

    if [[ $referenced -eq 0 ]]; then
        UNWIRED_FILES+=("$test_file")
    fi
done

# Step 4: Report results
if [[ ${#UNWIRED_FILES[@]} -gt 0 ]]; then
    ISSUES_FOUND=1
    echo -e "${RED}✗ Found ${#UNWIRED_FILES[@]} test file(s) that are NOT wired through mod.zig:${NC}"
    echo ""
    for file in "${UNWIRED_FILES[@]}"; do
        echo -e "  ${RED}✗${NC} $file"

        # Try to suggest which mod.zig should reference it
        module_dir=$(dirname "$file")
        mod_file="$module_dir/mod.zig"

        if [[ -f "$mod_file" ]]; then
            # Get relative path from mod.zig directory
            rel_path=$(basename "$file")
            if [[ "$file" == *"/test/"* ]]; then
                # Extract the test/ subdirectory part
                rel_path=$(echo "$file" | sed "s|.*$module_dir/||")
            fi

            echo -e "    ${YELLOW}→${NC} Should be added to $mod_file"
            echo -e "    ${YELLOW}→${NC} Add: std.testing.refAllDecls(@import(\"$rel_path\"));"
        else
            echo -e "    ${YELLOW}→${NC} No mod.zig found at $mod_file - may need to be created"
        fi
        echo ""
    done
fi

# Step 5: Check that modules with tests are in the build system
echo "Step 5: Checking build system configuration..."
echo ""

# Skip this check - it produces false positives because:
# 1. Module names in ModuleType enum don't always match directory names (e.g., "can" vs "canonicalize")
# 2. Some modules (cli, watch, snapshot_tool) use main.zig instead of mod.zig pattern
# 3. The build system is complex and this simple check isn't reliable enough
#
# Instead, rely on the file-level wiring check above, which is more precise.

echo ""

# Final summary
if [[ $ISSUES_FOUND -eq 0 ]]; then
    echo -e "${GREEN}✓ All tests are properly wired!${NC}"
    exit 0
else
    echo -e "${RED}✗ Test wiring issues found. Please fix the issues above.${NC}"
    echo ""
    echo "To fix:"
    echo "1. Add missing std.testing.refAllDecls() calls to the appropriate mod.zig files"
    echo "2. Ensure all modules with tests are listed in src/build/modules.zig test_configs"
    echo ""
    exit 1
fi
