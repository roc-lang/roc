#!/usr/bin/env python3

import os
import re
from pathlib import Path

def convert_snapshot_file(file_path):
    """Convert a single snapshot file from old format to new format"""

    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Split into lines
    lines = content.split('\n')
    result_lines = []
    i = 0

    while i < len(lines):
        line = lines[i]

        # Stop at PROBLEMS section or merge conflict markers
        if (line.strip().startswith('~~~PROBLEMS') or
            line.strip() == '# PROBLEMS' or
            line.strip().startswith('<<<<<<< ') or
            line.strip().startswith('=======') or
            line.strip().startswith('>>>>>>> ')):
            break

        # Convert old format section headers to new format
        if line.strip() == '~~~META':
            result_lines.append('# META')
            result_lines.append('~~~ini')
            i += 1
            # Copy content until next section
            while i < len(lines):
                next_line = lines[i]
                if next_line.strip().startswith('~~~') and next_line.strip() != '~~~END':
                    break
                if next_line.strip() == '~~~END':
                    i += 1
                    break
                result_lines.append(next_line)
                i += 1
            result_lines.append('~~~')
            continue

        elif line.strip() == '~~~SOURCE':
            result_lines.append('# SOURCE')
            result_lines.append('~~~roc')
            i += 1
            # Copy content until next section
            while i < len(lines):
                next_line = lines[i]
                if next_line.strip().startswith('~~~') and next_line.strip() != '~~~END':
                    break
                if next_line.strip() == '~~~END':
                    i += 1
                    break
                result_lines.append(next_line)
                i += 1
            result_lines.append('~~~')
            continue

        # Skip ~~~END lines
        elif line.strip() == '~~~END':
            i += 1
            continue

        # For files already in new format, just copy lines
        else:
            result_lines.append(line)
            i += 1

    # Join and clean up
    result = '\n'.join(result_lines).rstrip() + '\n'
    return result

def main():
    # Find the snapshots directory
    script_dir = Path(__file__).parent
    snapshots_dir = script_dir / 'src' / 'snapshots'

    if not snapshots_dir.exists():
        print(f"Snapshots directory not found: {snapshots_dir}")
        return

    # Find all .txt and .md files recursively
    txt_files = list(snapshots_dir.rglob('*.txt'))
    md_files = list(snapshots_dir.rglob('*.md'))

    print(f"Found {len(txt_files)} .txt files and {len(md_files)} .md files")

    converted_count = 0

    # Convert .txt files to .md
    for txt_file in txt_files:
        print(f"Converting {txt_file.relative_to(snapshots_dir)}...")

        try:
            converted_content = convert_snapshot_file(txt_file)

            # Create new .md file
            md_file = txt_file.with_suffix('.md')
            with open(md_file, 'w', encoding='utf-8') as f:
                f.write(converted_content)

            # Remove old .txt file
            txt_file.unlink()

            converted_count += 1
            print(f"  ✓ Converted to {md_file.name}")

        except Exception as e:
            print(f"  ✗ Error converting {txt_file.name}: {e}")

    # Fix existing .md files that might have issues
    for md_file in md_files:
        try:
            with open(md_file, 'r', encoding='utf-8') as f:
                original_content = f.read()

            # Check if it needs fixing
            needs_fixing = ('~~~META' in original_content or
                          '~~~SOURCE' in original_content or
                          '<<<<<<< ' in original_content or
                          '=======' in original_content or
                          '>>>>>>> ' in original_content or
                          '# PROBLEMS' in original_content)

            if needs_fixing:
                print(f"Fixing {md_file.relative_to(snapshots_dir)}...")

                converted_content = convert_snapshot_file(md_file)

                # Only write if content changed
                if converted_content != original_content:
                    with open(md_file, 'w', encoding='utf-8') as f:
                        f.write(converted_content)

                    converted_count += 1
                    print(f"  ✓ Fixed {md_file.name}")

        except Exception as e:
            print(f"  ✗ Error fixing {md_file.name}: {e}")

    print(f"\nConversion complete! {converted_count} files processed.")

if __name__ == '__main__':
    main()
