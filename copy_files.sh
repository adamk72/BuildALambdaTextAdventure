#!/bin/zsh

# Check if minimum number of arguments is provided
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <file_extension> <directory1> [directory2 ...]"
    echo "Example: $0 .hs /path/to/dir1 /path/to/dir2"
    exit 1
fi

# Store file extension and remove it from arguments array
extension=$1
shift

# Clean up: Remove tmp directory if it exists and create a fresh one
if [[ -d "tmp" ]]; then
    echo "Cleaning up existing tmp directory..."
    rm -rf tmp
fi
mkdir -p tmp

# Initialize counters
total_files=0
processed_dirs=0
failed_dirs=0

# Process each directory argument
for dir in "$@"; do
    if [[ ! -d "$dir" ]]; then
        echo "Warning: Directory '$dir' does not exist - skipping"
        ((failed_dirs++))
        continue
    fi

    # Find and copy files from current directory
    files_found=$(find "$dir" -type f -name "*$extension" -exec cp {} tmp/ \; -print | wc -l)

    if [[ $files_found -gt 0 ]]; then
        echo "Copied $files_found file(s) from '$dir'"
        ((total_files += files_found))
        ((processed_dirs++))
    else
        echo "No files with extension '$extension' found in '$dir'"
        ((processed_dirs++))
    fi
done

# Print summary
echo "\nSummary:"
echo "- Processed $processed_dirs directories"
echo "- Skipped $failed_dirs invalid directories"
echo "- Total files copied: $total_files"

# Exit with error if no files were copied
if [[ $total_files -eq 0 ]]; then
    echo "\nNo files were copied. Please check your file extension and directories."
    exit 1
fi