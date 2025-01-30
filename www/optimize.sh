#!/usr/bin/env sh

set -eu

function hash_file {
    # the file to rename
    filename="$1"

    # generate SHA256 hash of the file content
    hash=$(sha256sum "${filename}" | head -c 8)

    # get the filename without the extension and the extension
    base="${filename%.*}"
    ext="${filename##*.}"

    # build new filename
    echo "${base}-${hash}.${ext}"
}

content=$(cat content/*.md)

pushd dist/fonts
for ttf_file in *.ttf; do
	# Subset the font into a new file, then replace the original
	hb-subset "${ttf_file}" "${content}" --output-file "${ttf_file}.subset"
	mv "${ttf_file}.subset" "${ttf_file}"

	# Compress into woff2
	woff2_compress "${ttf_file}"

	# Rename ttf with hash
	hashed_ttf=$(hash_file "${ttf_file}")
	mv "${ttf_file}" "${hashed_ttf}"

	# Rename woff2 with hash
	woff2_file="${ttf_file%.*}.woff2"
	hashed_woff2=$(hash_file "${woff2_file}")
	mv "${woff2_file}" "${hashed_woff2}"

	# Replace urls in the CSS
	sed -i -e "s:/${ttf_file}:/${hashed_ttf}:g" -e "s:/${woff2_file}:/${hashed_woff2}:g" ../wip/site.css
done
popd

# Minify the CSS file, and let esbuild add a content hash to the file name
npm exec --yes esbuild -- --minify dist/site.css --outdir=dist/ --entry-names='[name]-[hash]'
# Remove unused original file
rm dist/site.css

# Find the new filename
css_with_hash=$(basename dist/site-*.css)
# Replace all occurrences in the html
sed -i "s:/wip/site.css:/wip/${css_with_hash}:g" dist/*.html
