#!/bin/bash

# Create the dist directory
mkdir -p dist

# Create dist/index.html with Hello World content
cat > dist/index.html << 'EOF'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Hello World</title>
</head>
<body>
    <h1>Hello World!</h1>
</body>
</html>
EOF

echo "Created dist directory and index.html with Hello World content"
