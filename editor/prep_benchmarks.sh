#!/bin/bash
sed -i -e 's/\/\/pub mod mvc/pub mod mvc/g' src/lib.rs
sed -i -e 's/\/\/pub mod text_buffer/pub mod text_buffer/g' src/lib.rs
sed -i -e 's/^mod mvc/\/\/mod mvc/g' src/lib.rs
sed -i -e 's/^mod text_buffer/\/\/mod text_buffer/g' src/lib.rs