#!/bin/bash

# Documentation Verification Wrapper
# This script runs the documentation verification from the root directory

echo "🔍 Verifying documentation structure..."
echo ""

# Change to documentation directory and run the verification script
cd documentation && ./verify-docs.sh

# Return to root directory
cd ..

echo ""
echo "📁 Current documentation structure:"
echo "├── README.md (root)"
echo "└── documentation/"
echo "    ├── CONTRIBUTING.md"
echo "    ├── SETUP_GUIDE.md"
echo "    ├── DEVELOPMENT_WORKFLOW.md"
echo "    ├── DEVELOPMENT_SETUP.md"
echo "    ├── verify-docs.sh"
echo "    └── ..."
echo ""
echo "✨ Documentation is organized with README.md in root and all other docs in documentation/ folder!"
