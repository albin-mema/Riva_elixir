#!/bin/bash

# Verify Documentation Links Script
# This script checks that all documentation files exist and are properly linked

echo "🔍 Verifying documentation structure and links..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track issues
ISSUES=0

# Function to check if file exists
check_file() {
    local file="$1"
    local description="$2"
    
    if [ -f "$file" ]; then
        echo -e "${GREEN}✓${NC} $description: $file"
    else
        echo -e "${RED}✗${NC} $description: $file (MISSING)"
        ((ISSUES++))
    fi
}

# Function to check if directory exists
check_directory() {
    local dir="$1"
    local description="$2"
    
    if [ -d "$dir" ]; then
        echo -e "${GREEN}✓${NC} $description: $dir"
    else
        echo -e "${RED}✗${NC} $description: $dir (MISSING)"
        ((ISSUES++))
    fi
}

echo ""
echo "📋 Checking main documentation files..."

# Main documentation files
check_file "../README.md" "Main README"
check_file "README.md" "Documentation index"
check_file "CONTRIBUTING.md" "Contributing guide"
check_file "SETUP_GUIDE.md" "Setup guide"
check_file "DEVELOPMENT_WORKFLOW.md" "Development workflow"
check_file "DEVELOPMENT_SETUP.md" "Development setup"

echo ""
echo "📋 Checking project-specific documentation..."

# Project-specific documentation
check_file "../packages/riva_ash/patterns.md" "Architectural patterns"
check_file "../packages/riva_ash/docs/testing_guide.md" "Testing guide"
check_directory "../packages/riva_ash/docs" "Documentation directory"

echo ""
echo "📋 Checking configuration files..."

# Configuration files
check_file "../package.json" "Package configuration"
check_file "../pnpm-workspace.yaml" "PNPM workspace config"
check_file "../docker-compose.yml" "Docker compose config"
check_file "../docker-dev.sh" "Docker development script"

echo ""
echo "📋 Checking project structure..."

# Key directories
check_directory "../packages" "Packages directory"
check_directory "../packages/riva_ash" "Main application directory"
check_directory "../packages/riva_ash/lib" "Application source"
check_directory "../packages/riva_ash/test" "Test directory"

echo ""
echo "📋 Checking for removed files..."

# Check that the redundant file was removed
if [ -f "../packages/riva_ash/contribution_section.md" ]; then
    echo -e "${RED}✗${NC} Redundant file still exists: packages/riva_ash/contribution_section.md"
    ((ISSUES++))
else
    echo -e "${GREEN}✓${NC} Redundant file properly removed: packages/riva_ash/contribution_section.md"
fi

echo ""
echo "📋 Checking documentation cross-references..."

# Check if README references CONTRIBUTING.md
if grep -q "documentation/CONTRIBUTING.md" ../README.md; then
    echo -e "${GREEN}✓${NC} README.md references CONTRIBUTING.md"
else
    echo -e "${YELLOW}⚠${NC} README.md should reference documentation/CONTRIBUTING.md"
    ((ISSUES++))
fi

# Check if patterns.md references main documentation
if grep -q "README.md" ../packages/riva_ash/patterns.md; then
    echo -e "${GREEN}✓${NC} patterns.md references main documentation"
else
    echo -e "${YELLOW}⚠${NC} patterns.md should reference main documentation"
fi

echo ""
echo "📋 Summary"

if [ $ISSUES -eq 0 ]; then
    echo -e "${GREEN}🎉 All documentation checks passed!${NC}"
    echo "Documentation structure is properly organized and all files are in place."
else
    echo -e "${RED}❌ Found $ISSUES issue(s) with documentation structure.${NC}"
    echo "Please review and fix the issues listed above."
    exit 1
fi

echo ""
echo "📖 Documentation Overview:"
echo "  • ../README.md - Project overview and quick start"
echo "  • documentation/README.md - Documentation index"
echo "  • documentation/CONTRIBUTING.md - Comprehensive contribution guide"
echo "  • documentation/SETUP_GUIDE.md - Detailed environment setup"
echo "  • documentation/DEVELOPMENT_WORKFLOW.md - Development process"
echo "  • ../packages/riva_ash/patterns.md - Architectural patterns"
echo "  • ../packages/riva_ash/docs/ - Project-specific documentation"
echo ""
echo "✨ Documentation is now properly organized and cross-referenced!"
