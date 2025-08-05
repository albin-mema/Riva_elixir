# Riva Ash Documentation

This directory contains comprehensive documentation for the Riva Ash business management system.

## 📚 Documentation Index

### Getting Started
- **[../README.md](../README.md)** - Project overview and quick start guide
- **[SETUP_GUIDE.md](SETUP_GUIDE.md)** - Complete environment setup with troubleshooting
- **[DEVELOPMENT_SETUP.md](DEVELOPMENT_SETUP.md)** - Alternative development setup guide

### Development
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Comprehensive contribution guidelines
- **[DEVELOPMENT_WORKFLOW.md](DEVELOPMENT_WORKFLOW.md)** - Development workflow and best practices
- **[../packages/riva_ash/patterns.md](../packages/riva_ash/patterns.md)** - Architectural patterns and design guidelines

### Project Analysis & Planning
- **[RESERVATION_SYSTEM_ASSESSMENT.md](RESERVATION_SYSTEM_ASSESSMENT.md)** - Reservation system analysis
- **[USER_ACTIONS.md](USER_ACTIONS.md)** - User action specifications
- **[todo-ui.md](todo-ui.md)** - UI development roadmap

### Technical Documentation
- **[TIMEX_INTEGRATION.md](TIMEX_INTEGRATION.md)** - Time handling integration guide
- **[../packages/riva_ash/docs/testing_guide.md](../packages/riva_ash/docs/testing_guide.md)** - Testing strategies and guidelines

### Utilities
- **[verify-docs.sh](verify-docs.sh)** - Documentation verification script

## 🏗️ Project Structure

```
Riva_Ash/
├── README.md                           # Project overview & quick start
├── documentation/                      # All documentation files
│   ├── README.md                      # This index file
│   ├── CONTRIBUTING.md                # Contribution guidelines
│   ├── SETUP_GUIDE.md                # Environment setup
│   ├── DEVELOPMENT_WORKFLOW.md       # Development process
│   └── ...                           # Other documentation
└── packages/riva_ash/
    ├── patterns.md                    # Architectural patterns
    └── docs/                          # Project-specific docs
        └── testing_guide.md           # Testing documentation
```

## 🚀 Quick Navigation

**New Contributors**: Start with [CONTRIBUTING.md](CONTRIBUTING.md)  
**Environment Setup**: See [SETUP_GUIDE.md](SETUP_GUIDE.md)  
**Architecture**: Review [../packages/riva_ash/patterns.md](../packages/riva_ash/patterns.md)  
**Development Process**: Follow [DEVELOPMENT_WORKFLOW.md](DEVELOPMENT_WORKFLOW.md)  

## 📝 Documentation Standards

All documentation in this project follows these standards:

- **Markdown Format**: All documentation uses Markdown for consistency
- **Clear Structure**: Each document has a clear table of contents and sections
- **Cross-References**: Documents link to related documentation
- **Examples**: Practical examples and code snippets are included
- **Up-to-Date**: Documentation is kept current with code changes

## 🔧 Maintenance

To verify documentation integrity, run:

```bash
# From project root
./verify-docs.sh

# Or from documentation directory
./verify-docs.sh
```

This script checks:
- All documentation files exist
- Cross-references are valid
- Project structure is correct
- No redundant files remain

---

**Need help?** Check the [CONTRIBUTING.md](CONTRIBUTING.md) guide or open an issue in the repository.
