# Documentation

## Essential Files
- **[GUIDE.md](./GUIDE.md)** - Complete development and deployment guide
- **[ARCHITECTURE.md](./ARCHITECTURE.md)** - System design and domain architecture
- **[SECURITY.md](./SECURITY.md)** - Security configuration and GDPR compliance
- **[UI.md](./UI.md)** - Component guidelines and design system
- **[RULES.md](./RULES.md)** - Framework usage patterns (Ash, Phoenix, etc.)

## Core Principles (high-level)
- Atomic Design components with Storybook focused on components only (no pages)
- Database-level filtering in Ash; avoid in-memory filtering
- Data-flow pipelines and pattern matching; single level of abstraction
- Property-based tests where it makes sense; browser tests via mix when possible
- Responsive-first web app; no native app planned





That's it. Everything you need is in these 5 files.